#' @title Generate a `TileDBArray2` Object
#'
#' @description
#' This class inherits from [TileDBArray] and offers additional methods to
#' operate on the array.
#'
#' **TODO** add section with methods: enum, consol/vac, fragment
#'
#' ## Initialization
#' A new `TileDBArray2` instance is initialized using the `new()` method.
#' Alternatively use [tdb_array()] to create an instance and open the array at
#' `READ` mode.
#'
#' ```r
#'  # uri path
#'  uri <- tempdir()
#'  # new instance
#'  obj <- TileDBArray2$new(uri = uri)
#'  # does array exist at this uri
#'  obj$exists() # FALSE
#'
#'  unlink(uri)
#' ```
#' @returns An object of class `TileDBArray2`.
#'
#' @export
TileDBArray2 <- R6::R6Class(
  classname = "TileDBArray2",
  inherit = TileDBArray,
  public = list(
    #' @description Create a new `TileDBArray2` instance.
    #'
    #' @param uri URI path for the `TileDB` Array.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object.
    #' @param tiledb_timestamp Optional Datetime (POSIXct) with TileDB timestamp.
    #'
    initialize = function(uri,
                          ctx = NULL,
                          tiledb_timestamp = NULL) {

      super$initialize(uri = uri,
                       ctx = ctx,
                       tiledb_timestamp = tiledb_timestamp,
                       internal_use = "permit")

    },
    #' @description Close and reopen the TileDB object in a new mode.
    #'
    #' @param mode New mode to open the object in; choose from: `"READ"` or `"WRITE"`.
    #'
    #' @param tiledb_timestamp Optional Datetime (POSIXct) with TileDB timestamp.
    #'
    #' @return The object, invisibly.
    #'
    reopen = function(mode = c('READ', 'WRITE'), tiledb_timestamp = NULL) {

      # reset fragment object
      private$.fragments_object <- NULL

      super$reopen(mode, tiledb_timestamp = tiledb_timestamp)
    },
    #' @description Retrieve factor columns (attributes).
    #'
    #' @return A character vector with factor columns (enum attributes).
    #'
    enum_columns = function() {

      idx <- vapply_lgl(self$attributes(), tiledb::tiledb_attribute_has_enumeration)
      names(idx[idx])

    },
    #' @description Retrieve factor levels for a given attribute.
    #'
    #' @param x An attribute name.
    #'
    #' @return A character vector with levels (enum values).
    #'
    enum_levels = function(x) {

      private$check_scalar_character(x)

      if (isFALSE(x %in% self$attrnames())) {
        cli::cli_abort("{.emph '{deparse(substitute(x))}'} is not attribute.", call = NULL)
      }

      .attrib <- self$attributes()[[x]]

      if (isFALSE(tiledb::tiledb_attribute_has_enumeration(.attrib))) {
        cli::cli_abort("{.emph '{deparse(substitute(x))}'} attribute is not factor (enum).", call = NULL)
      }

      enum <- tiledb::tiledb_attribute_get_enumeration(attr = .attrib, arr = self$object)

      enum

    },
    #' @description Checks array for factor columns.
    #'
    #'
    #' @return A boolean. `TRUE` indicating the array has factor columns and
    #' `FALSE` otherwise.
    #'
    has_enums = function(){
      idx <- vapply_lgl(self$attributes(), tiledb::tiledb_attribute_has_enumeration)
      any(idx)
    },
    # TODO: add key https://github.com/TileDB-Inc/TileDB-Py/blob/28714d9b25d44d6c6c1f318525184d3784b7de00/tiledb/array.py#L729
    # TODO: ctx? add example..
    # TODO: ADD LIST OF CONFIGUREATION PARAMETERS
    #' @description Consolidates the fragments of the array into
    #'  a single fragment.
    #'
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the consolidation. When `NULL` (default) the configuration parameters will
    #'  be retrieve from object context.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXlt`. If not provided, the default values from configuration
    #' object will be used.
    #' @param mode The consolidate mode, one of the following:
    #'
    #'  - `"fragments"`: - consolidate all fragments (default)
    #'  - `"commits"`: - consolidate all commit files
    #'  - `"fragment_meta"`: - consolidate only fragment metadata footers to a single file
    #'  - `"array_meta"`: - consolidate array metadata only
    #'
    #' @return `TRUE`, invisibly.
    #'
    consolidate = function(cfg = NULL,
                           start_time = NULL,
                           end_time = NULL,
                           mode = c("fragments",
                                    "commits",
                                    "fragment_meta",
                                    "array_meta")) {

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      if (!inherits(cfg, "tiledb_config")) {
        cli::cli_abort("{.emph '{deparse(substitute(cfg))}'} should be of class {.cls tiledb_config}.", call = NULL)
      }

      cfg["sm.consolidation.mode"] <- mode

      if (!is.null(start_time)) {
        if (!inherits(start_time, "POSIXt")) {
          cli::cli_abort("{.emph '{deparse(substitute(start_time))}'} should be of class {.cls POSIXt}.", call = NULL)
        }
        start_time_int64 <- as.character(bit64::as.integer64(as.numeric(start_time) * 1000))
        cfg["sm.consolidation.timestamp_start"] <- start_time_int64
      }

      if (!is.null(end_time)) {

        if (!inherits(end_time, "POSIXt")) {
          cli::cli_abort("{.emph '{deparse(substitute(end_time))}'} should be of class {.cls POSIXt}.", call = NULL)
        }
        end_time_int64 <- as.character(bit64::as.integer64(as.numeric(end_time) * 1000))
        cfg["sm.consolidation.timestamp_end"] <- end_time_int64
      }

      tiledb::array_consolidate(self$uri, cfg = cfg)

      # reset fragment object
      private$.fragments_object <- NULL

      invisible(TRUE)
    },
    #' @description Consolidate fragments of the array into a single fragment
    #'  asynchronously.
    #'
    #' The consolidation will run in a separate R process in a clean environment.
    #'
    #' **Note this function requires the [mirai](https://cran.r-project.org/web/packages/mirai/index.html) package**.
    #'
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the consolidation. When `NULL` (default) the configuration parameters will
    #'  be retrieve from object context.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXlt`. If not provided, the default values from configuration
    #' object will be used.
    #' @param mode The consolidate mode, one of the following:
    #'
    #'  - `"fragments"`: - consolidate all fragments (default)
    #'  - `"commits"`: - consolidate all commit files
    #'  - `"fragment_meta"`: - consolidate only fragment metadata footers to a single file
    #'  - `"array_meta"`: - consolidate array metadata only
    #'
    #' @return This function will return a [mirai::mirai()] object immediately. When it is
    #' resolved, it returns `TRUE` indicating consolidation success.
    #'
    consolidate_async = function(cfg = NULL,
                                 start_time = NULL,
                                 end_time = NULL,
                                 mode = c("fragments",
                                          "commits",
                                          "fragment_meta",
                                          "array_meta")) {

      if (!requireNamespace("mirai", quietly = TRUE)) {
        cli::cli_abort("{.emph 'consolicate_async'} requires {.pkg '{.href [mirai](https://cran.r-project.org/web/packages/mirai/index.html)}'} package.", call = NULL)
      }

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      cfg["sm.consolidation.mode"] <- mode

      if (!is.null(start_time)) {
        if (!inherits(start_time, "POSIXt")) {
          cli::cli_abort("{.emph '{deparse(substitute(start_time))}'} should be of class {.cls POSIXt}.", call = NULL)
        }
        start_time_int64 <- as.character(bit64::as.integer64(as.numeric(start_time) * 1000))
        cfg["sm.consolidation.timestamp_start"] <- start_time_int64
        }

      if (!is.null(end_time)) {

        if (!inherits(end_time, "POSIXt")) {
          cli::cli_abort("{.emph '{deparse(substitute(end_time))}'} should be of class {.cls POSIXt}.", call = NULL)
        }
        end_time_int64 <- as.character(bit64::as.integer64(as.numeric(end_time) * 1000))
        cfg["sm.consolidation.timestamp_end"] <- end_time_int64
      }


      # mirai namespace compute profile
      ns <- "r6.tiledb"

      # Start non-dispatcher background process if not already started
      if (is.null(mirai::nextget("n", .compute = ns))) {
       mirai::daemons(1L, dispatcher = FALSE, autoexit = tools::SIGINT, .compute = ns)
      }

      m <- mirai::mirai({

        # * Note: We cannot serialise external pointers without custom serialisers,
        #   so we setup config / context in the daemon and pass the config parameters
        #   from R session
        cfg <- tiledb::tiledb_config(config_params)

        ctx <- tiledb::tiledb_ctx(cfg)

        tiledb::array_consolidate(uri = uri, ctx = ctx)

        return(TRUE)

        }, uri = self$uri, config_params = as.vector(cfg), .compute = ns)

      # reset fragment object
      private$.fragments_object <- NULL

      m
    },
    #' @description Clean up consolidated fragments and array metadata.
    #'
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the vacuum. When `NULL` (default) the configuration parameters
    #'  will be retrieve from object context.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXlt`. If not provided, the default values from configuration
    #' object will be used.
    #' @param mode The vacuum mode, one of the following:
    #'
    #'  - `"fragments"`: - vacuum all fragments (default)
    #'  - `"commits"`: - vacuum all commit files
    #'  - `"fragment_meta"`: - vacuum only fragment metadata footers to a single file
    #'  - `"array_meta"`: - vacuum array metadata only
    #'
    #' @return `TRUE`, invisibly.
    #'
    vacuum = function(cfg = NULL,
                      start_time = NULL,
                      end_time = NULL,
                      mode = c("fragments",
                               "commits",
                               "fragment_meta",
                               "array_meta")){

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      if (!inherits(cfg, "tiledb_config")) {
        cli::cli_abort("{.emph '{deparse(substitute(cfg))}'} should be of class {.cls tiledb_config}.", call = NULL)
      }

      cfg["sm.vacuum.mode"] <- mode

      if (!is.null(start_time)) {
        if (!inherits(start_time, "POSIXt")) {
          cli::cli_abort("{.emph '{deparse(substitute(start_time))}'} should be of class {.cls POSIXt}.", call = NULL)
        }
        start_time_int64 <- as.character(bit64::as.integer64(as.numeric(start_time) * 1000))
        cfg["sm.vacuum.timestamp_start"] <- start_time_int64
      }

      if (!is.null(end_time)) {

        if (!inherits(end_time, "POSIXt")) {
          cli::cli_abort("{.emph '{deparse(substitute(end_time))}'} should be of class {.cls POSIXt}.", call = NULL)
        }
        end_time_int64 <- as.character(bit64::as.integer64(as.numeric(end_time) * 1000))
        cfg["sm.vacuum.timestamp_end"] <- end_time_int64
      }

      tiledb::array_vacuum(self$uri, cfg = cfg)

      # reset fragment object
      private$.fragments_object <- NULL

      invisible(TRUE)

    },
    #' @description Asynchronously clean up consolidated fragments and array metadata.
    #'
    #' The clean up will run in a separate R process in a clean environment.
    #'
    #' **Note this function requires the [mirai](https://cran.r-project.org/web/packages/mirai/index.html) package**.
    #'
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the vacuum process. When `NULL` (default) the configuration parameters will
    #'  be retrieve from object context.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXlt`. If not provided, the default values from configuration
    #' object will be used.
    #' @param mode The vacuum mode, one of the following:
    #'
    #'  - `"fragments"`: - vacuum all fragments (default)
    #'  - `"commits"`: - vacuum all commit files
    #'  - `"fragment_meta"`: - vacuum only fragment metadata footers to a single file
    #'  - `"array_meta"`: - vacuum array metadata only
    #'
    #' @return This function will return a [mirai::mirai()] object immediately. When it is
    #' resolved, it returns `TRUE` indicating vacuum success.
    #'
    vacuum_async = function(cfg = NULL,
                            start_time = NULL,
                            end_time = NULL,
                            mode = c("fragments",
                                     "commits",
                                     "fragment_meta",
                                     "array_meta")) {

      if (!requireNamespace("mirai", quietly = TRUE)) {
        cli::cli_abort("{.emph 'vacuum_async'} requires {.pkg '{.href [mirai](https://cran.r-project.org/web/packages/mirai/index.html)}'} package.", call = NULL)
      }

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      cfg["sm.vacuum.mode"] <- mode

      if (!is.null(start_time)) {
        if (!inherits(start_time, "POSIXt")) {
          cli::cli_abort("{.emph '{deparse(substitute(start_time))}'} should be of class {.cls POSIXt}.", call = NULL)
        }
        start_time_int64 <- as.character(bit64::as.integer64(as.numeric(start_time) * 1000))
        cfg["sm.vacuum.timestamp_start"] <- start_time_int64
      }

      if (!is.null(end_time)) {

        if (!inherits(end_time, "POSIXt")) {
          cli::cli_abort("{.emph '{deparse(substitute(end_time))}'} should be of class {.cls POSIXt}.", call = NULL)
        }
        end_time_int64 <- as.character(bit64::as.integer64(as.numeric(end_time) * 1000))
        cfg["sm.vacuum.timestamp_end"] <- end_time_int64
      }

      # mirai namespace compute profile
      ns <- "r6.tiledb"

      # Start non-dispatcher background process if not already started
      if (is.null(mirai::nextget("n", .compute = ns))) {
        mirai::daemons(1L, dispatcher = FALSE, autoexit = tools::SIGINT, .compute = ns)
      }

      m <- mirai::mirai({

        # * Note: We cannot serialise external pointers without custom serialisers,
        #   so we setup config / context in the daemon and pass the config parameters
        #   from R session
        cfg <- tiledb::tiledb_config(config_params)

        ctx <- tiledb::tiledb_ctx(cfg)

        tiledb::array_vacuum(uri = uri, ctx = ctx)

        return(TRUE)

      }, uri = self$uri, config_params = as.vector(cfg), .compute = ns)

      # reset fragment object
      private$.fragments_object <- NULL

      m
    },
    #' @description Remove an attribute from array.
    #'
    #' @param x An attribute name.
    #'
    #' @return The object, invisibly.
    #'
    drop_attribute = function(x) {

      private$check_scalar_character(x)

      if (isFALSE(x %in% self$attrnames())) {
        cli::cli_abort("{.emph '{deparse(substitute(x))}'} is not attribute.", call = NULL)
      }

      ase <- tiledb::tiledb_array_schema_evolution()
      ase <- tiledb::tiledb_array_schema_evolution_drop_attribute(ase, x)
      ase <- tiledb::tiledb_array_schema_evolution_array_evolve(ase, self$uri)

      invisible(self)
    },

    # rework: populate data
    # add_attribute = function(attr, enums = NULL) {
    #
    #   # STOP IF NOT attribute
    #
    #   ase <- tiledb::tiledb_array_schema_evolution()
    #
    #   if (!is.null(enums)) {
    #     ase <- tiledb::tiledb_array_schema_evolution_add_enumeration(ase, attr_name, enums)
    #     attr <- tiledb::tiledb_attribute_set_enumeration_name(attr, attr_name)
    #
    #   }
    #
    #   ase <- tiledb::tiledb_array_schema_evolution_add_attribute(ase, attr)
    #
    #   tiledb::tiledb_array_schema_evolution_array_evolve(ase, self$uri)
    # }

    #' @description The number of fragments.
    #'
    frag_num = function() {
      self$fragments_object$frag_num()
    },
    #' @description Consolidated fragments to be removed.
    #'
    #' @param trunc_uri `TRUE` to truncate uri path.
    #'
    #' @return An object of class `data.frame` with four columns:
    #'
    #'  - `Fragment`: the fragment index (starts at 1)
    #'  - `start_timestamp`: fragment's start time stamp
    #'  - `end_timestamp`: fragment's end time stamp
    #'  - `URI`: fragment's truncated uri path (fragment name) when
    #'  `trunc_uri = TRUE` (default), otherwise the full uri path
    #'
    #'  Note that return object will be of class `data.table` if the
    #'  package is found in your system.
    #'
    frag_to_vacuum = function(trunc_uri = TRUE) {

      self$fragments_object$to_vacuum(trunc_uri)
    },
    #' @description Dump to console the commit fragments.
    #'
    frag_dump = function() {
      self$fragments_object$dump()
    },
    #' @description Return a `data.frame` with  time stamps and
    #' fragments uris.
    #'
    #' @param trunc_uri `TRUE` to truncate uri path.
    #'
    #' @return An object of class `data.frame` with four columns:
    #'
    #'  - `Fragment`: the fragment index (start at 1)
    #'  - `start_timestamp`: start time-stamp of when fragment was written
    #'  - `end_timestamp`: end time-stamp of when fragment was written
    #'  - `URI`: fragment's truncated uri path (fragment name) when
    #'  `trunc_uri = TRUE` (default), otherwise the full uri path
    #'
    #'  Note that return object will be of class `data.table` if the
    #'  package is found in your system.
    #'
    frag_uris = function(trunc_uri = TRUE){

      self$fragments_object$frag_uris(trunc_uri)

    },
    #' @description Upgrade the array to the latest format version.
    #'
    #' @param cfg A configuration object [tiledb::tiledb_config()].
    #' @param ctx Optional [tiledb::tiledb_ctx()] object. By default, object's context
    #'  is used.
    #'
    schema_upgrade = function(cfg = NULL, ctx = NULL) {
      if (is.null(ctx)) {
        ctx <- self$ctx
      }
      tiledb::tiledb_array_upgrade_version(self$object, config = cfg, ctx = self$ctx)
    }
  ),

  active = list(
    #' @field fragments_object Access the [TileDBFragments] instance for this
    #' array.
    #'
    fragments_object = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("fragment_object")
      }
      if (is.null(private$.fragments_object)) {
        private$.fragments_object <- TileDBFragments$new(self$uri, ctx = self$ctx)
      }
      private$.fragments_object

    },
    #' @field schema_version Retrieve the schema version for this array.
    #'
    schema_version = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("schema_version")
      }
      tiledb::tiledb_array_schema_version(self$schema())
    }
  ),

  private = list(

    # Contains TileDBFragments object
    .fragments_object = NULL
  )
)
