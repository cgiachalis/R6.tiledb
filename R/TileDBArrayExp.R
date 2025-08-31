#' @title Generate a `TileDBArrayExp` Object
#'
#' @description
#' An enhanced version of [TileDBArray] with additional methods to
#' operate on the array.
#'
#' ## Initialization
#' A new `TileDBArrayExp` instance is initialised using the `new()` method.
#' Alternatively use [tdb_array()] to create a new instance and open the array
#' at `READ` mode.
#'
#' ```r
#'  # uri path
#'  uri <- tempdir()
#'  # new instance
#'  obj <- TileDBArrayExp$new(uri = uri)
#'  # does array exist at this uri
#'  obj$exists() # FALSE
#'
#'  unlink(uri)
#' ```
#' @returns An object of class `TileDBArrayExp`, `R6`.
#'
#' @export
TileDBArrayExp <- R6::R6Class(
  classname = "TileDBArrayExp",
  inherit = TileDBArray,
  public = list(
    #' @description Close and reopen the TileDB object in a new mode.
    #'
    #' @param mode New mode to open the object in; choose from: `"READ"` or `"WRITE"`.
    #'
    #' @return The object, invisibly.
    #'
    reopen = function(mode = c('READ', 'WRITE')) {

      # reset fragment object
      private$.fragments_object <- NULL

      super$reopen(mode)
    },
    #' @description Checks array for factors (enumerations).
    #'
    #' @return A boolean. `TRUE` indicating the array has factors and
    #' `FALSE` otherwise.
    #'
    any_enums = function(){
      any(self$has_enumeration())
    },
    #' @description Retrieve factor columns (attributes).
    #'
    #' @return A character vector with factor columns (enumeration attributes).
    #'
    enum_columns = function() {

      idx <- tiledb::tiledb_array_has_enumeration(self$object)
      names(idx[idx])

    },
    #' @description Retrieve factor levels for a given attribute.
    #'
    #' @param x An attribute name.
    #'
    #' @return A character vector with levels (enumeration values).
    #'
    enum_levels = function(x) {

      private$check_scalar_character(x)

      if (isFALSE(x %in% self$attrnames())) {
        cli::cli_abort("{.arg {deparse(substitute(x))}} is not attribute.", call = NULL)
      }

      .attrib <- self$attributes()[[x]]

      if (isFALSE(tiledb::tiledb_attribute_has_enumeration(.attrib))) {
        cli::cli_abort("{.arg {deparse(substitute(x))}} attribute is not factor (enum).", call = NULL)
      }

      enum <- tiledb::tiledb_attribute_get_enumeration(attr = .attrib, arr = self$object)

      enum

    },
    #' @description Check columns for factors.
    #'
    #'
    #' @return A logical vector indicating which column (attribute) is encoded as factor (enumeration).
    #'
    has_enumeration = function(){
      tiledb::tiledb_array_has_enumeration(self$object)
    },

    # TODO: add key https://github.com/TileDB-Inc/TileDB-Py/blob/28714d9b25d44d6c6c1f318525184d3784b7de00/tiledb/array.py#L729
    # TODO: ctx? add example..
    # TODO: ADD LIST OF CONFIGUREATION PARAMETERS
    #' @description Consolidates the fragments of the array into
    #'  a single fragment.
    #'
    #' @param mode The consolidate mode, one of the following:
    #'
    #'  - `"fragments"`: - consolidate all fragments (default)
    #'  - `"commits"`: - consolidate all commit files
    #'  - `"fragment_meta"`: - consolidate only fragment metadata footers to a single file
    #'  - `"array_meta"`: - consolidate array metadata only
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the consolidation. When `NULL` (default) the configuration parameters will
    #'  be retrieved from object's context.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXt`. If not provided, the default values from configuration
    #' object will be used.
    #'
    #' @return `TRUE`, invisibly.
    #'
    consolidate = function(mode = c("fragments",
                                    "commits",
                                    "fragment_meta",
                                    "array_meta"),
                           cfg = NULL,
                           start_time = NULL,
                           end_time = NULL) {

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      check_tiledb_config(cfg)

      cfg["sm.consolidation.mode"] <- mode

      # set consolidation time stamps
      if (!is.null(start_time) || !is.null(end_time)) {
        cfg <- set_consolidation_tstamps(cfg, start_time, end_time)
      }

      .libtiledb_array_consolidate(ctx = self$ctx@ptr, uri = self$uri, cfgptr = cfg@ptr)

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
    #' @param mode The consolidate mode, one of the following:
    #'
    #'  - `"fragments"`: - consolidate all fragments (default)
    #'  - `"commits"`: - consolidate all commit files
    #'  - `"fragment_meta"`: - consolidate only fragment metadata footers to a single file
    #'  - `"array_meta"`: - consolidate array metadata only
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the consolidation. When `NULL` (default) the configuration parameters will
    #'  be retrieved from object's context.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXt`. If not provided, the default values from configuration
    #' object will be used.
    #'
    #' @return This function will return a [mirai::mirai()] object immediately. When it is
    #' resolved, it returns `TRUE` indicating consolidation success.
    #'
    consolidate_async = function(mode = c("fragments",
                                          "commits",
                                          "fragment_meta",
                                          "array_meta"),
                                 cfg = NULL,
                                 start_time = NULL,
                                 end_time = NULL) {

      if (!requireNamespace("mirai", quietly = TRUE)) {
        cli::cli_abort("{.arg consolicate_async} requires {.pkg '{.href [mirai](https://cran.r-project.org/web/packages/mirai/index.html)}'} package.", call = NULL)
      }

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      check_tiledb_config(cfg)

      cfg["sm.consolidation.mode"] <- mode

      # set consolidation time stamps
      if (!is.null(start_time) || !is.null(end_time)) {
        cfg <- set_consolidation_tstamps(cfg, start_time, end_time)
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

        ctx <- R6.tiledb::new_context(cfg)
        R6.tiledb:::.libtiledb_array_consolidate(ctx = ctx@ptr, uri = uri)

        return(TRUE)

        }, uri = self$uri, config_params = as.vector(cfg), .compute = ns)

      # reset fragment object
      private$.fragments_object <- NULL

      m
    },
    #' @description Clean up consolidated fragments and array metadata.
    #'
    #' @param mode The vacuum mode, one of the following:
    #'
    #'  - `"fragments"`: - vacuum all fragments (default)
    #'  - `"commits"`: - vacuum all commit files
    #'  - `"fragment_meta"`: - vacuum only fragment metadata footers to a single file
    #'  - `"array_meta"`: - vacuum array metadata only
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the vacuum. When `NULL` (default) the configuration parameters
    #'  will be retrieved from object's context.
    #'
    #' @return `TRUE`, invisibly.
    #'
    vacuum = function(mode = c("fragments",
                               "commits",
                               "fragment_meta",
                               "array_meta"),
                      cfg = NULL){

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      check_tiledb_config(cfg)

      cfg["sm.vacuum.mode"] <- mode

      .libtiledb_array_vacuum(ctx = self$ctx@ptr, uri = self$uri, cfgptr = cfg@ptr)

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
    #' @param mode The vacuum mode, one of the following:
    #'
    #'  - `"fragments"`: - vacuum all fragments (default)
    #'  - `"commits"`: - vacuum all commit files
    #'  - `"fragment_meta"`: - vacuum only fragment metadata footers to a single file
    #'  - `"array_meta"`: - vacuum array metadata only
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the vacuum process. When `NULL` (default) the configuration parameters will
    #'  be retrieved from object's context.
    #'
    #' @return This function will return a [mirai::mirai()] object immediately. When it is
    #' resolved, it returns `TRUE` indicating vacuum success.
    #'
    vacuum_async = function(mode = c("fragments",
                                     "commits",
                                     "fragment_meta",
                                     "array_meta"),
                            cfg = NULL) {

      if (!requireNamespace("mirai", quietly = TRUE)) {
        cli::cli_abort("{.arg vacuum_async} requires {.pkg '{.href [mirai](https://cran.r-project.org/web/packages/mirai/index.html)}'} package.", call = NULL)
      }

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      check_tiledb_config(cfg)

      cfg["sm.vacuum.mode"] <- mode

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

        ctx <- R6.tiledb::new_context(cfg)
        R6.tiledb:::.libtiledb_array_vacuum(ctx = ctx@ptr, uri = uri)

        return(TRUE)

      }, uri = self$uri, config_params = as.vector(cfg), .compute = ns)

      # reset fragment object
      private$.fragments_object <- NULL

      m
    },
    #' @description Consolidates and vacuums the fragments.
    #'
    #' @param mode The consolidate and vacuum mode, one of the following:
    #'
    #'  - `"fragments"`: - consolidate all fragments (default)
    #'  - `"commits"`: - consolidate all commit files
    #'  - `"fragment_meta"`: - consolidate only fragment metadata footers to a single file
    #'  - `"array_meta"`: - consolidate array metadata only
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the consolidation and vacuum. When `NULL` (default) the configuration parameters will
    #'  be retrieved from object's context.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXt`. If not provided, the default values from configuration
    #' object will be used.
    #'
    #' @return `TRUE`, invisibly.
    #'
    consolidate_and_vacuum = function(mode = c("fragments",
                                               "commits",
                                               "fragment_meta",
                                               "array_meta"),
                                      cfg = NULL,
                                      start_time = NULL,
                                      end_time = NULL) {

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      check_tiledb_config(cfg)

      cfg["sm.consolidation.mode"] <- mode
      cfg["sm.vacuum.mode"] <- mode

      if (!is.null(start_time)) {

        cfg["sm.consolidation.timestamp_start"] <- .posixt_to_int64char(start_time)

      }

      if (!is.null(end_time)) {

        cfg["sm.consolidation.timestamp_end"] <- .posixt_to_int64char(end_time)

      }

      tiledb::array_consolidate(self$uri, cfg = cfg)
      tiledb::array_vacuum(self$uri, cfg = cfg)

      # reset fragment object
      private$.fragments_object <- NULL

      invisible(TRUE)
    },


    #' @description Consolidate and vacuum fragments asynchronously.
    #'
    #' The consolidation and vacuum will run in a separate R process in a clean environment.
    #'
    #' **Note this function requires the [mirai](https://cran.r-project.org/web/packages/mirai/index.html) package**.
    #'
    #' @param mode The consolidate and vacuum mode, one of the following:
    #'
    #'  - `"fragments"`: - consolidate all fragments (default)
    #'  - `"commits"`: - consolidate all commit files
    #'  - `"fragment_meta"`: - consolidate only fragment metadata footers to a single file
    #'  - `"array_meta"`: - consolidate array metadata only
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the consolidation and vacuum. When `NULL` (default) the configuration parameters will
    #'  be retrieved from object's context.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXt`. If not provided, the default values from configuration
    #' object will be used.
    #'
    #' @return This function will return a [mirai::mirai()] object immediately. When it is
    #' resolved, it returns `TRUE` indicating consolidation success.
    #'
    consolidate_and_vacuum_async = function(mode = c("fragments",
                                                     "commits",
                                                     "fragment_meta",
                                                     "array_meta"),
                                            cfg = NULL,
                                            start_time = NULL,
                                            end_time = NULL) {

      if (!requireNamespace("mirai", quietly = TRUE)) {
        cli::cli_abort("{.arg consolicate_async} requires {.pkg '{.href [mirai](https://cran.r-project.org/web/packages/mirai/index.html)}'} package.", call = NULL)
      }

      mode <- match.arg(mode)

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$ctx)
      }

      check_tiledb_config(cfg)

      cfg["sm.consolidation.mode"] <- mode
      cfg["sm.vacuum.mode"] <- mode

      if (!is.null(start_time)) {

        cfg["sm.consolidation.timestamp_start"] <- .posixt_to_int64char(start_time)

      }

      if (!is.null(end_time)) {

        cfg["sm.consolidation.timestamp_end"] <- .posixt_to_int64char(end_time)

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
        cli::cli_abort("{.arg {deparse(substitute(x))}} is not an attribute.", call = NULL)
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
    #'  Note that the return object will be of class `data.table` if the
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
    #'  Note that the return object will be of class `data.table` if the
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
        private$check_read_only("fragment_object")
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
        private$check_read_only("schema_version")
      }

      tiledb::tiledb_array_schema_version(self$schema())
    },
    #' @field is_sparse Check array schema for sparsity.
    #'
    is_sparse = function(value) {

      if (!missing(value)) {
        private$check_read_only("is_sparse")
      }
      sch <- self$schema()

      tiledb::is.sparse(sch)

    }
  ),

  private = list(

    # Contains TileDBFragments object
    .fragments_object = NULL
  )
)
