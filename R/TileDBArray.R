#' @title Generate a `TileDBArray` Object
#'
#' @description Base class for representing a `TileDB` Array.
#'
#' ## Initialization
#' A new `TileDBArray` instance is initialized using the `new()` method:
#'
#' ```r
#'  # uri path
#'  uri <- tempdir()
#'  # new instance
#'  obj <- TileDBArray$new(uri = uri)
#'  # does array exist at this uri
#'  obj$exists() # FALSE
#'
#'  unlink(uri)
#' ```
#' @returns An object of class `TileDBArray`, `R6`.
#'
#' @export
TileDBArray <- R6::R6Class(
  classname = "TileDBArray",
  inherit = TileDBObject,
  public = list(
    #' @description Open TileDB array object for read or write.
    #'
    #' This methods opens the underlying [tiledb::tiledb_array()] object in the
    #' new mode if it is different from the current mode.
    #'
    #' When the new mode and current mode is the same, no action is taken.
    #' To force close and then open again use `reopen()` method.
    #'
    #' When a time-stamp is specified, it will be effective in `"READ"` mode  only.
    #'
    #' @param mode Mode to open : either `"READ"` or `"WRITE"`.  Default is `"READ"`.
    #'
    #' @return The object, invisibly.
    #'
    open = function(mode = c("READ", "WRITE")) {

      private$check_object_exists()
      mode <- match.arg(mode)

      if (is.null(private$.tiledb_array)) {
        private$log_debug("open", "Opening with mode '{}'", mode)
        private$initialize_object()
      }

      has_tstamp <- !is.null(self$tiledb_timestamp)
      is_write <- mode == "WRITE"

      init_mode <- self$mode
      is_identical_mode <- init_mode == mode

      if (isFALSE(has_tstamp) | isTRUE(has_tstamp & is_write)) {

        # Notes:
        #  - If new mode is different from current mode then switch to new mode, otherwise no action
        #  - Cases (1) no timestamp or (2) has timestamp but new mode is "WRITE" (here timestamp will no have effect)

        private$log_debug0("open", "Requested open mode is {}",
                           ifelse(is_identical_mode, "identical, no mode switch", "not identical, switch mode"))

        if (isFALSE(is_identical_mode)) {

          if (tiledb::tiledb_array_is_open(private$.tiledb_array)) {

            private$log_debug("open", "Closing to switch from {} to {} mode", init_mode, mode)

            tiledb::tiledb_array_close(self$object)
          }

          private$log_debug("open", "Opening in {} mode", mode)

          private$.tiledb_array <- tiledb::tiledb_array_open(self$object, type = mode)
          private$.mode <- mode
          private$update_metadata_cache()
        }

      } else if (isTRUE(has_tstamp & !is_write)) {

        # Opening array at time-stamp. For READ only.
        #
        # Note: If array is open, we must close it and re-opening it at time-stamp.

        if (tiledb::tiledb_array_is_open(private$.tiledb_array)) {

          private$log_debug("open", "Closing to re-opening from {} to {} mode", init_mode, mode)

          tiledb::tiledb_array_close(self$object)
        }

        private$log_debug("open", "Opening in {} mode at {}", mode, self$tiledb_timestamp)

        tstart <- self$tiledb_timestamp$timestamp_start
        tend <- self$tiledb_timestamp$timestamp_end

        private$.tiledb_array <- tiledb::tiledb_array(self$uri,
                                                      query_type = mode,
                                                      query_layout = "UNORDERED",
                                                      keep_open = TRUE,
                                                      timestamp_start = tstart,
                                                      timestamp_end = tend)
        private$.mode <- mode
        private$update_metadata_cache()

      }
# libtiledb_array_set_open_timestamp_end
      invisible(self)

    },

    #' @description Close the object.
    #'
    #' @return The object, invisibly.
    #'
    close = function() {

      private$log_debug("close", "Closing array")

      tiledb::tiledb_array_close(self$object)

      private$.mode <-  "CLOSED"

      invisible(self)
    },

    #' @description Return a [tiledb::tiledb_array] object.
    #'
    #' If a `query_type` not provided then it will be inherited from class
    #' mode; in case the class mode is `"CLOSED"`, then the query type
    #' defaults to `"READ"`.
    #'
    #' @param ... Optional arguments to pass to `tiledb::tiledb_array()`.
    #'
    #' @return A [tiledb::tiledb_array] object.
    #'
    tiledb_array = function(...) {

      private$check_object_exists()

      args <- list(...)
      args$uri <- self$uri
      # user has not supplied 'query_type'
      if (is.null(args$query_type)) {
        mode <- self$mode
        args$query_type <- ifelse(mode == "CLOSED", "READ", mode)
      }
      args$query_layout <- "UNORDERED"
      args$ctx <- self$ctx

      private$log_debug("tiledb_array", "Array handle with query type '{}' and layout '{}'", args$query_type, args$query_layout)

      do.call(tiledb::tiledb_array, args)
    },

    #' @description Retrieve the array schema as TileDB schema.
    #'
    #' @return A [tiledb::tiledb_array_schema] object.
    #'
    schema = function() {

      private$check_object_exists()

      tiledb::schema(self$object)
    },
    #' @description Retrieve schema information.
    #'
    #' @return A `data.frame` object with four column schema
    #'  information: `names`, `types`, `status` and `enum`.
    #'
    schema_info = function() {
      sch <- self$schema()
      data.frame(
        names = tiledb::tiledb_schema_get_names(sch),
        types = tiledb::tiledb_schema_get_types(sch),
        status = .tiledb_schema_get_dim_attr_status(sch),
        enum = .tiledb_schema_get_enumeration_status(sch),
        row.names = NULL
      )
    },

    #' @description Retrieve the array dimensions.
    #'
    #' @return A named list of [tiledb::tiledb_dim] objects.
    #'
    dimensions = function() {
      dims <- tiledb::dimensions(self$schema())
      setNames(dims, nm = vapply_char(dims, tiledb::name))
    },


    #' @description Retrieve the array attributes.
    #'
    #' @return A list of [tiledb::tiledb_attr] objects.
    #'
    attributes = function() {
      tiledb::attrs(self$schema())
    },

    #' @description Retrieve dimension names.
    #'
    #' @return A character vector with the array's dimension names.
    #'
    dimnames = function() {
      names(self$dimensions())
    },

    #' @description Retrieve attribute names.
    #'
    #' @return A character vector with the array's attribute names.
    #'
    attrnames = function() {
      vapply_char(self$attributes(), FUN = tiledb::name, USE.NAMES = FALSE)
    },

    #' @description Retrieve the names of all columns, including dimensions and
    #' attributes.
    #'
    #' @return A character vector with the array's column names.
    #'
    colnames = function() {

      c(self$dimnames(), self$attrnames())

    },

    #' @description Print summary of the array.
    #'
    print = function() {

      if (self$exists()) {

        cli::cli_div(theme = list(.val = list(color = "cyan"),
                                  .emph = list(color = "orange")))

        dims <- cli::cli_vec(self$dimnames(), list("vec-trunc" = 3))
        attrs <- cli::cli_vec(self$attrnames(), list("vec-trunc" = 3))

        olid <- cli::cli_ol()
        ulid <- cli::cli_ul()
        cli::cli_inform("R6Class: {.cls {self$class()}}")
        cli::cli_bullets(c(">" = "URI Basename: {.emph {basename(self$uri)}}"))
        ulid <- cli::cli_ul()
        cli::cli_li(c("Dimensions: {.val {dims}}",
                      "Attributes: {.val {attrs}}"))
        cli::cli_end(olid)
        cli::cli_end(ulid)
      } else {
        cli::cli_inform(c("i" = "R6Class: {.cls {self$class()}} object does not exist."))
      }

      invisible(self)
    }
  ),

  active = list(
    #' @field object Access the underlying [tiledb::tiledb_array()] object. When
    #' used before open() method, the underlying array will be initialised at `"READ"`
    #' mode and kept open.
    object = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("object")
      }
      # If the array was created after the object was instantiated, we need to
      # initialize private$.tiledb_array
      if (is.null(private$.tiledb_array)) {
        private$initialize_object()
      }
      private$.tiledb_array
    }
  ),

  private = list(

    # Internal pointer to the TileDB array.
    #
    # Important implementation note:
    # * In TileDB-R there is an unopened handle obtained by tiledb::tiledb_array, which takes
    #   a URI as its argument.
    # * One may then open and close this using tiledb::tiledb_array_open (for read or write)
    #   and tiledb::tiledb_array_close, which take a tiledb_array handle as their first argument.
    #
    # However, for groups:
    # * tiledb::tiledb_group and tiledb::group_open both return an object opened for read or write.
    # * Therefore for groups we cannot imitate the behaviour for arrays.
    #
    # For this reason there is a limit to how much handle-abstraction we can do in the TileDBObject
    # parent class. In particular, we cannot have a single .tiledb_object shared by both TileDBArray
    # and TileDBGroup.
    .tiledb_array = NULL,


    # Once the array has been created this initializes the TileDB array object
    # and stores the reference in private$.tiledb_array.
    initialize_object = function() {

      private$.tiledb_array <- tiledb::tiledb_array(
        uri = self$uri,
        ctx = self$ctx,
        query_layout = "UNORDERED",
        keep_open = TRUE)
    }
)
)
