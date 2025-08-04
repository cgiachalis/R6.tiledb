#' @title Generate a `TileDBObject` Object
#'
#' @description
#' A virtual class to implement shared functionality for [TileDBArray] and
#' [TileDBGroup] classes.
#'
#'  **This class is not intended to be used directly**.
#'
#' @keywords internal
#'
#' @export
TileDBObject <- R6::R6Class(
  classname = "TileDBObject",
  public = list(
    #' @description Create a new TileDB object.
    #'
    #' @param uri URI path for the TileDB object.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object.
    #' @param tiledb_timestamp Optional Datetime (POSIXct) with TileDB timestamp.
    #'
    initialize = function(uri,
                          ctx = NULL,
                          tiledb_timestamp = NULL) {

      if (missing(uri)) {
        cli::cli_abort("{.arg uri} argument is missing.", call = NULL)
      }

      check_uri(uri)

      private$tiledb_uri <- uri

      # Set context

      if (is.null(ctx)) ctx <- tiledb::tiledb_ctx()

      if (!inherits(ctx, what = 'tiledb_ctx')) {
        cli::cli_abort("{.arg ctx}  must be a {.cls tiledb_ctx} object.", call = NULL)
      }

      private$.tiledb_ctx <- ctx


      if (!is.null(tiledb_timestamp)) {

        check_timestamp(tiledb_timestamp)

        private$.tiledb_timestamp <- tiledb_timestamp
      }

      private$log_debug("initialize", "Initialize with timestamp ({})", self$tiledb_timestamp %||% "now")

    },

    #' @description Print the name of the R6 class.
    #'
    class = function() {
      class(self)[1]
    },

    #' @description Determine if the object is open for reading or writing.
    #'
    #' @return `TRUE` if the object is open, otherwise `FALSE`.
    #'
    is_open = function() {
      self$mode != 'CLOSED'
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

      private$check_object_exists()

      mode <- match.arg(mode)

      if (!is.null(tiledb_timestamp)) {

        check_timestamp(tiledb_timestamp)

        private$.tiledb_timestamp <- tiledb_timestamp
      }

      self$close()

      private$.tiledb_timestamp <- tiledb_timestamp

      self$open(mode)

      invisible(self)
    },

    #' @description Check if the object exists.
    #'
    #' @return `TRUE` if the object exists, `FALSE` otherwise.
    #'
    exists = function() {
      if (self$class() == "TileDBObject") {
        expected_type <- c("ARRAY", "GROUP")
      } else if (inherits(self, "TileDBArray")) {
        expected_type <- "ARRAY"
      } else if (inherits(self, "TileDBGroup")) {
        expected_type <- "GROUP"
      } else {
        cli::cli_abort("Unknown object type {.cls {self$class()}}", call = NULL)
      }
      self$object_type %in% expected_type
    }
  ),

  active = list(

    #' @field ctx TileDB Context.
    ctx = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("ctx")
      }
      private$.tiledb_ctx
    },

    #' @field tiledb_timestamp Time that object was opened at.
    #'
    tiledb_timestamp = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("tiledb_timestamp")
      }
      private$.tiledb_timestamp
    },

    #' @field uri The URI of the TileDB object.
    #'
    uri = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("uri")
      }
      private$tiledb_uri
    },

    #' @field mode Get the mode of the object: one of the following:
    #' `"CLOSED"`, `"READ"` or `"WRITE"`.
    #'
    mode = function(value) {

      if (!missing(value)) {
        .emit_read_only_error("uri")
      }

      if (is.null(private$.mode)) {
        "CLOSED"
      } else {
        private$.mode
      }
    },
    #' @field object_type The TileDB object type:
    #'
    #'  - `"ARRAY"`, for dense or sparse array resource
    #'  - `"GROUP"`, for group resource
    #'  - `"INVALID"`, for not a TileDB resource
    #'
    object_type = function(value) {

      if (!missing(value)) {
        .emit_read_only_error("object_type")
      }

      # For NULL or "INVALID" state we are re-checking
      if (is.null(private$.object_type) || private$.object_type == "INVALID") {
        private$.object_type <- tiledb::tiledb_object_type(self$uri, ctx = private$.tiledb_ctx)
      }
      private$.object_type

    }
  ),

  private = list(
    # Pro tip: in R6 we can't set these to anything other than NULL here, even if we want to.  If
    # you want them defaulted to anything other than NULL, leave them NULL here and set the defaults
    # in the constructor.

    # Set by TileDBArray and TileDBGroup, as stateful handles have incompatible semantics
    # we can't completely abstract here in this parent class
    #
    # Semantics:
    # * "READ" when opened for read
    # * "WRITE" when opened for write
    # * NULL when never opened, or when closed.
    # * In particular, an is-open predicate can be reliably implemented by
    #   checking if .mode is non-null.
    .mode = NULL,

    # * "ARRAY", "GROUP" or "INVALID"
    .object_type = NULL,

    # Contains TileDBURI object
    tiledb_uri = NULL,

    # Opener-supplied POSIXct timestamp, if any. TileDBArray and TileDBGroup are each responsible
    # for making this effective, since the methods differ slightly.
    .tiledb_timestamp = NULL,

    .tiledb_ctx = NULL,

    is_open_for_read = function() {
      # Pro-tip: it's not enough to check $private.mode != "READ", since logical(0) isn't
      # the same as FALSE
      if (is.null(private$.mode)) {
        FALSE
      } else if (private$.mode != "READ") {
        FALSE
      } else {
        TRUE
      }
    },

    is_open_for_write = function() {
      if (is.null(private$.mode)) {
        FALSE
      } else if (private$.mode != "WRITE") {
        FALSE
      } else {
        TRUE
      }
    },

    # read requires open for read mode.
    check_open_for_read = function() {

      if (!private$is_open_for_read()) {

        cli::cli_abort(
          c("{.cls {self$class()}} should be open for read:",
            "*" = cli::style_italic("{.val {self$uri}}")), call = NULL)

      }
    },

    # write requires open for write mode.
    check_open_for_write = function() {

      if (!private$is_open_for_write()) {

        cli::cli_abort(
          c("{.cls {self$class()}} should be open for write:",
          "*" = cli::style_italic("{.val {self$uri}}")), call = NULL)

      }
    },

    # get-metadata requires open for read mode or write mode.
    check_open_for_read_or_write = function() {

      if (!self$is_open()) {

        cli::cli_abort(
          c("{.cls {self$class()}} should be open for read or write:",
            "*" = cli::style_italic("{.val {self$uri}}")), call = NULL)

      }
    },

    # check method is for internal use
    check_internal_use = function(x, method) {
      if (is.null(x) || x != "permit") {
        cli::cli_abort(
          c(paste(cli::col_br_blue("{.arg {deparse(substitute(method))}}"),  "method is for internal use only."),
            "i" = "Please use a subclass or factory method."),
          call = NULL)
      }
    },

    # set-metadata requires named list.
    check_metadata = function(x) {
      if (!.is_named_list(x)) {
        cli::cli_abort(
          "{.arg {deparse(substitute(x))}} should be a named list with metadata.",
          call = NULL
        )
      }
    },

    check_scalar_character = function(x) {
      if (isFALSE(rlang::is_scalar_character(x))) {
        cli::cli_abort("{.arg {deparse(substitute(x))}}  must be a single character string.", call = NULL)
      }
    },

    check_object_exists = function() {
      if (!self$exists()) {
        cli::cli_abort("R6Class: {.cls {self$class()}} object does not exist.", call = NULL)
      }
    },
    log_debug0 = function(method, comment, ...) {

      comment <- spdl::fmt(comment, ...)

      spdl::debug("[{}] [{}${}] {}",
                  getPackageName(parent.frame()),
                  self$class(),
                  method,
                  comment)

    },
    log_debug = function(method, comment, ...) {

      comment <- spdl::fmt(comment, ...)

        spdl::debug("[{}] [{}${}] {} for uri '{}'",
                    getPackageName(parent.frame()),
                    self$class(),
                    method,
                    comment,
                    self$uri)

    }
  )
)
