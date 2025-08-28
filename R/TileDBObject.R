#' @title Generate a `TileDBObject` Object
#'
#' @description
#' An parent class to implement shared functionality for [TileDBArray] and
#' [TileDBGroup] classes.
#'
#'  **This class is not intended to be used directly**.
#'
#' @keywords internal
#'
#' @returns An object of class `TileDBObject`, `R6`.
#'
#' @export
TileDBObject <- R6::R6Class(
  classname = "TileDBObject",
  public = list(
    #' @description Create a new `TileDB` object.
    #'
    #' @param uri URI path for the `TileDB` object.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object.
    #' @param tiledb_timestamp Set a `TileDB` timestamp range that
    #'  the resource will be opened at. Effective in `"READ"` mode only.
    #'  Valid options:
    #'  - A `NULL` value (default)
    #'  - An `R` object coercible to `POSIXct` with length 1 which used for end timestamp,
    #'  or length 2 with start, end timestamps
    #'  - An object of class `tiledb_timestamp`. See [set_tiledb_timestamp()]
    #'
    #'
    #' **Note:** When setting new a time-stamp, the object will be reopened only if it is in
    #' `"READ"` mode.
    #'
    initialize = function(uri,
                          ctx = NULL,
                          tiledb_timestamp = NULL) {

      if (missing(uri)) {
        cli::cli_abort("{.arg uri} argument is missing.", call = NULL)
      }

      check_uri(uri)

      private$.tiledb_uri <- uri

      if (is.null(ctx)) {
        ctx <- tiledb::tiledb_ctx()
      }

      check_tiledb_ctx(ctx)

      if (is.null(tiledb_timestamp)) {
        private$.tiledb_timestamp <- set_tiledb_timestamp()
      } else if (length(tiledb_timestamp) == 1L) {
        private$.tiledb_timestamp <- set_tiledb_timestamp(end_time = tiledb_timestamp)
      } else if (length(tiledb_timestamp) == 2L & !inherits(tiledb_timestamp, "tiledb_timestamp")) {
        private$.tiledb_timestamp <- set_tiledb_timestamp(start_time = iledb_timestamp[1],
                                                          end_time = tiledb_timestamp[2])
      } else if (inherits(tiledb_timestamp, "tiledb_timestamp")) {
        private$.tiledb_timestamp <- tiledb_timestamp
      } else {
        cli::cli_abort("Invalid 'tiledb_timestamp' input", call = NULL)
      }

      # TODO: REVIEW
      private$.tiledb_ctx <- .set_group_timestamps(ctx, private$.tiledb_timestamp)

      tend <- private$.tiledb_timestamp$timestamp_end
      tend <- if (length(tend) == 0) NULL else tend
      private$log_debug("initialize", "Initialize with timestamp ({})", tend %||% "now")

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
    #' @return The object, invisibly.
    #'
    reopen = function(mode = c('READ', 'WRITE')) {

      private$check_object_exists()

      mode <- match.arg(mode)

      self$close()

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
    },

    #' @description Retrieve metadata from a `TileDB` Object.
    #'
    #' When a `TileDB` object (array or group) is in `"CLOSED"` mode, then it will be
    #' opened in `"READ"` mode in order to fetch the metadata; and be kept opened until
    #' is closed by the user.
    #'
    #' @param keys A character vector of metadata keys to retrieve. For `NULL` (default),
    #'  it returns all metadata.
    #'
    #' @return
    #'  - For scalar key, it returns the key metadata value; if nothing is found
    #'   it returns `NULL`
    #'  - For character vector, it returns `list` of metadata with valid keys only;
    #'   if nothing is found it returns an empty `list`
    #'  - For `NULL` (default), it returns a list of all metadata
    #'
    get_metadata = function(keys = NULL) {

      if (isFALSE(.is_character(keys) || is.null(keys))) {
        cli::cli_abort(
          "{.arg {deparse(substitute(keys))}} should be either character vector or {.code NULL}.",
          call = NULL
        )
      }
      if (!self$is_open()) {
        self$open(mode = "READ")
      }

      private$log_debug("get_metatdata", "Retrieving metadata")

      private$fill_metadata_cache_if_null()


      if (!is.null(keys)) {
        if (length(keys) == 1) {
          # return scalar
          val <- private$.metadata_cache[[keys]]
          if (is.list(val)) {
            val <- unlist(val)
          }
          val
        } else {

          idx <- keys %in% names(private$.metadata_cache)
          # return subset by indexing, only valid keys
          private$.metadata_cache[keys[idx]]
        }

      } else {
        # return all
        private$.metadata_cache
      }
    },
    #' @description Add list of metadata to a `TileDB` Object.
    #'
    #' The `TileDB` object should be open in `"WRITE"` mode.
    #'
    #' @param metadata A named list of metadata.
    #'
    #' @return The object, invisibly.
    #'
    set_metadata = function(metadata) {

      private$check_object_exists()
      private$check_metadata(metadata)
      private$check_open_for_write()

      private$log_debug("set_metatdata", "Setting metadata")

      if (is.null(private$.metadata_cache)) {
        .m <- list()
        class(.m) <- c("tdb_metadata", "list")
        attr(.m, "R6.class") <- self$class()
        attr(.m, "object_type") <- self$object_type
        private$.metadata_cache <- .m
      }

      if (self$object_type == "ARRAY") {
        .put_metadata <- function(obj, key, val) {
          tiledb::tiledb_put_metadata(obj, key, val)
          private$.metadata_cache[[key]] <- val
        }

      } else if (self$object_type == "GROUP") {
        .put_metadata <- function(obj, key, val) {
          tiledb::tiledb_group_put_metadata(obj, key, val)
          private$.metadata_cache[[key]] <- val
        }
      }

      mapply(key = names(metadata),
             val = metadata,
             MoreArgs = list(obj = self$object),
             FUN = .put_metadata)

      invisible(self)
    }
  ),

  active = list(

    #' @field ctx TileDB Context.
    ctx = function(value) {
      if (!missing(value)) {
        private$check_read_only("ctx")
      }
      private$.tiledb_ctx
    },

    #'@field tiledb_timestamp Set or retrieve a `TileDB` timestamp range that
    #'  the resource will be opened at. Effective in `"READ"` mode only.
    #'
    #'  This is a **mutable** field to set timestamps dynamically
    #'  for time-travelling. Valid options:
    #'  - A `NULL` value (default)
    #'  - An `R` object coercible to `POSIXct` with length 1 which used for end timestamp,
    #'  or length 2 with start, end timestamps
    #'  - An object of class `tiledb_timestamp`. See [set_tiledb_timestamp()]
    #'
    #' **Note:** Setting a new timestamp, the object will be reopened only if it is in
    #' `"READ"` mode. For `TileDBGroup` objects will clear the member cache and will reopen
    #' the group resource so as to propagate the `TileDB` time-stamp to members.
    #'
    tiledb_timestamp = function(value) {

      private$check_object_exists()

      if (!missing(value)) {

        if (is.null(value)) {
          .time_stamp <- set_tiledb_timestamp()
        } else if (length(value) == 1L) {
          .time_stamp <- set_tiledb_timestamp(end_time = value)
        } else if (length(value) == 2L & !inherits(value, "tiledb_timestamp")) {
          .time_stamp <- set_tiledb_timestamp(start_time = value[1], end_time = value[2])
        } else if (inherits(value, "tiledb_timestamp")) {
          .time_stamp <- value
        } else {
          cli::cli_abort("Invalid 'tiledb_timestamp' input", call = NULL)
        }

        if (self$mode != "WRITE") {

          objtype <- self$object_type

          self$close()

          if (objtype == "GROUP") {
            # .member cache is only applicable to TileDBGroup
            if (!is.null(private$.member_cache)) {
              # Clear cache in order to reopen members with new timestamps
              private$.member_cache <- NULL
            }
            private$.tiledb_ctx <- .set_group_timestamps(private$.tiledb_ctx, .time_stamp)
          }

          private$.tiledb_timestamp <- .time_stamp

          self$open()
        }

      }
      private$.tiledb_timestamp
    },

    #' @field uri The URI of the `TileDB` object.
    #'
    uri = function(value) {
      if (!missing(value)) {
        private$check_read_only("uri")
      }
      private$.tiledb_uri
    },

    #' @field mode Get the mode of the object: one of the following:
    #' `"CLOSED"`, `"READ"` or `"WRITE"`.
    #'
    mode = function(value) {

      if (!missing(value)) {
        private$check_read_only("mode")
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
        private$check_read_only("object_type")
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

    #
    # * "ARRAY", "GROUP" or "INVALID"
    .object_type = NULL,

    # Contains a URI string
    .tiledb_uri = NULL,


    # Store the object from set_tiledb_timestamp
    .tiledb_timestamp = NULL,

    # TileDB Context
    .tiledb_ctx = NULL,

    # Initially NULL, once the TileDB object (array or group) is created or opened,
    # this is populated with a list that's empty or contains the array/group metadata.
    #
    # The cache allows to readback of metadata even when the array/group are open for write.
    .metadata_cache = NULL,

    # ----------------------------------------------------------------
    # Metadata-caching for Arrays and Groups

    # Fill Metadata Cache
    #
    # This will update the metadata cache if null. To force an update
    # use `private$update_metadata_cache()`
    #
    fill_metadata_cache_if_null = function() {
      if (is.null(private$.metadata_cache)) {
        private$update_metadata_cache()
      }
    },

    # Update Metadata Cache
    #
    # Array/Group must be opened for reading.
    #
    #  * NOTE:
    #     - We cannot read metadata while the object is open for writing so
    #       we must open a temporary handle for reading, to fill the cache.
    #
    update_metadata_cache = function() {

      private$log_debug("update_metadata_cache", "Updating metadata cache for class {}", self$class())

      out <- switch(self$object_type,

        GROUP = {

          group_handle <- private$.tiledb_group

          if (private$.mode == "WRITE") {

            private$log_debug("update_metadata_cache", "Getting group object")

            group_handle <- tiledb::tiledb_group(self$uri, type = "READ", ctx = private$.tiledb_ctx)
            on.exit({ tiledb::tiledb_group_close(group_handle) })
          }

          # NOTE: Strip off key attribute; see https://github.com/TileDB-Inc/TileDB-R/issues/775
          meta_list <- tiledb::tiledb_group_get_all_metadata(group_handle)

          .m <- lapply(meta_list,  function(.x) {attr(.x, "key") <- NULL; .x})

          .m

        },

        ARRAY = {

           array_handle <- private$.tiledb_array

           tstamp <- self$tiledb_timestamp
           ts_info <- attr(tstamp, which = "ts_info", exact = TRUE)
           tstart <- tstamp$timestamp_start
           tend <- tstamp$timestamp_end

           # NOTE: we should open new handle only when WRITE mode?
          if (private$.mode == "WRITE" | ts_info != "default") {

            if (length(tend) == 0) {
              tend <- Sys.time()
            }

            private$log_debug("update_metadata_cache", "Getting array object")

            array_object <- tiledb::tiledb_array(self$uri, ctx = private$.tiledb_ctx)

            array_handle <- .tiledb_array_open_at2(array_object, type = "READ", timestamp = tend)

            on.exit({.tiledb_array_close2(array_handle)})
           }

          .m  <- tiledb::tiledb_get_all_metadata(array_handle)

          .m
        }
      )

      class(out) <- c("tdb_metadata", "list")
      attr(out, "R6.class") <- self$class()
      attr(out, "object_type") <- self$object_type

      private$.metadata_cache <- out

      invisible(NULL)
    },


    # ----------------------------------------------------------------
    # Assertion - utilities

    # Assert a TileDB object if it is open in 'READ' mode
    #
    is_open_for_read = function() {

      if (is.null(private$.mode)) {
        FALSE
      } else if (private$.mode != "READ") {
        FALSE
      } else {
        TRUE
      }
    },

    # Assert a TileDB object if it is open in 'WRITE' mode
    #
    is_open_for_write = function() {
      if (is.null(private$.mode)) {
        FALSE
      } else if (private$.mode != "WRITE") {
        FALSE
      } else {
        TRUE
      }
    },

    # Errors for read-only value for intended for active bindings
    #
    check_read_only = function(x) {
      cli::cli_abort(paste0(cli::style_italic("{.val {x}}"), " is a read-only field."), call = NULL)

    },

    # Checks a TileDB object if it is open in 'WRITE' mode
    #
    check_open_for_read = function() {

      if (!private$is_open_for_read()) {

        cli::cli_abort(
          c("{.cls {self$class()}} should be open for read:",
            "*" = cli::style_italic("{.val {self$uri}}")), call = NULL)

      }
    },

    # Write methods require to open in write mode.
    check_open_for_write = function() {

      if (!private$is_open_for_write()) {

        cli::cli_abort(
          c("{.cls {self$class()}} should be open for write:",
          "*" = cli::style_italic("{.val {self$uri}}")), call = NULL)

      }
    },

    # Checks a TileDB object if it is open, 'READ' or WRITE' mode
    #
    check_open = function() {

      if (!self$is_open()) {

        cli::cli_abort(
          c("{.cls {self$class()}} should be open for read or write:",
            "*" = cli::style_italic("{.val {self$uri}}")), call = NULL)

      }
    },

    # Checks a method for internal use, useful for child classes.
    #
    # param x status flag, use "permit" to escape abort
    # param method A character describing the method
    #
    check_internal_use = function(x, method) {
      if (is.null(x) || x != "permit") {
        cli::cli_abort(
          c(paste(cli::col_br_blue("{.arg {deparse(substitute(method))}}"),  "method is for internal use only."),
            "i" = "Please use a subclass or factory method."),
          call = NULL)
      }
    },

    # Checks metadata input list. Intended for set_metadata()
    #
    # param x A named list of key value pairs of metadata.
    #
    check_metadata = function(x) {
      if (!.is_named_list(x)) {
        cli::cli_abort(
          "{.arg {deparse(substitute(x))}} should be a named list with metadata.",
          call = NULL
        )
      }

      # keys should have only character strings
      idx <- vapply_lgl(x,\(.x) .is_character(.x) & !.is_scalar_character(.x))

      if (any(idx)) {
        invalid_keys <- .string_collapse(names(which(idx)))
        cli::cli_abort(
          c("{.arg metadata} with character values should be scalar strings only, not vectors.",
            "i" = "Consider concacate or serialise them: {.val {invalid_keys}}"),
          call = NULL
        )
      }
    },

    # Checks for scalar character
    #
    # param x An R object
    #
    check_scalar_character = function(x) {
      if (isFALSE(.is_scalar_character(x))) {
        cli::cli_abort("{.arg {deparse(substitute(x))}} should be a single character string.", call = NULL)
      }
    },

    # Checks TileDB object exists
    #
    check_object_exists = function() {
      if (!self$exists()) {
        cli::cli_abort("R6Class: {.cls {self$class()}} object does not exist.", call = NULL)
      }
    },

    # Checks a TileDB object is closed
    #
    check_object_is_closed = function() {

      private$check_object_exists()

      if (self$is_open()) {
         tp <- if (self$object_type == "GROUP") {
           "TileDB Group"
         } else {
           "TileDB Array"
         }
        cli::cli_abort(c("{.arg {tp}} is already opened.",
                         "i" = "Use {.code reopen()} method instead."), call = NULL)
      }
    },

    # Debug logging
    #
    # param method A character string describing the method you're calling from.
    # param comments Logging message
    # param ... Supplementary arguments for the logging string, passed into
    # `spdl::fmt`
    #
    # This function does not include uri in the logging message
    log_debug0 = function(method, comment, ...) {

      comment <- spdl::fmt(comment, ...)

      spdl::debug("[{}] [{}${}] {}",
                  getPackageName(parent.frame()),
                  self$class(),
                  method,
                  comment)

    },
    # Debug logging
    #
    # param method A character string describing the method you're calling from.
    # param comments Logging message
    # param ... Supplementary arguments for the logging string, passed into
    # `spdl::fmt`
    #
    # This function does includes uri in the logging message
    #
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
