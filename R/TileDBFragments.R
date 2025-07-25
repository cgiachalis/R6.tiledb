
# TODO: add debug statements
# TODO: UNIT TESTING
# TODO: list delete fragments
# TODO: revert_deleted_frags

#' @title Generate a `TileDBFragments` Object
#'
#' @description
#' An R6 class for handling `TileDB` Fragments.
#'
#' @returns An object of class `TileDBFragments`.
#'
#' @export
TileDBFragments <- R6::R6Class(
  classname = "TileDBFragments",
  public = list(
    #' @description Create a new `TileDBFragments` instance.
    #'
    #' @param uri URI path for the `TileDB` Array.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object.
    #'
    initialize = function(uri, ctx = NULL) {

      if (missing(uri)) {
        cli::cli_abort("{.arg uri} argument is missing.", call = NULL)
      }

      # TODO: review do we need TileDBURI?
      private$tiledb_uri <- TileDBURI$new(uri)

      # Set context
      if (is.null(ctx)) ctx <- tiledb::tiledb_ctx()

      if (!inherits(ctx, what = 'tiledb_ctx')) {
        cli::cli_abort("{.arg ctx} must be a {.emph 'tiledb_ctx'} object.", call = NULL)
      }

      private$.tiledb_ctx <- ctx

      # TODO: review
      private$log_debug("initialize", "Initialize TileDBFragments")

    },
    #' @description Print the name of the R6 class.
    #'
    class = function() {
      class(self)[1]
    },
    #' @description The number of fragments.
    #'
    frag_num = function() {
      tiledb::tiledb_fragment_info_get_num(private$finfo())
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
    frag_uris = function(trunc_uri = TRUE) {

      idx <- self$frag_num()

      # No fragments, then return empty data.frame
      if (idx == 0) {
        out <-  data.frame(Fragment = character(),
                           start_timestamp = numeric(),
                           end_timestamp = numeric(),
                           URI = character())
        return(out)

      }

      # C++ index
      idx <- idx - 1
      finfo <- private$finfo()

      lst <- lapply(0:idx, function(.x) {
        uri <- tiledb::tiledb_fragment_info_uri(finfo, .x)
        tsp <- tiledb::tiledb_fragment_info_get_timestamp_range(finfo, .x)
        tsp <- sapply(tsp, as.POSIXct, tz = "UTC", simplify = FALSE)
        data.frame(Fragment = paste0("#",.x + 1),
                   start_timestamp = tsp[[1]],
                   end_timestamp = tsp[[2]],
                   URI = ifelse(trunc_uri, sub(".*__fragments/", "", uri) , uri))

      })

      if (requireNamespace("data.table", quietly = TRUE)) {
        out <- data.table::rbindlist(lst)
      } else {
        out <- do.call(rbind, lst)
      }

      out

    },
    #' @description Refresh the TileDB Fragment Info object.
    #'
    #' @return The object, invisibly.
    #'
    reload_finfo = function() {

      private$.finfo <- tiledb::tiledb_fragment_info(self$uri)

      invisible(self)
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
    to_vacuum = function(trunc_uri = TRUE) {

      finfo <- private$finfo()

      idx <- tiledb::tiledb_fragment_info_get_to_vacuum_num(finfo) - 1

      if (idx < 0) {

          out <-  data.frame(Fragment = character(),
                             start_timestamp = numeric(),
                             end_timestamp = numeric(),
                             URI = character())
          return(out)

      }

      lst <- lapply(0:idx, function(.x) {

        uri <- tiledb::tiledb_fragment_info_get_to_vacuum_uri(finfo, .x)

        trunc01 <-  sub(".*__fragments/__", "", uri)
        tsp <- c(as.numeric(substr(trunc01, 0, 13)) + 0.0001,
                 as.numeric(substr(trunc01, 15, 27)) + 0.0001) / 1000

        tsp <- sapply(tsp, as.POSIXct, tz = "UTC", simplify = FALSE)
        data.frame(Fragment = paste0("#", .x + 1),
                   start_timestamp = tsp[[1]],
                   end_timestamp = tsp[[2]],
                   URI = ifelse(trunc_uri, sub(".*__fragments/", "", uri) , uri))
      })

      if (requireNamespace("data.table", quietly = TRUE)) {
        out <- data.table::rbindlist(lst)
      } else {
        out <- do.call(rbind, lst)
      }

      out

    },
    #' @description Return the number of fragments to vacuum
    #'
    #' @return An numeric value.
    #'
    to_vacuum_num = function() {
      tiledb::tiledb_fragment_info_get_to_vacuum_num(private$finfo())
    },
    #' @description Delete fragments using a range of timestamps.
    #'
    #' @param timestamp_range A vector of length 2 with date time elements
    #' of class `POSIXct` that represents the start and end time stamps.
    #'
    #' @return `TRUE` for successful deletion, invisibly.
    #'
    delete_fragment_range = function(timestamp_range) {

      if (!inherits(timestamp_range, "POSIXct") | length(timestamp_range) != 2L) {
        cli::cli_abort("{.arg {deparse(substitute(timestamp_range))}} must be a class  {.cls POSIXct} of length 2.", call = NULL)
      }

      if (timestamp_range[1] > timestamp_range[2]) {
        cli::cli_abort(
          c("{.arg {deparse(substitute(timestamp_range))}} not in the right order: {.cls {timestamp_range}}.",
            "i" = "Please use {.emph {'<start_timestamp, end_timestamp>'}} format."),
          call = NULL)
      }

      arr <- tiledb::tiledb_array(self$uri, keep_open = FALSE)

      tiledb::tiledb_array_delete_fragments(arr,
                                            ts_start = timestamp_range[1],
                                            ts_end = timestamp_range[2],
                                            ctx = private$.tiledb_ctx)
      self$reload_finfo()

      invisible(TRUE)

    },
    #' @description Delete fragments using a vector of fragment uris.
    #'
    #' Use `$frag_uris(trunc_uri = FALSE)` method to get a `data.frame`
    #' with all fragment uri paths.
    #'
    #' @param frag_uris A vector of fragment uris.
    #'
    #' @return `TRUE` for successful deletion, invisibly.
    #'
    delete_fragment_list = function(frag_uris) {

      if (isFALSE(is.character(frag_uris))) {
        cli::cli_abort("{.arg {deparse(substitute(frag_uris))}}  must be a character vector.", call = NULL)
      }

      arr <- tiledb::tiledb_array(self$uri, keep_open = FALSE)

      tiledb::tiledb_array_delete_fragments_list(arr,
                                                 fragments = frag_uris,
                                                 ctx = private$.tiledb_ctx)
      self$reload_finfo()

      invisible(TRUE)

    },
    #' @description Delete a fragment by index.
    #'
    #' @param n A fragment index to be deleted (starts at 1).
    #'
    #' @return A boolean,  invisibly. `TRUE` for successful deletion and
    #' `FALSE` for no fragment to delete.
    #'
    delete_fragment = function(n) {

      if (isFALSE( rlang::is_scalar_double(n))) {
        cli::cli_abort("{.arg {deparse(substitute(n))}}  must be a numeric value.", call = NULL)
      }

      furis <- self$frag_uris(FALSE)

      num_frags <- nrow(furis)

      if (num_frags == 0) {
        cli::cli_alert_info("No fragments found to delete.")
        return(invisible(FALSE))

      }

      # if (n > nrow(furis)) {
      #   cli::cli_abort("Out of bound fragment index.")
      # }

      old_frags <- furis$URI[n]

      arr <- tiledb::tiledb_array(self$uri, keep_open = FALSE)

      tiledb::tiledb_array_delete_fragments_list(arr,
                                                 fragments = old_frags,
                                                 ctx = private$.tiledb_ctx)
      self$reload_finfo()

      invisible(TRUE)

    },
    #' @description Dump to console the commit fragments.
    #'
    dump = function() {
      tiledb::tiledb_fragment_info_dump(private$finfo())
    }
  ),

  active = list(
    #' @field uri The URI of the TileDB object.
    #'
    uri = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("uri")
      }
      private$tiledb_uri$uri
    },
    #TODO review
    #' @field fragment_info TileDB Fragment Info object.
    #'
    fragment_info = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("fragment_info")
      }
      private$finfo()

    }
  ),

  private = list(
    # TileDBURI object or string?
    tiledb_uri = NULL,

    .tiledb_ctx = NULL,
    .finfo = NULL,

    # TileDB fragment info object
    finfo = function(){
      if (is.null(private$.finfo)){
        private$.finfo <- tiledb::tiledb_fragment_info(self$uri)
      }
      private$.finfo
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
