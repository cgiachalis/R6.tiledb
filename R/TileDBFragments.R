#' @title Generate a `TileDBFragments` Object
#'
#' @description
#' An R6 object for working with `TileDB` Fragments.
#'
#' @returns An object of class `TileDBFragments`, `R6`.
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

      check_uri(uri)

      private$.tiledb_uri <- uri

      # Set context
      if (is.null(ctx)) {
        ctx <- tiledb::tiledb_ctx()
      }

      check_tiledb_ctx(ctx)

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
    #'  Note that the return object will be of class `data.table` if the
    #'  package is found in your system.
    #'
    frag_uris = function(trunc_uri = TRUE) {

      idx <- self$frag_num()

      private$log_debug("frag_uris", "Number of fragments: {}", idx)

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

      private$log_debug0("reload_finfo", "Fragment Info reloaded")

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
    #'  Note that the return object will be of class `data.table` if the
    #'  package is found in your system.
    #'
    to_vacuum = function(trunc_uri = TRUE) {

      finfo <- private$finfo()

      idx <- tiledb::tiledb_fragment_info_get_to_vacuum_num(finfo) - 1

      if (idx < 0) {

        private$log_debug0("to_vacuum", "No fragments found to vacuum")

          out <-  data.frame(Fragment = character(),
                             start_timestamp = numeric(),
                             end_timestamp = numeric(),
                             URI = character())
          return(out)

      }

      private$log_debug0("to_vacuum", "{} fragments to vaccum", idx + 1)

      lst <- lapply(0:idx, function(.x) {

        uri <- tiledb::tiledb_fragment_info_get_to_vacuum_uri(finfo, .x)

        trunc01 <-  sub(".*__fragments/__", "", uri)

        tsp <- strsplit(trunc01, split ='_', fixed = TRUE)[[1]]
        tsp <- as.POSIXct(as.numeric(c(tsp[1], tsp[2])) / 1000, tz = "UTC")

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
    #' @description Delete fragments using a time-stamp range.
    #'
    #' @param start_time,end_time Time stamp values. A date time objects
    #' of class `POSIXct`.
    #'
    #' @return A logical `TRUE`, invisibly. Note that if time stamps fall
    #' outside fragments' time range no deletion will incur.
    #'
    delete_fragment_range = function(start_time, end_time) {

      check_timestamp_posixt(start_time)

      check_timestamp_posixt(end_time)

      if (start_time > end_time) {
        cli::cli_abort(
          c("{.arg {deparse(substitute(start_time))}} and {.arg {deparse(substitute(end_time))}} not in the right order.",
            "i" = "Please use {.emph {'<start_timestamp, end_timestamp>'}} format."),
          call = NULL)
      }

      arr <- tiledb::tiledb_array(self$uri, keep_open = FALSE)

      tiledb::tiledb_array_delete_fragments(arr,
                                            ts_start = start_time[1],
                                            ts_end = end_time[2],
                                            ctx = private$.tiledb_ctx)
      self$reload_finfo()

      private$log_debug0("delete_fragment_range", "Fragments in the range <{},{}> deleted successfully", start_time, end_time)

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
        cli::cli_abort("{.arg {deparse(substitute(frag_uris))}} should be a character vector.", call = NULL)
      }

      arr <- tiledb::tiledb_array(self$uri, keep_open = FALSE)

      tiledb::tiledb_array_delete_fragments_list(arr,
                                                 fragments = frag_uris,
                                                 ctx = private$.tiledb_ctx)
      self$reload_finfo()

      private$log_debug0("delete_fragment_list", "Fragment list deleted successfully")

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
        cli::cli_abort("{.arg {deparse(substitute(n))}} should be a numeric value.", call = NULL)
      }

      furis <- self$frag_uris(FALSE)

      num_frags <- nrow(furis)

      if (num_frags == 0) {
        cli::cli_alert_info("No fragments found to delete.")
        return(invisible(FALSE))

      }

      if (n > num_frags) {
        cli::cli_abort("Out of bounds fragment index.", call = NULL)
      }

      old_frags <- furis$URI[n]

      arr <- tiledb::tiledb_array(self$uri, keep_open = FALSE)

      tiledb::tiledb_array_delete_fragments_list(arr,
                                                 fragments = old_frags,
                                                 ctx = private$.tiledb_ctx)
      self$reload_finfo()

      private$log_debug0(" delete_fragment", "#{} fragment deleted successfully", n)

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
      private$.tiledb_uri
    },
    #' @field fragment_info Get the TileDB Fragment Info object as returned by
    #' [tiledb::tiledb_fragment_info].
    #'
    fragment_info = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("fragment_info")
      }
      private$finfo()

    }
  ),

  private = list(

    .tiledb_uri = NULL,

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
