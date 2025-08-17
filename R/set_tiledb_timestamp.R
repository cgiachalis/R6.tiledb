#' Set a `TileDB` time-stamp
#'
#' Define a list of start,end timestamps intended for opening a `TileDB`
#' object, either at initialisation or via active field `tiledb_timestamp`.
#'
#' @param start_time,end_time An object coercible to `POSIXct`. See [as.POSIXct()].
#' @param tz A character string for the time zone specification to be used
#' for the conversion.
#'
#' @returns An list object of class `tiledb_timestamp`.
#'
#' @export
#'
#' @seealso [TileDBObject]
#'
#' @examples
#' # Character input
#' set_tiledb_timestamp("2025-01-01", "2025-08-01")
#'
#' # Numeric input
#' set_tiledb_timestamp(1000000, 1000000*4)
#'
#' ts1 <- 0
#' ts2 <- Sys.time() - 100
#' tstamp <- set_tiledb_timestamp(ts1, ts2)
#'
#' print(tstamp, tz = "Europe/London")
#'
set_tiledb_timestamp <- function(start_time, end_time, tz = "UTC") {

  is_st_default <- is_end_default  <- FALSE

  if (missing(start_time)) {
    start_time <- as.POSIXct(0, tz = tz)
    is_st_default <- TRUE
  } else if (isTRUE(is.na(start_time)) | is.null(start_time)) {
    start_time <- as.POSIXct(0, tz = tz)
    is_st_default <- TRUE
  } else {
    start_time <- as.POSIXct(start_time, tz = tz)
  }

  if (missing(end_time)) {
    end_time <- as.POSIXct(double(), tz = tz)
    is_end_default <- TRUE
  } else if (isTRUE(is.na(end_time)) | is.null(end_time)) {
    end_time <- as.POSIXct(double(), tz = tz)
    is_end_default <- TRUE
  } else {
    end_time <- as.POSIXct(end_time, tz = tz)
  }

  is_default <- is_st_default & is_end_default

  if ( length(end_time) > 0) {
    if (start_time > end_time) {
      cli::cli_abort("{.arg start_time} is greater than {.arg end_time}.", call = NULL)
    }
  } else {
    if (start_time > Sys.time()) {
      cli::cli_abort("{.arg start_time} is greater than {.arg end_time}.", call = NULL)
    }
  }

  structure(list(timestamp_start = start_time,
                 timestamp_end = end_time),
            class = c("tiledb_timestamp"),
            user_tstamp = !is_default)
}
