#' Set a `TileDB` time-stamp
#'
#' Define a list of start,end timestamps intended for opening a `TileDB`
#' object, either at initialisation or via active field `tiledb_timestamp`.
#'
#' @param start_time,end_time An object coercible to `POSIXct` using [as.POSIXct()].
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
set_tiledb_timestamp <- function(start_time, end_time, tz = "UTC") {

  if (missing(start_time)) {
    start_time <- as.POSIXct(0, tz = tz)
  } else if (is.na(start_time) | is.null(start_time)) {
    start_time <- as.POSIXct(0, tz = tz)
  } else {
    start_time <- as.POSIXct(start_time, tz = tz)
  }

  if (missing(end_time)) {
    end_time <- as.POSIXct(double(), tz = tz)
  } else if (is.na(end_time) | is.null(end_time)) {
    end_time <- as.POSIXct(double(), tz = tz)
  } else {
    end_time <- as.POSIXct(end_time, tz = tz)
  }

  start_ok <- length(start_time) > 0
  end_ok <- length(end_time) > 0

  if (end_ok) {
    if (start_time > end_time) {
      cli::cli_abort("{.arg start_time} is greater than {.arg end_time}", call = NULL)
    }
  } else {
    if (start_time > Sys.time()) {
      cli::cli_abort("{.arg start_time} is greater than {.arg end_time}", call = NULL)
    }
  }

  structure(list(timestamp_start = start_time,
                 timestamp_end = end_time),
            class = c("tiledb_timestamp"),
            user_tstamp = start_ok | end_ok)
}
