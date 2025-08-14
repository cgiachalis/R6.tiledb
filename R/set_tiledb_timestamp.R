#' Set a `TileDB` time-stamp
#'
#' Define a list of start,end timestamps intended for opening a `TileDB`
#' object, either at initialisation or via active field `tiledb_timestamp`.
#'
#' @param ts_start,ts_end An object coercible to `POSIXct` using [as.POSIXct()].
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
set_tiledb_timestamp <- function(ts_start, ts_end, tz = "UTC") {

  if (missing(ts_start)) {
    ts_start <- as.POSIXct(double(), tz = tz)
  } else if (is.na(ts_start) | is.null(ts_start)) {
    ts_start <- as.POSIXct(double(), tz = tz)
  } else {
    ts_start <- as.POSIXct(ts_start, tz = tz)
  }

  if (missing(ts_end)) {
    ts_end <- as.POSIXct(double(), tz = tz)
  } else if (is.na(ts_end) | is.null(ts_end)) {
    ts_end <- as.POSIXct(double(), tz = tz)
  } else {
    ts_end <- as.POSIXct(ts_end, tz = tz)
  }

  start_ok <- length(ts_start) > 0
  end_ok <- length(ts_end) > 0

  structure(list(timestamp_start = ts_start,
                 timestamp_end = ts_end),
            class = c("tiledb_timestamp"),
            user_tstamp = start_ok | end_ok)
}
