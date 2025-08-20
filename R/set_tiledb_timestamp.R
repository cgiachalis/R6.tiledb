#' Set a `TileDB` time-stamp
#'
#' Define a list of start,end timestamps intended for opening a `TileDB`
#' object, either at initialisation or via active field `tiledb_timestamp`.
#'
#' @param start_time,end_time An object coercible to `POSIXct`. See [as.POSIXct()].
#' @param tz A character string for the time zone specification to be used
#' for the conversion. Defaults to [Sys.timezone()].
#'
#' @returns An list object of class `tiledb_timestamp`.
#'
#' @export
#'
#' @seealso [TileDBObject]
#'
#' @examples
#' # Character input
#' set_tiledb_timestamp("2025-01-01", "2025-08-01", tz = "UTC")
#'
#' # Numeric input
#' set_tiledb_timestamp(1000000, 1000000*4, tz = "UTC")
#'
#' # Default
#' set_tiledb_timestamp(tz = "Europe/London")
#'
#' # Invalid: start_time > end_time
#' # set_tiledb_timestamp(start_time = 1, end_time = 0)
#' # Error: `start_time` is greater than `end_time`.
#'
set_tiledb_timestamp <- function(start_time, end_time, tz = "") {

  is_st_default <- is_end_default  <- FALSE

  if (nchar(tz) == 0) {
    tz <- Sys.timezone()
  }

  if (isFALSE(tz %in% OlsonNames())) {
  cli::cli_abort("{.arg tz} should be valid a timezone. See {.help [{.fun OlsonNames}](base::OlsonNames)} for details.")
  }

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

  if ( length(end_time) > 0) {
    if (start_time > end_time) {
      cli::cli_abort("{.arg start_time} is greater than {.arg end_time}.", call = NULL)
    }
  } else {
    if (start_time > Sys.time()) {
      cli::cli_abort("{.arg start_time} is greater than {.arg end_time}.", call = NULL)
    }
  }

  is_default <- is_st_default & is_end_default

  if (is_default) {
    ts_info <- "default"
  } else if(is_st_default & !is_end_default) {

    ts_info <- "user tpnt"

  } else {
    ts_info <- "user trng"
  }

  structure(
    list(timestamp_start = start_time, timestamp_end = end_time),
    class = "tiledb_timestamp",
    tzone = tz,
    ts_info = ts_info
  )

}
