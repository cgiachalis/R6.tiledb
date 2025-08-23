
#' @export
array_timestamps <- function(object, tz = "", ...) {
  UseMethod("array_timestamps")
}

#' Get Array Timestamps
#'
#' @param object An `R` object that contains a `TileDB` resource pointer.
#' @param tz A character string for the time zone specification to be used
#' for the conversion in print method only. Defaults to [Sys.timezone()].
#' @param ... Other arguments passed to methods. Not used.
#'
#' @returns An object of class `array_timestamps` that is a list that
#' holds the user supplied temporal timestamps and the time range the array
#' is opened at.
#'
#' @seealso [group_timestamps()], [set_tiledb_timestamp()]
#'
#' @export
#'
#' @name array_timestamps
#'
NULL

#' @export
array_timestamps.default <- function(object, tz = "", ...) {
  stop(sprintf("No method for object %s. See ?array_timestamps for details.",
               sQuote(deparse(substitute(object)))), call. = FALSE)
}

#' @export
#' @rdname array_timestamps
array_timestamps.tiledb_array <- function(object, tz = "", ...) {

  if (nchar(tz) == 0) {
    tz <- Sys.timezone()
  }

  if (isFALSE(tz %in% OlsonNames())) {
    cli::cli_abort("{.arg tz} should be valid a timezone. See {.help [{.fun OlsonNames}](base::OlsonNames)} for details.", call = NULL)
  }

  qtstart <- object@timestamp_start
  qtend <- object@timestamp_end

  otstart <- .libtiledb_array_open_timestamp_start(object@ptr)
  otend <- .libtiledb_array_open_timestamp_end(object@ptr)

  if (tiledb::tiledb_array_is_open_for_reading(object)) {
    mode <- "read"
  } else if (tiledb::tiledb_array_is_open_for_writing(object)) {
    mode <- "write"
  } else {
    mode <- "closed"
  }


  structure(
    list(
      user_query = list(timestamp_start = qtstart, timestamp_end = qtend),
      open_array = list(timestamp_start = otstart, timestamp_end = otend)
    ),
    class = "array_timestamps",
    mode = mode,
    tzone = tz
  )
}

#' @export
#' @rdname array_timestamps
array_timestamps.TileDBArray <- function(object, tz = "", ...) {

  if (!object$exists()) {
    cli::cli_abort("R6Class: {.cls {object$class()}} object does not exist.", call = NULL)
  }

  array_timestamps(object$object, tz = tz)

}

#' @export
print.array_timestamps <- function(x, ...) {

  tzx <- attr(x, "tzone", exact = TRUE)
  tz_txt <- paste0("(", tzx ,")")
  mode_txt <-  paste0("(", attr(x, "mode", exact = TRUE) ,")")

  user_query <- x$user_query
  ts_user <- vector("character", length = 2)
  ts_user[1] <- if (length(user_query$timestamp_start) == 0) {
    "none"
  }  else {
    format(user_query$timestamp_start, "%Y-%m-%d %H:%M:%S", tz = tzx)
  }

  ts_user[2] <- if (length(user_query$timestamp_end) == 0 ) {
    "none"
  } else {
    format(user_query$timestamp_end,"%Y-%m-%d %H:%M:%S", tz = tzx)
  }

  open_arr <- x$open_array
  ts_open <- vector("character", length = 2)
  ts_open[1] <- if (length(open_arr$timestamp_start) == 0) {
    "none"
  }  else {
    format(open_arr$timestamp_start, "%Y-%m-%d %H:%M:%S", tz = tzx)
  }

  ts_open[2] <- if (length(open_arr$timestamp_end) == 0 ) {
    "none"
  } else {
    format(open_arr$timestamp_end,"%Y-%m-%d %H:%M:%S", tz = tzx)
  }

  txt1 <- paste0(c("start", "end  "), ": ", cli::col_br_blue(ts_user))
  out1 <- paste0("  ", cli::col_br_cyan(cli::symbol$bullet), " ", txt1, collapse = "\n")
  txt2 <- paste0(c("start", "end  "), ": ", cli::col_br_blue(ts_open))
  out2 <- paste0("  ", cli::col_br_cyan(cli::symbol$bullet), " ", txt2, collapse = "\n")

  txt <- c(txt1, txt2)
  out <- paste0(" ", cli::col_br_cyan(cli::symbol$bullet), " ", txt, collapse = "\n")

  header <- paste0("Array Timestamps ",
                   cli::col_br_red(cli::symbol$bullet),
                   paste0(" Mode ", cli::col_grey(mode_txt), " "),
                   cli::col_br_red(cli::symbol$bullet),
                   paste0(" TZ ", cli::col_grey(tz_txt)))

  cli::cat_line(c(header,
                  cli::col_green(" Temporal Range"), out1,
                  cli::col_green(" Open Range"), out2))

  invisible(x)
}
