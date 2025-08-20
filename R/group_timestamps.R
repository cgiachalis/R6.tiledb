#' @export
group_timestamps <- function(object, tz = "", ...) {
  UseMethod("group_timestamps")
}


#' Get Group Timestamps
#'
#' @param object An `R` object with `TileDB` group pointer.
#' @param from TileDBGroup's source of timestamps: either from context `ctx`, or
#'  group object configuration `cfg`.
#' @inheritParams set_tiledb_timestamp
#' @param ... Other arguments passed to methods. Not used.
#'
#' @returns An object of class `group_timestamps` that is a list with
#' the time range the group is opened at.
#'
#' @seealso [array_timestamps()], [set_tiledb_timestamp()]
#'
#' @export
#'
#' @name group_timestamps
# @examples
NULL

#' @export
group_timestamps.default <- function(object) {
  stop(sprintf("No method for object %s. See ?group_timestamps for details.",
               sQuote(deparse(substitute(object)))), call. = FALSE)
}

#' @export
#' @rdname group_timestamps
group_timestamps.tiledb_ctx <- function(object, tz = "", ...) {

  cfg <- tiledb::config(object)

  if (nchar(tz) == 0) {
    tz <- Sys.timezone()
  }

  if (isFALSE(tz %in% OlsonNames())) {
    cli::cli_abort("{.arg tz} should be valid a timezone. See {.help [{.fun OlsonNames}](base::OlsonNames)} for details.")
  }


  tstart <- as.POSIXct(as.numeric(cfg["sm.group.timestamp_start"]) / 1000)
  tend <- cfg["sm.group.timestamp_end"]
  if (tend == "18446744073709551615") {
    tend <- NA
  } else {
    tend <- as.numeric(tend)
  }

  tend <-  as.POSIXct(tend / 1000)

  structure(
    list(timestamp_start = tstart,
         timestamp_end = tend),
    class = "group_timestamps",
    tzone = tz,
    tdbsrc = "ctx"
  )
}


#' @export
#' @rdname group_timestamps
group_timestamps.tiledb_group <- function(object,  tz = "", ...) {

  cfg <- tiledb::tiledb_group_get_config(object)

  if (nchar(tz) == 0) {
    tz <- Sys.timezone()
  }

  if (isFALSE(tz %in% OlsonNames())) {
    cli::cli_abort("{.arg tz} should be valid a timezone. See {.help [{.fun OlsonNames}](base::OlsonNames)} for details.")
  }

  tstart <- as.POSIXct(as.numeric(cfg["sm.group.timestamp_start"]) / 1000)
  tend <- cfg["sm.group.timestamp_end"]
  if (tend == "18446744073709551615") {
    tend <- NA
  }

  tend <-  as.POSIXct(tend / 1000)

  structure(
    list(timestamp_start = tstart,
         timestamp_end = tend),
    class = "group_timestamps",
    tzone = tz,
    tdbsrc = "group_config"
  )
}

#' @export
#' @rdname group_timestamps
group_timestamps.tiledb_config <- function(object, tz = "", ...) {

  if (nchar(tz) == 0) {
    tz <- Sys.timezone()
  }

  if (isFALSE(tz %in% OlsonNames())) {
    cli::cli_abort("{.arg tz} should be valid a timezone. See {.help [{.fun OlsonNames}](base::OlsonNames)} for details.")
  }

  tstart <- as.POSIXct(as.numeric(object["sm.group.timestamp_start"]) / 1000)
  tend <- object["sm.group.timestamp_end"]
  if (tend == "18446744073709551615") {
    tend <- NA
  }

  tend <-  as.POSIXct(tend / 1000)

  structure(
    list(timestamp_start = tstart,
         timestamp_end = tend),
    class = "group_timestamps",
    tzone = tz,
    tdbsrc = "config"
  )
}

#' @export
#' @rdname group_timestamps
group_timestamps.TileDBGroup <- function(object, from = c("ctx", "cfg"),  tz = "", ...) {

  from <- match.arg(from)

  if (from == "ctx") {
    group_timestamps(object$ctx, tz = tz)
  } else {
    group_timestamps(object$object, tz = tz)
  }
}



#' @export
print.group_timestamps <- function(x,...) {

  tdbsrc <-  attr(x, "tdbsrc", exact = TRUE)
  tzx <-  attr(x, "tzone", exact = TRUE)


  if (tdbsrc == "group_config") {
    tdbsrc <- "group config"
  }

  note <- paste0("(",tdbsrc,")")
  tz_txt <- paste0("(",tzx,")")

  ts_char <- vector("character", length = 2)
  ts_char[1] <- if (length(x$timestamp_start) == 0) "origin" else format(x$timestamp_start, "%Y-%m-%d %H:%M:%S", tz = tzx)
  ts_char[2] <- if (length(x$timestamp_end) == 0 | is.na(x$timestamp_end) ) {
     format(Sys.time(), tz = tzx)
    } else {
      format(x$timestamp_end,"%Y-%m-%d %H:%M:%S", tz = tzx)
    }


  txt <- paste0(c("start", "end  "), ": ", cli::col_br_blue(ts_char))
  out <- paste0(" ", cli::col_br_cyan(cli::symbol$bullet), " ", txt, collapse = "\n")

  header1 <- paste0("TileDB Group Timestamps ", cli::col_grey(note), " ")
  header2 <- paste0(" TZ ", cli::col_grey(tz_txt))
  header <- paste0(header1, cli::col_br_red(cli::symbol$bullet), header2)

  cli::cat_line(c(header , out))

  invisible(x)
}
