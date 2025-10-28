#' @export
print.tdb_metadata <- function(x,...) {

  n <- length(x)
  footer <- NULL

  if (n > 0) {
    x0 <- x
    if (n > 20L) {
      footer <- sprintf(" ... and %s more metadata", n - 20L)
      x0 <- x[seq_len(20L)]
    }

    .l <- lapply(as.vector(unclass(x0)), function(.i) if(is.character(.i)) sQuote(.i) else .i)
    nms <- paste0(names(x0),": ", .l)

    out <- paste0(" ", cli::col_br_cyan(cli::symbol$bullet), " ", unname(nms), collapse = "\n")

  } else {
    out <- character()
  }

  cls <- cli::col_br_blue(sprintf("<R6 Class: %s>", attr(x, "R6.class")))
  sub1 <- sprintf(paste("TileDB %s:", cls), attr(x, "object_type"))
  sub2 <- paste0("<key,value> ", cli::col_br_red(cli::symbol$bullet), sprintf(" total %i", n))

  cli::cat_line(c(sub1, paste0("Metadata: ", cli::col_grey(sub2)), out))
  cli::cat_line(footer)

  invisible(x)
}

#' @export
`[.tdb_metadata` <- function(x, i) {

  xattrs <- attributes(x)
  out <- unclass(x)[i]
  attr.names <- names(xattrs)
  attr.names <- attr.names[attr.names != 'names']
  attributes(out)[attr.names] <- xattrs[attr.names]
  class(out) <- c("tdb_metadata", "list")
  out
}

#' @export
print.tiledb_timestamp <- function(x, ...) {

  tzx <-  attr(x, "tzone", exact = TRUE)
  tz_txt <- paste0("(",tzx,")")
  note <- attr(x, "ts_info", exact = TRUE)
  note <- paste0("(",note,")")

  ts_char <- vector("character", length = 2)
  ts_char[1] <- if (length(x$timestamp_start) == 0) "origin" else format(x$timestamp_start, "%Y-%m-%d %H:%M:%S", tz = tzx)
  ts_char[2] <- if (length(x$timestamp_end) == 0) format(Sys.time(), tz = tzx) else format(x$timestamp_end,"%Y-%m-%d %H:%M:%S", tz = tzx)

  txt <- paste0(c("start", "end  "), ": ", cli::col_br_blue(ts_char))
  out <- paste0(" ", cli::col_br_cyan(cli::symbol$bullet), " ", txt, collapse = "\n")

  header1 <- paste0("TileDB Timestamp ", cli::col_grey(note), " ")
  header2 <- paste0(" TZ ", cli::col_grey(tz_txt))
  header <- paste0(header1, cli::col_br_red(cli::symbol$bullet), header2)

  cli::cat_line(c(header , out))

  invisible(x)
}

# Format dimensions' non-empty domains: <'dim': [min, max] (data type)>
# Note: datatypes DATETIME_DAY/MS/NS represented in character format
#       date/datetime/nanotime, the latter if the 'nanotime' package is
#       installed, otherwise as int64.
.format_nonempty_domain <- function(x) {
  dimtype <- attr(x, "dimtype")
  switch (dimtype,
    DATETIME_DAY = {
      x <- format(as.Date(as.numeric(x), tz = "UTC"))
    },
    DATETIME_MS = {
      x <- format(as.POSIXct(as.numeric(x), tz = "UTC"))
    },
    DATETIME_NS = {
      if (requireNamespace("nanotime", quietly = TRUE)) {
      x <- format(nanotime::as.nanotime(x), tz = "UTC")
      }
    }
  )
  paste0("[", x[1], ", ", x[2],"]", " ",
         cli::style_italic(cli::style_dim("(", dimtype, ")")))
}


#' @export
print.ifragment <- function(x, ...){

  cli::cat_rule(left = paste0("FRAGMENT", " #", x$fid), col = "blue")
  nms <- c("URI",
           "Type",
           "Non-empty domain",
           "Size",
           "Cell num",
           "Timestamp range",
           "Format version",
           "Has consolidated metadata")
  nms <- paste0(nms, ": ")

  txt <- x[-1]
  tstamp <- cli::col_br_cyan(format(txt$timestamp_range, tz = "UTC", digits= 6, usetz = TRUE))
  txt$timestamp_range <- paste0("[", tstamp[1], ", ", tstamp[2],"]")

  txt$nonemptydom <- "\n"
  ndm_txt <- sapply(x$nonemptydom, .format_nonempty_domain)
  ndm_txt <- paste0("   ", cli::col_br_red(cli::symbol$bullet),
           " ", names(x$nonemptydom),
           ": ",
           ndm_txt, collapse = "\n")

  part1 <- paste0(" ", cli::col_br_black(cli::symbol$pointer), " ", nms[1:3], txt[1:3],
                collapse = "\n")
  part2 <- paste0(" ", cli::col_br_black(cli::symbol$pointer), " ", nms[4:8], txt[4:8],
                  collapse = "\n")
  cli::cat_line(part1, ndm_txt,"\n", part2, "\n")
  invisible(x)
}

#' @export
print.ifragment_list <- function(x, ...){
  dev_null <- lapply(x, function(i) {print(i); NULL})
  invisible(x)
}
