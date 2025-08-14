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

  s <- sprintf("TileDB %s: <R6 Class: %s>", attr(x, "object_type"), attr(x, "R6.class"))
  cli::cat_line(c(s, "Metadata: <key,value>", out))
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


  ts_char <- vector("character", length = 2)
  ts_char[1] <- if (length(x$timestamp_start) == 0) "origin" else format(x$timestamp_start, tz = "UTC")
  ts_char[2] <- if (length(x$timestamp_end) == 0) format(Sys.time(), tz = "UTC") else format(x$timestamp_end, tz = "UTC")

  txt <- paste0(c("start", "end  "), ": ", cli::col_br_blue(ts_char))
  out <- paste0(" ", cli::col_br_cyan(cli::symbol$bullet), " ", txt, collapse = "\n")
  cli::cat_line(c("TileDB Timestamp", out))
  invisible(x)
}
