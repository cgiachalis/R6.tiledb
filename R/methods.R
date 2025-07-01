#' @export
print.tdb_metadata <- function(x,...) {

  n <- length(x)
  footer <- NULL

  if (n > 0) {
    x0 <- x

    if (n > 20L) {
      footer <- sprintf(" ... and %s more metadata", n -
                          20L)
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
  # see footer in rlang:::print.rlang_envs
  invisible(x)
}
