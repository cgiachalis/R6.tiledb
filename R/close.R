
#' Close a TileDB Resource
#'
#' Close a [tiledb::tiledb_array()], [tiledb::tiledb_group()], [TileDBArray]
#' or [TileDBGroup] object.
#'
#'
#' @param con An `R` object that contains a `TileDB` resource pointer.
#' @param ... Other arguments passed to methods. Not used.
#'
#' @returns Invisibly, a logical `TRUE` on success.
#'
#' @name close
#'
NULL

#' @export
#' @rdname close
close.tiledb_array <- function(con, ...) {
  .tiledb_array_close2(con)
  invisible(TRUE)
}


#' @export
#' @rdname close
close.TileDBArray <- function(con, ...) {
  con$close()
  invisible(TRUE)
}

#' @export
#' @rdname close
close.tiledb_group <- function(con, ...) {
  tiledb::tiledb_group_close(con)
  invisible(TRUE)
}

#' @export
#' @rdname close
close.TileDBGroup <- function(con, ...) {
  con$close()
  invisible(TRUE)
}
