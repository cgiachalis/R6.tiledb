
.onLoad <- function(libname, pkgname) {

  opt <- getOption("R6.tiledb.internal")

  if (is.null(opt)) {
    options(R6.tiledb.internal = "permit")
  }
  invisible()
}
