# nocov start
.onLoad <- function(libname, pkgname) {

}

.onUnload <- function(libname, pkgname) {

  # reset "r6.tiledb" daemon connection on unloading
  if (requireNamespace("mirai", quietly = TRUE)) {
    if (mirai::daemons_set("r6.tiledb")) {
     mirai::daemons(0L, .compute = "r6.tiledb")
    }
  }
}

# nocov end
