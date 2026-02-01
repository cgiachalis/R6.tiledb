# nocov start
#' Packages Version Information
#'
#' Print version information for \pkg{tiledb} (R package), \pkg{R6.tiledb},
#' \pkg{R6} and `TileDB` embedded.
#'
#' @export
#'
#' @keywords internal
#'
#' @importFrom utils packageVersion
r6_tiledb_info <- function() {
  cat(
    " R6.tiledb:     ", toString(utils::packageVersion("R6.tiledb")), "\n",
    " tiledb-r:      ", toString(utils::packageVersion("tiledb")), "\n",
    " tiledb core:   ", as.character(tiledb::tiledb_version(compact = TRUE)), "\n",
    " R6:            ", toString(utils::packageVersion("R6")), "\n",
    " R:             ", R.version.string, "\n",
    " OS:            ", utils::osVersion, "\n",
    sep = ""
  )
}

# nocov end
