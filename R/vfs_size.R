#' VFS Directory Size
#'
#' @param uri URI path for the `TileDB` object.
#' @param vfs A [tiledb::tiledb_vfs()] object. Defaults to
#' `TileDB` VFS object from the `tiledb` environment and cache.
#'
#' @returns The size of the directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # URI path
#' uri <- tempfile()
#'
#' # Demo array
#' demo_UCBAdmissions_array(uri)
#'
#' # Directory size
#' vfs_size(uri)
#' }
vfs_size <- function(uri, vfs = NULL) {

  if (is.null(vfs)) {
    vfs <- tiledb::tiledb_vfs()
  }

  s <- tiledb::tiledb_vfs_dir_size(uri, vfs = vfs)

  class(s) <- "vfs_size"

  s

}
