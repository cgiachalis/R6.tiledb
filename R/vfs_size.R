#' VFS Directory Size
#'
#' @inheritParams vfs_dir_tree
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
