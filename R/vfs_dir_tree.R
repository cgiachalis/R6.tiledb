.box_chars <- utils::getFromNamespace("box_chars", "fs")
.colourise_fs_path <- utils::getFromNamespace("colourise_fs_path", "fs")
.byte_size_format <- function(size){

  # standard "IEC" for powers of 1024

  k <- size / (1024.0^1)
  m <- size / (1024.0^2)
  g <- size / (1024.0^3)
  t <- size / (1024.0^4)
  p <- size / (1024.0^5)

  if (p > 1) {
    out <- paste(round(t,2),"PiB", sep = " ")
  } else if (t > 1){
    out <- paste(round(t,2),"TiB", sep = " ")
  } else if (g > 1) {
    out <- paste(round(g,2),"GiB", sep = " ")
  } else if (m > 1) {
    out <- paste(round(m,2),"MiB", sep = " ")
  } else if (k > 1) {
    out <- paste(round(k,2),"KiB", sep = " ")
  } else{
    out <- paste(round(size,2),"B", sep = " ")
  }

  out
}


#' Print directory contents
#'
#' @param uri URI path for the `TileDB` object.
#' @param vfs A [tiledb::tiledb_vfs()] object. Defaults to
#' `TileDB` VFS object from the `tiledb` environment and cache.
#'
#' @returns A character vector with file paths, invisibly.
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
#' # Array instance
#' arrobj <- tdb_array(uri)
#'
#' # Print directory contents
#' arrobj$dir_tree()
#' }
vfs_dir_tree <- function(uri, vfs = tiledb::tiledb_get_vfs()) {
  v <- tiledb::tiledb_vfs_ls_recursive(uri, vfs = vfs)
  files <-  sapply(v$path, function(.x) strsplit(.x, ":///")[[1]][[2]])

  by_dir <- split(files, fs::path_dir(files))

  # non-empty dirs, sans root dir
  subdirs_len <- length(by_dir) - 1
  dir_size <- tiledb::tiledb_vfs_dir_size(uri, vfs = vfs)
  dir_size <- .byte_size_format(dir_size)

  pc <- function(...) {
    paste0(..., collapse = "")
  }

  ch <- .box_chars()

  get_coloured_name <- function(x) {
    sub(x, fs::path_file(x), .colourise_fs_path(x), fixed = TRUE)
  }

  print_leaf <- function(x, indent) {
    leafs <- by_dir[[x]]
    for (i in seq_along(leafs)) {
      if (i == length(leafs)) {
        cat(indent, pc(ch$l, ch$h, ch$h, " "), get_coloured_name(leafs[[i]]),
            "\n", sep = "")
        print_leaf(leafs[[i]], paste0(indent, "    "))
      }
      else {
        cat(indent, pc(ch$j, ch$h, ch$h, " "), get_coloured_name(leafs[[i]]),
            "\n", sep = "")
        print_leaf(leafs[[i]], paste0(indent, pc(ch$v,"   ")))
      }
    }
  }

  path <- names(by_dir[1])
  cat(.colourise_fs_path(path), "\n", sep = "")
  print_leaf(path, "")

  cat("\n")
  dd <- paste0("directories ", cli::col_grey(paste0("(",subdirs_len,")")), " ")
  ss <- paste0(" total size ", cli::col_grey(paste0("(", dir_size,")")), " ")
  dinfo <- paste0(dd, cli::col_br_red(cli::symbol$bullet), ss)
  cli::cat_bullet(dinfo, bullet = "pointer", bullet_col = "blue")
  invisible(files)

}
