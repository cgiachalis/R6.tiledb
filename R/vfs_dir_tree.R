.box_chars <- utils::getFromNamespace("box_chars", "fs")
.colourise_fs_path <- utils::getFromNamespace("colourise_fs_path", "fs")
.byte_size_format <- function(size) {

  # standard "IEC" for powers of 1024
  kilo <- size / (1024.0^1)
  mega <- size / (1024.0^2)
  giga <- size / (1024.0^3)
  tera <- size / (1024.0^4)
  peta <- size / (1024.0^5)
  exa  <- size / (1024.0^6)
  eza  <- size / (1024.0^7)

  if (eza >= 1) {
    out <- paste(round(eza, 2), "ZiB", sep = " ")
  } else if (exa >= 1) {
    out <- paste(round(exa, 2), "EiB", sep = " ")
  } else if (peta >= 1){
    out <- paste(round(peta, 2), "PiB", sep = " ")
  } else if (tera >= 1) {
    out <- paste(round(tera, 2), "TiB", sep = " ")
  } else if (giga >= 1) {
    out <- paste(round(giga, 2), "GiB", sep = " ")
  } else if (mega >= 1) {
    out <- paste(round(mega, 2), "MiB", sep = " ")
  } else if (kilo >= 1) {
    out <- paste(round(kilo, 2), "KiB", sep = " ")
  } else{
    out <- paste(round(size, 2), "B", sep = " ")
  }

  out
}

#' Print Directory Contents
#'
#' A modified version of [fs::dir_tree()] to work with TileDB VFS.
#'
#' @param uri URI path for the `TileDB` object.
#' @param recursive Should it recurse fully? Defaults to `TRUE`.
#' @param vfs A [tiledb::tiledb_vfs()] object. If `NULL`
#' (default) will create a new VFS object.
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
#' demo_array_UCBAdmissions(uri)
#'
#' # Array instance
#' arrobj <- tdb_array(uri)
#'
#' # Print directory contents
#' arrobj$dir_tree()
#' }
vfs_dir_tree <- function(uri, recursive = TRUE, vfs = NULL) {

  if (is.null(vfs)) {
    vfs <- tiledb::tiledb_vfs()
  }

  if (recursive) {
    v <- tiledb::tiledb_vfs_ls_recursive(uri, vfs = vfs)$path
  } else {
    v <- tiledb::tiledb_vfs_ls(uri, vfs = vfs)
  }
  files <- vapply_char(v, function(.x) strsplit(.x, ":///")[[1]][[2]], USE.NAMES = FALSE)

  by_dir <- split(files, fs::path_dir(files))

  # non-empty dirs
  subdirs_len <- length(by_dir)
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
