# source: apis/r/R/utils-uris.R
# https://github.com/single-cell-data/TileDB-SOMA/tree/4d37aa94871d6e8110661370952c3ad88374a7d2/apis/r

# remove leading and trailing slash
# gsub('^/?(.*?)/?$', '\\1', list("file://", "/as"))

# Drop-in replacement for file.paths() that ignores the platform separator when
# constructing remote S3 or TileDB URIs
file_path <- function(..., fsep = .Platform$file.sep) {

  paths <- list(...)

  if (is_remote_uri(paths[[1]])) fsep <- "/"
  file.path(..., fsep = fsep)
}

# TODO: add azure/gcs when supported
#' Checks for remote URI
#' @noRd
is_remote_uri <- function(x) {
  .string_starts_with(x, "s3://") | .string_starts_with(x, "tiledb://")
}

#' Return the scheme of a URI
#' @noRd
uri_scheme <- function(uri) {

  if (isFALSE(.is_scalar_character(uri))) {
    cli::cli_abort("{.arg uri} should be scalar character vector", call = NULL)
  }

  uri_parts <- strsplit(uri, "://")[[1]]
  if (length(uri_parts) == 1) return(NULL)
  uri_parts[[1]]
}

#' Remove the scheme from a URI
#' @noRd
uri_scheme_remove <- function(uri) {

  uri_parts <- strsplit(uri, "://")[[1]]
  if (length(uri_parts) == 1) return(uri)
  uri_parts[[2]]
}

#' Return a URI relative to a parent URI
#' This takes URI schemes into account and errors if they do not match. URIs
#' without a scheme are treated as `file://` URIs.
#' @noRd
make_uri_relative <- function(uri, relative_to) {

  if (isFALSE(.is_scalar_character(uri) && .is_scalar_character(relative_to))) {
    cli::cli_abort("{.arg uri} and {.arg relative_to} should be scalar character vectors", call = NULL)
  }

  uri_in <- uri_scheme(uri)
  relative_to_scheme <- uri_scheme(relative_to)
  if (uri_in %||% "file" != relative_to_scheme %||% "file") {
    cli::cli_abort("Unable to make relative path between URIs with different schemes", call = NULL)
  }

  # Remove schemes from URIs before calculating relative path
  uri <- uri_scheme_remove(uri)
  relative_to <- uri_scheme_remove(relative_to)

  if (!fs::path_has_parent(uri, relative_to)) {
    cli::cli_abort("Unable to make relative path between URIs with no common parent", call = NULL)
    # stop(sprintf("uri %s and relative to %s", uri, relative_to))
  }

  fs::path_rel(path = uri, start = relative_to)
}

