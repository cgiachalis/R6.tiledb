# @importFrom rlang is_scalar_character
NULL
# is_scalar_integerish is_scalar_logical

string_starts_with <- function(x, prefix) {
  prefix <- paste0("^", prefix)
  grepl(prefix, x)
}

is_remote_uri <- function(x) {
  string_starts_with(x, "s3://") | string_starts_with(x, "tiledb://")
}

check_uri <- function(uri) {
  if (isFALSE(rlang::is_scalar_character(uri))) {
    cli::cli_abort(
      "{.emph '{deparse(substitute(uri))}'} should be a character string for URI path",
      call = NULL
    )
  }
}

check_timestamp <- function(x){
  if (!inherits(x, "POSIXct") || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.emph '{deparse(substitute(x))}'} must be a single {.cls POSIXct} datetime object.", call = NULL)
  }
}
