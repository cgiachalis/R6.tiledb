
check_uri <- function(x) {
  if (isFALSE(.is_scalar_character(x))) {
    cli::cli_abort(
      "{.arg {deparse(substitute(x))}} should be a character string for URI path.",
      call = NULL
    )
  }
}

check_timestamp <- function(x){
  if (!inherits(x, "POSIXct") || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {deparse(substitute(x))}} should be a single {.cls POSIXct} datetime object.", call = NULL)
  }
}

check_timestamp_posixt <- function(x){
  if (!inherits(x, "POSIXt") || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {deparse(substitute(x))}} should be a single {.cls POSIXt} datetime object.", call = NULL)
  }
}

check_tiledb_config <- function(x) {
  if (!inherits(x, "tiledb_config")) {
    cli::cli_abort("{.arg {deparse(substitute(x))}} should be of class {.help [{.fun tiledb_config}](tiledb::tiledb_config)}.", call = NULL)
  }
}

check_tiledb_ctx <- function(x) {
  if (!inherits(x, what = 'tiledb_ctx')) {
    cli::cli_abort("{.arg {deparse(substitute(x))}} should be a {.help [{.fun tiledb_ctx}](tiledb::tiledb_ctx)} object.", call = NULL)
  }
}
