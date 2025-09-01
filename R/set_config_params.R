#' Modify Config Parameters
#'
#' These functions will set or unset the parameters of TileDB Config object
#' and return the modified configuration object.
#'
#' @param cfg A configuration object [tiledb::tiledb_config()].
#' @param keyval A named character vector with configuration parameters.
#' @param keys A character with configuration keys to unset.
#'
#' @returns The modified `tiledb_config` object.
#'
#' @seealso [tiledb::tiledb_config()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' origparams <- c("sm.consolidation.timestamp_start" = "100",
#'                 "sm.consolidation.timestamp_end" = "20000")
#'
#' # Create a context with a config setting the params above
#' cfg <- tiledb::tiledb_config(origparams)
#' ctx <- tiledb::tiledb_ctx(config = cfg)
#'
#' # Do something with ctx
#'
#' # Get configs from context
#' cfg <- tiledb::config(ctx)
#'
#' # New params
#' params <-  c("sm.consolidation.timestamp_start" = "100",
#'              "sm.consolidation.timestamp_end" = "10000")
#'
#' cfg <- set_config_params(cfg, params)
#'
#' cfg["sm.consolidation.timestamp_end"] # "10000"
#'
#' # Update context with modified config object
#' ctx <- tiledb::tiledb_ctx(config = cfg)
#'
#'
#' # Reset consolidation timestamps
#' cfg <- unset_config_params(cfg, names(params))
#'
#' # check the default values
#' cfg["sm.consolidation.timestamp_start"] # "0"
#' cfg["sm.consolidation.timestamp_end"]   # "18446744073709551615"
#'
#' # Update context with modified config object
#' ctx <- tiledb::tiledb_ctx(config = cfg)
#'
#'}
#' @name set_config_params
set_config_params <- function(cfg, keyval) {

  check_tiledb_config(cfg)

  if (isFALSE(.is_named(keyval, allow_empty = FALSE))) {
    cli::cli_abort("{.arg {deparse(substitute(keyval))}} should be a named vector.", call = NULL)
  }

  if (isFALSE(.is_character(keyval))) {
    cli::cli_abort("{.arg {deparse(substitute(keyval))}} should be a character vector.", call = NULL)
  }

  keys <- names(keyval)

  for (i in seq_along(keys)) {
    ptr <- .libtiledb_config_set(cfg@ptr, keys[i], keyval[i])
  }

  new("tiledb_config", ptr = ptr)

}

#' @export
#' @rdname set_config_params
unset_config_params <- function(cfg, keys) {

  check_tiledb_config(cfg)

  if (isFALSE(.is_character(keys))) {
    cli::cli_abort(".arg {deparse(substitute(keys))}} should be a character vector.", call = NULL)
  }

  for (i in seq_along(keys)) {
    ptr <- .libtiledb_config_unset(cfg@ptr, keys[i])
  }

  new("tiledb_config", ptr = ptr)

}
