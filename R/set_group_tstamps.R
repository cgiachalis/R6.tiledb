#' Modify Group Start/End Open Timestamps
#'
#' Set or unset group opening start, end timestamps to the given
#' configuration object.
#'
#' @param cfg A configuration object [tiledb::tiledb_config()].
#' @param start_time,end_time Optional time stamp values. A date time objects
#' of class `POSIXt`.
#'
#' @returns The modified `tiledb_config` object.
#'
#' @seealso [set_config_params()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- tiledb::tiledb_config()
#' cfg <- set_group_tstamps(cfg, as.POSIXct(1), as.POSIXct(1000))
#' cfg["sm.group.timestamp_start"] # 1000 (ms)
#'
#' # reset
#' cfg <- unset_group_tstamps(cfg)
#' cfg["sm.group.timestamp_start"] # 0
#'}
#' @name set_group_tstamps
set_group_tstamps <- function(cfg, start_time = NULL, end_time = NULL) {

  check_tiledb_config(cfg)

  params <- NULL

  if (!is.null(start_time)) {
    start_time <- .posixt_to_int64char(start_time)
    params <- c("sm.group.timestamp_start" = start_time)
  }

  if (!is.null(end_time)) {
    end_time <- .posixt_to_int64char(end_time)

    if (is.null(start_time)) {
      start_time <- cfg["sm.group.timestamp_start"]
    }

    # ops will convert char to nums
    if (start_time > end_time) {
      cli::cli_abort("{.arg start_time} is greater than {.arg end_time}.", call = NULL)
    }

    params <- c(params, "sm.group.timestamp_end" = end_time)

  }

  if (!is.null(params)) {
    cfg <- set_config_params(cfg, params)
  }

  cfg

}

#' @export
#' @rdname set_group_tstamps
unset_group_tstamps <- function(cfg) {

  params <- c("sm.group.timestamp_start","sm.group.timestamp_end")
  cfg <- unset_config_params(cfg, params)

  cfg
}

