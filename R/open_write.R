#' @export
open_write <- function(object, ...) {
  UseMethod("open_write")
}


#' Open a TileDB Resource at Write Mode
#'
#' Open a URI, [tiledb::tiledb_array()], [tiledb::tiledb_group()], [TileDBArray]
#' or [TileDBGroup] at write mode with an optional timestamp.
#'
#' Opening a character string should be a valid URI path for a TileDB resource.
#'
#' Objects other than a URI character are implicitly closed if found opened and
#' re-opened at write mode.
#'
#' Note that when using the `timestamp` argument, the `TileDBArray`, `TileDBGroup`
#' and their subclasses will have different timestamps from the returning TileDB
#' object. This is by design, you can not write at timestamp using
#' `TileDBArray` and `TileDBGroup` interface only via `open_write()` method.
#'
#'
#' @param object An `R` object that contains a `TileDB` resource pointer.
#' @param timestamp Optional datetime object of class `"POSIXct"` to write
#'  at this timestamp.
#' @param ctx Optional [tiledb::tiledb_ctx()] object.
#' @param ... Other arguments passed to methods. Not used.
#'
#' @returns An object of class `tiledb_array` or `tiledb_group` depending on
#' the method; the object is opened in `‘WRITE’` mode.
#'
#'
#' @export
#'
#' @name open_write
#'
NULL

#' @export
open_write.default <- function(object, timestamp = NULL, ...) {
  cli::cli_abort("No method for class {.cls {class(object)[1]}}. See {.help [{.fun open_write}](R6.tiledb::open_write)} for details.", call = NULL)
}


#' @export
#' @rdname open_write
open_write.TileDBArray <- function(object, timestamp = NULL, ctx = NULL, ...) {

  if (!object$exists()) {
    cli::cli_abort("R6Class: {.cls {object$class()}} object does not exist.", call = NULL)
  }

  if (is.null(ctx)) {
    ctx <- new_context()
  }

  if (is.null(timestamp)) {
    arr <- tiledb::tiledb_array(object$uri, query_type = "WRITE", ctx = ctx, keep_open = TRUE)
  } else {

    arr <- tiledb::tiledb_array(object$uri, ctx = ctx)
    arr <- tiledb::tiledb_array_open_at(arr, "WRITE", timestamp = timestamp)
  }

  arr
}


#' @export
#' @rdname open_write
open_write.tiledb_array <- function(object, timestamp = NULL, ctx = NULL, ...) {

  if (is.null(ctx)) {
    ctx <- new_context()
  }

  if (is.null(timestamp)) {
    arr <- tiledb::tiledb_array(object@uri, query_type = "WRITE", ctx = ctx, keep_open = TRUE)
  } else {

    arr <- tiledb::tiledb_array(object@uri, ctx = ctx)
    arr <- tiledb::tiledb_array_open_at(arr, "WRITE", timestamp = timestamp)
  }

  arr
}

#' @export
#' @rdname open_write
open_write.TileDBGroup <- function(object, timestamp = NULL, ctx = NULL, ...) {

  if (!object$exists()) {
    cli::cli_abort("R6Class: {.cls {object$class()}} object does not exist.", call = NULL)
  }

  if (is.null(ctx)) {
    ctx <- new_context()
  }

  if (is.null(timestamp)) {
    grp <- tiledb::tiledb_group(object$uri, type = "WRITE", ctx = ctx)

  } else {

    cfg <- tiledb::config(ctx)
    cfg["sm.group.timestamp_end"] <- .posixt_to_int64char(timestamp)

    grp <- tiledb::tiledb_group(object$uri, type = "WRITE", ctx = ctx, cfg = cfg)

  }

  grp
}

#' @export
#' @rdname open_write
open_write.tiledb_group <- function(object, timestamp = NULL, ctx = NULL, ...) {

  if (is.null(ctx)) {
    ctx <- new_context()
  }

  uri <- tiledb::tiledb_group_uri(object)

  if (is.null(timestamp)) {
    grp <- tiledb::tiledb_group(uri, type = "WRITE", ctx = ctx)

  } else {

    cfg <- tiledb::config(ctx)
    cfg["sm.group.timestamp_end"] <- .posixt_to_int64char(timestamp)

    grp <- tiledb::tiledb_group(uri, type = "WRITE", ctx = ctx, cfg = cfg)

  }

  grp
}

#' @export
#' @rdname open_write
open_write.character <- function(object, timestamp = NULL, ctx = NULL, ...) {

  check_uri(object)

  if (is.null(ctx)) {
    ctx <- new_context()
  }

  object_type <- tiledb::tiledb_object_type(object, ctx = ctx)

  if (object_type == "INVALID") {
    cli::cli_abort(c("Invalid TileDB resource.",
                     "i" = "Please check {.arg uri} is a valid path."), call = NULL)
  }

  if (object_type == "ARRAY") {

    if (is.null(timestamp)) {
      arr <- tiledb::tiledb_array(object, query_type = "WRITE", ctx = ctx, keep_open = TRUE)
    } else {

      arr <- tiledb::tiledb_array(object, ctx = ctx)
      arr <- tiledb::tiledb_array_open_at(arr, "WRITE", timestamp = timestamp)
    }

  } else {

    if (is.null(timestamp)) {
      obj <- tiledb::tiledb_group(object, type = "WRITE", ctx = ctx)

    } else {

      cfg <- tiledb::config(ctx)
      cfg["sm.group.timestamp_end"] <- .posixt_to_int64char(timestamp)

      obj <- tiledb::tiledb_group(object, type = "WRITE", ctx = ctx, cfg = cfg)

      obj
    }
  }

}
