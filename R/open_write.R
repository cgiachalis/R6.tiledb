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
#' then opened again at write mode.
#'
#' `TileDBArray`, `TileDBGroup` or their subclasses will return the underlying
#'  TileDB object (`tiledb_array` or `tiledb_group`) and their timestamps will
#'  differ from the objects in the R6 class as writing at timestamp is not
#'  applicable.
#'
#'
#' @param object An `R` object that contains a `TileDB` resource pointer.
#' @param timestamp Optional datetime object of class `"POSIXct"` to write
#'  at this timestamp.
#' @param ... Other arguments passed to methods. Not used.
#'
#' @returns An object of class `tiledb_array` or `tiledb_group` depending on
#' the method.
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
open_write.TileDBArray <- function(object, timestamp = NULL, ...) {

  if (!object$exists()) {
    cli::cli_abort("R6Class: {.cls {object$class()}} object does not exist.", call = NULL)
  }

  if (object$is_open()) {
    object$close()
  }

  if (is.null(timestamp)) {
   arr <- object$open("WRITE")$object
  } else {
   arr <- tiledb::tiledb_array_open_at(object$object, "WRITE", timestamp = timestamp)
  }

  arr
}


#' @export
#' @rdname open_write
open_write.tiledb_array <- function(object, timestamp = NULL, ...) {


  if (tiledb::tiledb_array_is_open(object)) {
    object <- .tiledb_array_close2(object)
  }

  if (is.null(timestamp)) {
    arr <- tiledb::tiledb_array_open(object, type = "WRITE")
  } else {
    arr <- tiledb::tiledb_array_open_at(object, "WRITE", timestamp = timestamp)
  }

  arr
}

#' @export
#' @rdname open_write
open_write.TileDBGroup <- function(object, timestamp = NULL, ...) {

  if (!object$exists()) {
    cli::cli_abort("R6Class: {.cls {object$class()}} object does not exist.", call = NULL)
  }

  if (object$is_open()) {
    object$close()
  }

  object$open("WRITE")

  if (is.null(timestamp)) {

    grp <- object$object
  } else {

    ctx <- object$ctx
    cfg <- tiledb::config(ctx)
    cfg["sm.group.timestamp_end"] <- .posixt_to_int64char(timestamp)

    grp <- tiledb::tiledb_group(object$uri, type = "WRITE", ctx = ctx, cfg = cfg)

  }

  grp
}

#' @export
#' @rdname open_write
open_write.tiledb_group <- function(object, timestamp = NULL, ...) {

   uri <- tiledb::tiledb_group_uri(object)
   grp <- TileDBGroup$new(uri)
   open_write(grp, timestamp)
}

#' @export
#' @rdname open_write
open_write.character <- function(object, timestamp = NULL, ...) {

  check_uri(object)

  object_type <- tiledb::tiledb_object_type(object)

  if (object_type == "INVALID") {
    cli::cli_abort(c("Invalid TileDB resource.",
                     "i" = "Please check {.arg uri} is a valid path."), call = NULL)
  }

  cstor <- switch(object_type, ARRAY = TileDBArray, GROUP = TileDBGroup)

  obj <- cstor$new(object)
  open_write(obj, timestamp)
}
