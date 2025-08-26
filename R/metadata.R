#' @export
#' @usage metadata(x, which)
#' @rdname metadata
metadata <- function(x, which) {
  UseMethod("metadata")
}

#' @export
#' @usage metadata(x, which) <- value
#' @rdname metadata
`metadata<-` <- function(x, which, value) {
  UseMethod('metadata<-', x)
}


#' TileDB Object Metadata
#'
#' Get or set a metadata key for a `TileDB` array or group.
#'
#' These functions work similar to [attr()] and provide access to a
#' single metadata key on an TileDB object. The replacement form (setter)
#' puts a metadata key with the value specified or creates a new metadata
#' with the value given.
#'
#' The character method is intended for a valid URI path.
#'
#' The methods will not alter the mode of the `TileDB` object.
#'
#'  For the extractor the object will be opened temporarily to access the
#'  metadata if it is closed.
#'
#' For the replacement, the object will be opened in write mode temporarily
#' and on exit will revert back to previous mode; in the case where the object
#' is already in write mode then it will be reopened at the same mode in order
#' to flush the metadata on disk.
#'
#' @param x An `R` object that points to a `TileDB` resource whose
#'  metadata are to accessed.
#' @param which A non-empty character string specifying which metadata key
#' is to be accessed.
#' @param value An object, the new value of the metadata, or `NULL` to remove
#' the key. Note that character vectors should be of length one (scalar).
#'
#' @returns For the extractor, the key value of the metadata matched, or `NULL`
#' if no exact match is found.
#'
#' @export
#'
#' @name metadata
#'
NULL


# Getters -----------------------------------------------------------

#' @export
metadata.default <- function(x, which) {
  cli::cli_abort("No method for class {.cls {class(x)[1]}}.
                 See {.help [{.fun metadata}](R6.tiledb::metadata)} for details.",
                 call = NULL)
}

#' @export
#' @rdname metadata
metadata.TileDBArray <- function(x, which) {

  if (isFALSE(.is_scalar_character(which))) {
    cli::cli_abort("{.arg {deparse(substitute(which))}} should be a single character string.", call = NULL)
  }

  mode <- x$mode

  if (mode == "CLOSED") {
    on.exit({
      x$close()
    })
  }

  # 1 length non-empty
  x$get_metadata(which)

}

#' @export
#' @rdname metadata
metadata.TileDBGroup <- metadata.TileDBArray


#' @export
#' @rdname metadata
metadata.tiledb_array <- function(x, which) {

  if (isFALSE(.is_scalar_character(which))) {
    cli::cli_abort("{.arg {deparse(substitute(which))}} should be a single character string.", call = NULL)
  }

  uri <- x@uri

  obj <- TileDBArray$new(uri)

  metadata(obj, which)

}

#' @export
#' @rdname metadata
metadata.tiledb_group <- function(x, which) {

  if (isFALSE(.is_scalar_character(which))) {
    cli::cli_abort("{.arg {deparse(substitute(which))}} should be a single character string.", call = NULL)
  }

  uri <- tiledb::tiledb_group_uri(x)

  obj <- TileDBGroup$new(uri)

  metadata(obj, which)

}

#' @export
#' @rdname metadata
metadata.character <- function(x, which) {

  if (isFALSE(.is_scalar_character(which))) {
    cli::cli_abort("{.arg {deparse(substitute(which))}} should be a single character string.", call = NULL)
  }

  check_uri(x)

  object_type <- tiledb::tiledb_object_type(x)

  if (object_type == "INVALID") {
    cli::cli_abort(c("Invalid TileDB resource.",
                     "i" = "Please check {.arg uri} is a valid path."), call = NULL)
  }

  cstor <- switch(object_type, ARRAY = TileDBArray, GROUP = TileDBGroup)

  obj <- cstor$new(x)

  metadata(obj, which)
}

# Setters -----------------------------------------------------------

#' @export
#' @rdname metadata
`metadata<-.TileDBArray` <- function(x, which, value) {

  if (isFALSE(.is_scalar_character(which))) {
    cli::cli_abort("{.arg {deparse(substitute(which))}} should be a single character string.", call = NULL)
  }

  if (isFALSE(.is_scalar(value, typeof(value)) | is.null(value))) {
    cli::cli_abort("Replacement value: {.arg {deparse(substitute(value))}} should be a scalar or NULL.", call = NULL)
  }

  mode <- x$mode

  if (mode == "CLOSED") {

    x$open("WRITE")

    on.exit({x$close()})

  } else if (mode == "READ") {

    x$reopen("WRITE")
    on.exit({x$reopen(mode)})
  } else {

    on.exit({x$reopen(mode)})
  }

  if (is.null(value)) {
    bool <- tiledb::tiledb_delete_metadata(x$object, which)

    return(x)

  } else {
    meta <- list(value)
    names(meta) <- which

    x$set_metadata(meta)

  }
}

#' @export
#' @rdname metadata
`metadata<-.TileDBGroup` <- function(x, which, value) {

  if (isFALSE(.is_scalar_character(which))) {
    cli::cli_abort("{.arg {deparse(substitute(which))}} should be a single character string.", call = NULL)
  }

  if (isFALSE(.is_scalar(value, typeof(value)) | is.null(value))) {
    cli::cli_abort("Replacement value: {.arg {deparse(substitute(value))}} should be a scalar or NULL.", call = NULL)
  }

  mode <- x$mode

  if (mode == "CLOSED") {

    x$open("WRITE")

    on.exit({x$close()})

  } else if (mode == "READ") {

    x$reopen("WRITE")
    on.exit({x$reopen(mode)})
  } else {

    on.exit({x$reopen(mode)})
  }

  if (is.null(value)) {
    bool <- tiledb::tiledb_group_delete_metadata(x$object, which)

    return(x)

  } else {
    meta <- list(value)
    names(meta) <- which

    x$set_metadata(meta)
  }
}

#' @export
#' @rdname metadata
`metadata<-.tiledb_array` <- function(x, which, value) {

  if (isFALSE(.is_scalar_character(which))) {
    cli::cli_abort("{.arg {deparse(substitute(which))}} should be a single character string.", call = NULL)
  }

  if (isFALSE(.is_scalar(value, typeof(value)) | is.null(value))) {
    cli::cli_abort("Replacement value: {.arg {deparse(substitute(value))}} should be a scalar or NULL.", call = NULL)
  }

  uri <- x@uri

  obj <- TileDBArray$new(uri)

  metadata(obj, which) <- value

}

#' @export
#' @rdname metadata
`metadata<-.tiledb_group` <- function(x, which, value) {

  if (isFALSE(.is_scalar_character(which))) {
    cli::cli_abort("{.arg {deparse(substitute(which))}} should be a single character string.", call = NULL)
  }

  if (isFALSE(.is_scalar(value, typeof(value)) | is.null(value))) {
    cli::cli_abort("Replacement value: {.arg {deparse(substitute(value))}} should be a scalar or NULL.", call = NULL)
  }

  uri <- tiledb::tiledb_group_uri(x)

  obj <- TileDBGroup$new(uri)

  metadata(obj, which) <- value
}


#' @export
#' @rdname metadata
`metadata<-.character` <- function(x, which, value) {

  check_uri(x)

  if (isFALSE(.is_scalar_character(which))) {
    cli::cli_abort("{.arg {deparse(substitute(which))}} should be a single character string.", call = NULL)
  }

  if (isFALSE(.is_scalar(value, typeof(value)) | is.null(value))) {
    cli::cli_abort("Replacement value: {.arg {deparse(substitute(value))}} should be a scalar or NULL.", call = NULL)
  }

  object_type <- tiledb::tiledb_object_type(x)

  if (object_type == "INVALID") {
    cli::cli_abort(c("Invalid TileDB resource.",
                     "i" = "Please check {.arg uri} is a valid path."), call = NULL)
  }

  cstor <- switch(object_type, ARRAY = TileDBArray, GROUP = TileDBGroup)

  obj <- cstor$new(x)

  metadata(obj, which) <- value

  return(x)
}
