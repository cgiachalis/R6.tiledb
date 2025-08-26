#' Replacement method
#'
#' See [metadata()] documentation.
#'
#' @export
#' @keywords internal
`metadata<-` <- function(x, which, value) {
  UseMethod("metadata<-", x)
}

#' @export
metadata <- function(x, which) {
  UseMethod("metadata")
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
#' For the extractor the object will be opened temporarily to access the
#' metadata if it is closed.
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
#' @seealso For a list of metadata and time-travelling use [set_metadata()] and
#'  [fetch_metadata()].
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

  cstor <- switch(object_type, ARRAY = TileDBArray, GROUP = TileDBGroup, {
    cli::cli_abort(c("Invalid TileDB resource.", "i" = "Please check {.arg uri} is a valid path."),  call = NULL)
  })

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

  cstor <- switch(object_type, ARRAY = TileDBArray, GROUP = TileDBGroup, {
    cli::cli_abort(c("Invalid TileDB resource.", "i" = "Please check {.arg uri} is a valid path."),  call = NULL)
  })

  obj <- cstor$new(x)

  metadata(obj, which) <- value

  return(x)
}

# * Set_metadata ----

#' Set TileDB Metadata
#'
#' Set metadata as a list of key-value pairs for a `TileDB` array or group.
#' Optionally, you can write metadata at specific point in time
#' (time-travelling).
#'
#' This functions work similar to [`metadata<-()`] but works with
#' a list of key value pairs and always returns a list.
#'
#' The optional argument `timestamp` can be used to set metadata at specific
#' point in time.
#'
#' The character method is intended for a valid URI path.
#'
#' The methods will not alter the mode of the `TileDB` object.
#'
#' The object will be opened in write mode temporarily and on exit will
#' revert back to previous mode; in the case where the object is already
#' in write mode then it will be reopened at the same mode in order
#' to flush the metadata on disk.
#'
#' @param x An `R` object that points to a `TileDB` resource whose
#'  metadata are to written.
#' @param keys A named list of key value metadata.
#' @inheritParams open_write
#'
#' @returns A named list of class `tdb_metadata`.
#'
#' @seealso [fetch_metadata()] and [metadata()].
#'
#' @export
#'
#' @name set_metadata
#'
NULL

#' @export
set_metadata <- function(x, keys, timestamp) {
  UseMethod("set_metadata")
}


#' @export
set_metadata.default <- function(x, keys, timestamp = NULL) {
  cli::cli_abort("No method for class {.cls {class(x)[1]}}.
                 See {.help [{.fun set_metadata}](R6.tiledb::set_metadata)} for details.",
                 call = NULL)
}

#' @export
#' @rdname set_metadata
set_metadata.TileDBArray <- function(x, keys, timestamp = NULL) {

  mode <- x$mode

  if (is.null(timestamp)) {

    if (mode == "CLOSED") {

      x$open("WRITE")

      on.exit({x$close()})

    } else if (mode == "READ") {

      x$reopen("WRITE")
      on.exit({x$reopen(mode)})
    } else {

      on.exit({x$reopen(mode)})
    }

    x$set_metadata(keys)

  } else {

    check_timestamp_posixt(timestamp)

    if (x$object_type == "ARRAY") {
      .put_metadata <- function(obj, key, val) {
        tiledb::tiledb_put_metadata(obj, key, val)
      }

    } else if (x$object_type == "GROUP") {
      .put_metadata <- function(obj, key, val) {
        tiledb::tiledb_group_put_metadata(obj, key, val)
      }
    }


    if (mode == "CLOSE") {
      on.exit({x$close()})
    } else {
      on.exit({x$reopen(mode)})
    }

    obj <- open_write(x, timestamp = timestamp)

    dev_null <-  mapply(
      key = names(keys),
      val = keys,
      MoreArgs = list(obj = obj),
      FUN = .put_metadata
    )

    close(obj)

  }
}


#' @export
#' @rdname set_metadata
set_metadata.TileDBGroup <- set_metadata.TileDBArray


#' @export
#' @rdname set_metadata
set_metadata.tiledb_array <- function(x, keys, timestamp = NULL) {

  uri <- x@uri
  obj <- TileDBArray$new(uri)

  set_metadata(obj, keys, timestamp)

}

#' @export
#' @rdname set_metadata
set_metadata.tiledb_group <- function(x, keys, timestamp = NULL) {

  uri <- x@uri
  obj <- TileDBGroup$new(uri)

  set_metadata(obj, keys, timestamp)

}

#' @export
#' @rdname set_metadata
set_metadata.character <- function(x, keys, timestamp = NULL) {

  check_uri(x)

  object_type <- tiledb::tiledb_object_type(x)

  cstor <- switch(object_type, ARRAY = TileDBArray, GROUP = TileDBGroup, {
    cli::cli_abort(c("Invalid TileDB resource.", "i" = "Please check {.arg uri} is a valid path."),  call = NULL)
  })

  obj <- cstor$new(x)

  set_metadata(obj, keys, timestamp)

}


# * fetch_metadata ----

#' Fetch TileDB Metadata
#'
#' Fetch metadata as a list of key-value pairs for a `TileDB` array or group.
#' Optionally, you can access metadata at specific point in time
#' (time-travelling).
#'
#' This functions work similar to [metadata()] but works with
#' a list of key value pairs and always returns a list.
#'
#' The character method is intended for a valid URI path.
#'
#' The methods will not alter the mode of the `TileDB` object; also, the object
#'  will be opened temporarily to access the metadata if it is closed.
#'
#' @param x An `R` object that points to a `TileDB` resource whose
#'  metadata are to accessed.
#' @inheritParams set_metadata
#'
#' @returns A named list of class `tdb_metadata`.
#'
#' @seealso [set_metadata()] and [metadata()].
#'
#' @export
#'
#' @name fetch_metadata
#'
NULL

#' @export
fetch_metadata <- function(x, keys, timestamp) {
  UseMethod("fetch_metadata")
}

#' @export
fetch_metadata.default <- function(x, keys, timestamp = NULL) {
  cli::cli_abort("No method for class {.cls {class(x)[1]}}.
                 See {.help [{.fun fetch_metadata}](R6.tiledb::fetch_metadata)} for details.",
                 call = NULL)
}


#' @export
#' @rdname fetch_metadata
fetch_metadata.TileDBArray <- function(x, keys, timestamp = NULL) {



  mode <- x$mode

  if (is.null(timestamp)) {
    if (mode == "CLOSED") {
      on.exit({ x$close() })
    }
    x$get_metadata(keys)
  } else {

    check_timestamp_posixt(timestamp)

    if (mode == "WRITE") {
      on.exit({x$reopen(mode)})
      x$reopen("READ")
    }

    x$timestamp <- timestamp
    x$get_metadata(keys)
  }
}


#' @export
#' @rdname fetch_metadata
fetch_metadata.TileDBGroup <- fetch_metadata.TileDBArray


#' @export
#' @rdname fetch_metadata
fetch_metadata.tiledb_array <- function(x, keys, timestamp = NULL) {

  uri <- x@uri
  obj <- TileDBArray$new(uri)

  fetch_metadata(obj, keys, timestamp)

}


#' @export
#' @rdname fetch_metadata
fetch_metadata.tiledb_group <- function(x, keys, timestamp = NULL) {

  uri <- x@uri
  obj <- TileDBGroup$new(uri)

  fetch_metadata(obj, keys, timestamp)

}


#' @export
#' @rdname fetch_metadata
fetch_metadata.character <- function(x, keys, timestamp = NULL) {

  check_uri(x)

  object_type <- tiledb::tiledb_object_type(x)

  cstor <- switch(object_type, ARRAY = TileDBArray, GROUP = TileDBGroup, {
    cli::cli_abort(c("Invalid TileDB resource.", "i" = "Please check {.arg uri} is a valid path."),  call = NULL)
  })

  obj <- cstor$new(x)

  fetch_metadata(obj, keys, timestamp)

}

