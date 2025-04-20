# For use in read-only R6 active bindings
.emit_read_only_error = function(x) {

   cli::cli_abort(paste0(cli::style_italic("{.val {x}}"), " is a read-only field."), call = NULL)
}

.string_collapse <- function(x, sep = ", ") {
  paste0(x, collapse = sep)
}


vapply_char <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, FUN.VALUE = character(1L), ..., USE.NAMES = USE.NAMES)
}

vapply_lgl <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, FUN.VALUE = logical(1L), ..., USE.NAMES = USE.NAMES)
}

vapply_int <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, FUN.VALUE = integer(1L), ..., USE.NAMES = USE.NAMES)
}


#' Check if a vector is named
#' @noRd
.is_named <- function(x, allow_empty = TRUE) {
  nms <- names(x)
  !is.null(nms) && ifelse(allow_empty, TRUE, all(nzchar(nms)))
}

.is_named_list <- function(x) {
  #  is.list(x) && !(is.null(names(x)) | '' %in% names(x))
  is.list(x) && .is_named(x, FALSE)
}

.is_character_or_null <- function(x) {
  is.character(x) || is.null(x)
}

.has_character_rownames <- function(x) {

  if (isFALSE(is.data.frame(x))) {
    cli::cli_abort("{.emph '{deparse(substitute(x))}' } should be a data.frame, not {.cls {class(x)}}", call = NULL)
  }

  typeof(attr(x, "row.names")) == "character"
}


# Encode and compress an object
# x is an R object
.encode_model <- function(x) {
  qs <- qs::base85_encode(qs::qserialize(x, preset = "high"))
  qs
}

# Decode a compressed string
# x is a string
.decode_model <- function(x) {
  qs <- qs::qdeserialize(qs::base85_decode(x))
  qs
}

# nocov start
.set_log_level <- function(s = "debug") {
  spdl::set_pattern("[%X] [%L] %v");
  spdl::set_level(s)
}
# nocov end

# Modified from tiledb:::tiledb_schema_get_dim_attr_status
.tiledb_schema_get_dim_attr_status <- function(sch) {
  stopifnot(`The 'sch' argument must be a schema` = is(sch,
                                                       "tiledb_array_schema"))
  dom <- tiledb::domain(sch)
  dims <- tiledb::dimensions(dom)
  attrs <- tiledb::attrs(sch)
  return(c(rep("Dim", length(dims)), rep("Attr", length(attrs))))
}

# Modified from tiledb:::tiledb_schema_get_enumeration_status
.tiledb_schema_get_enumeration_status <- function(sch) {
  stopifnot(`The 'sch' argument must be a schema` = is(sch,
                                                       "tiledb_array_schema"))
  dom <- tiledb::domain(sch)
  dims <- tiledb::dimensions(dom)
  attrs <- tiledb::attrs(sch)
  return(c(rep(FALSE, length(dims)), sapply(attrs, tiledb::tiledb_attribute_has_enumeration)))
}
