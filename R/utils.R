
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
  .is_character(x) || is.null(x)
}

.is_logical_or_null <- function(x) {
  .is_logical(x) || is.null(x)
}

.is_scalar <- function(x, type) {
  (typeof(x) == type) && is.atomic(x) && length(x) == 1L
}

.is_character <- function(x) {
  typeof(x) == "character"
}

.is_scalar_character <- function(x) {

  .is_scalar(x, "character")
}

.is_scalar_logical <- function(x) {

  .is_scalar(x, "logical")
}

.is_scalar_double <- function(x) {

  .is_scalar(x, "double")
}


.posixt_to_int64char <- function(x) {
  check_timestamp_posixt(x)
  as.character(bit64::as.integer64(as.numeric(x) * 1000))
}

.systime_to_int64char <- function() {
  as.character(bit64::as.integer64(as.numeric(Sys.time()) * 1000))
}

# nocov start
.set_log_level <- function(s = "debug") {
  spdl::set_pattern("[%X] [%L] %v");
  spdl::set_level(s)
}

# @param ctx TileDB Context
# @param ts a tiledb_timestamp object
.set_group_timestamps <- function(ctx, ts) {

  cfg <- tiledb::config(ctx)
  tstart <- ts$timestamp_start
  tend <- ts$timestamp_end

  if (length(tstart) > 0) {
    cfg["sm.group.timestamp_start"] <- .posixt_to_int64char(tstart)
  } else {
    cfg["sm.group.timestamp_start"] <- "0"
  }

  if (length(tend) > 0) {
    cfg["sm.group.timestamp_end"] <- .posixt_to_int64char(tend)
  } else {
    cfg["sm.group.timestamp_end"] <- "18446744073709551615"
  }

  tiledb::tiledb_ctx(cfg)
}


# nocov end

# Modified from tiledb:::tiledb_schema_get_dim_attr_status
# 1-> Dim, 2 -> Attr
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
  nms <- vapply_char(dims, tiledb::name)
  dim_enum <- rep(FALSE, length(dims))
  names(dim_enum) <- nms

  attrs <- tiledb::attrs(sch)
  c(dim_enum, vapply_lgl(attrs, tiledb::tiledb_attribute_has_enumeration))
}

#' Create array for `UCBAdmissions` dataset
#'
#' @param uri The uri path.
#'
#' @export
demo_UCBAdmissions_array <- function(uri) {

  idx_cols <- c("Dept", "Gender")
  df <- as.data.frame(datasets::UCBAdmissions)

  tiledb::fromDataFrame(df[1:8, ], uri, col_index = idx_cols, sparse = TRUE)

  arr <- tiledb::tiledb_array(uri)
  arr[] <- df[9:16, ]
  arr[] <- df[17:24, ]

  invisible(TRUE)

}
