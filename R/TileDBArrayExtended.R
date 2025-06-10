# TODO:
#  4. is_sparse
#  5. consolidate / vacuum_async (using mirai)

# 11. non_empty_domain ? use  tiledb_array_get_non_empty_domain_from_index and dimnames
# 13. add_attribute (with or without enum)
# 14. drop_attribute
# 15. rename_attribute
# 16. create method
# 17. consolidate/vacuum metadata
# cfg["sm.consolidation.mode"] <- "array_meta"
# cfg["sm.vacuum.mode"] <- "array_meta"

# TODO DOCUMENT
tdb_array <- function(uri,
                      ctx = NULL,
                      tiledb_timestamp = NULL){
  obj <- TileDBArrayExtended$new(uri,
                                 ctx = ctx,
                                 tiledb_timestamp = tiledb_timestamp)
  obj$open("READ")
}

#' @title Generate a `TileDBArrayExtended` Object
#'
#' @description
#' This class inherits from [TileDBArrayExtended] and represents `TileDB` Array
#' with enhanced functionality.
#'
#' ## Initialization
#' A new `TileDBArrayExtended` instance is initialized using the `new()` method.
#' Alternatively use [tdb_array()] to create an instance and open the array at
#' `READ` mode.
#'
#' ```r
#'  # uri path
#'  uri <- tempdir()
#'  # new instance
#'  obj <- TileDBArrayExtended$new(uri = uri)
#'  # does array exist at this uri
#'  obj$exists() # FALSE
#'
#'  unlink(uri)
#' ```
#' @returns An object of class `TileDBArrayExtented`.
#'
#' @export
TileDBArrayExtended <- R6::R6Class(
  classname = "TileDBArrayExtended",
  inherit = TileDBArray,
  public = list(
    #' @description Create a new `TileDBArrayExtended` instance.
    #'
    #' @param uri URI path for the `TileDB` Array.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object.
    #' @param tiledb_timestamp Optional Datetime (POSIXct) with TileDB timestamp.
    #'
    initialize = function(uri,
                          ctx = NULL,
                          tiledb_timestamp = NULL) {

      super$initialize(uri = uri,
                       ctx = ctx,
                       tiledb_timestamp = tiledb_timestamp,
                       internal_use = "permit")

    },
    #' @description Retrieve factor columns (attributes).
    #'
    #' @return A character vector with factor columns (enum attributes).
    #'
    enum_columns = function() {

      idx <- vapply_lgl(self$attributes(), tiledb::tiledb_attribute_has_enumeration)
      names(idx[idx])

    },
    #' @description Retrieve factor levels for a given attribute.
    #'
    #' @param x An attribute name.
    #'
    #' @return A character vector with levels (enum values).
    #'
    enum_levels = function(x) {

      if (isFALSE(x %in% self$attrnames())) {
        cli::cli_abort( "{.emph '{deparse(substitute(x))}'} is not attribute.", call = NULL)}

      .attrib <- self$attributes()[[x]]

      if (isFALSE(tiledb::tiledb_attribute_has_enumeration(.attrib))) {
        cli::cli_abort("{.emph '{deparse(substitute(x))}'} attribute is not factor (enum).", call = NULL)
      }

      enum <- tiledb::tiledb_attribute_get_enumeration(attr = .attrib, arr = self$object)

      enum

    },
    #' @description Checks array for factor columns.
    #'
    #'
    #' @return A boolean. `TRUE` indicating the array has factor columns and
    #' `FALSE` otherwise.
    #'
    has_enums = function(){
      idx <- vapply_lgl(self$attributes(), tiledb::tiledb_attribute_has_enumeration)
      any(idx)
    },
    # TODO: add key https://github.com/TileDB-Inc/TileDB-Py/blob/28714d9b25d44d6c6c1f318525184d3784b7de00/tiledb/array.py#L729
    # TODO: ctx? add example..
    # TODO: ADD LIST OF CONFIGUREATION PARAMETERS
    #' @description Consolidates the fragments of the array into
    #'  a single fragment.
    #'
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the consolidation.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXlt`. If not provided, the default values from configuration
    #' object will be used.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object. By default, object's context
    #'  is used.
    #' @return `NULL`, invisibly.
    #'
    consolidate = function(cfg = NULL, start_time, end_time, ctx = NULL) {

      if (is.null(ctx)) {
        ctx <- self$ctx
      }

      tiledb::array_consolidate(self$uri,
                                cfg = cfg,
                                start_time,
                                end_time,
                                ctx = ctx)
    },
    #' @description Clean up consolidated fragments and array metadata.
    #'
    #' @param cfg A configuration object [tiledb::tiledb_config()] to set parameters
    #'  for the vacuum process.
    #' @param start_time,end_time Optional time stamp values. A date time objects
    #' of class `POSIXlt`. If not provided, the default values from configuration
    #' object will be used.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object. By default, object's context
    #'  is used.
    #'
    #' @return `NULL`, invisibly.
    #'
    vacuum = function(cfg = NULL, start_time, end_time, ctx = NULL){

      if (is.null(ctx)) {
        ctx <- self$ctx
      }

      tiledb::array_vacuum(self$uri,
                           cfg = cfg,
                           start_time,
                           end_time,
                           ctx = ctx)
    },
    #' @description Returns a `data.frame` with consolidated fragments to be
    #' removed.
    #'
    #' @param trunc_uri `TRUE` to return the truncated uri path of fragments.
    #'
    finfo_to_vacuum = function(trunc_uri = TRUE) {

      private$.fragments_object$to_vacuum(trunc_uri)
    },
    #' @description Dump to console the commit fragments.
    #'
    finfo_dump = function() {
     private$.fragments_object$dump()
    },
    #' @description Fragments uri and time stamps.
    #' @param trunc_uri `TRUE` to truncate uri path.
    finfo_uris = function(trunc_uri = TRUE){
      private$.fragments_object$finfo_uris(trunc_uri)

    },
    #' @description Upgrade the array to the latest format version.
    #'
    #' @param cfg A configuration object [tiledb::tiledb_config()].
    #' @param ctx Optional [tiledb::tiledb_ctx()] object. By default, object's context
    #'  is used.
    schema_upgrade = function(cfg = NULL, ctx = NULL) {
      if (is.null(ctx)) {
        ctx <- self$ctx
      }
      tiledb::tiledb_array_upgrade_version(self$object, config = cfg, ctx = self$ctx)
    }
  ),

  active = list(
    #' @field fragments_object Access the [TileDBFragments] instance for this
    #' array.
    #'
    fragments_object = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("fragment_object")
      }
      if (is.null(private$.fragments_object)) {
        private$.fragments_object <- TileDBFragments$new(self$uri, ctx = self$ctx)
      }
      private$.fragments_object

    },
    #' @field schema_version Retrieve the schema version for this array.
    #'
    schema_version = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("schema_version")
      }
      tiledb::tiledb_array_schema_version(self$schema())
    }

  ),

  private = list(

    # Contains TileDBFragments object
    .fragments_object = NULL
  )
)
