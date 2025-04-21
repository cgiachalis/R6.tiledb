#' @title  TileDB Array Base Class
#'
#' @description Base class for representing an individual TileDB array.
#'
#' @keywords internal
#'
#' @export
TileDBArray <- R6::R6Class(
  classname = "TileDBArray",
  inherit = TileDBObject,
  public = list(

    #' @description Open TileDB array object for read or write.
    #'
    #' This methods opens the underlying [`tiledb::tiledb_array()`] object in the
    #' requested mode if it is different from the current mode.
    #'
    #' @param mode Mode to open : either `"READ"` or `"WRITE"`.  Default is `"READ"`.
    #' @param internal_use  A character value that gives access to internal method `new()`.
    #' The `new()` method should not be called directly.
    #'
    #' @return The object, invisibly.
    #'
    open = function(mode = c("READ", "WRITE"), internal_use = NULL) {

      private$check_internal_use(internal_use, method = "open()")
      private$check_object_exists()
      mode <- match.arg(mode)

      if (is.null(private$.tiledb_array)) {
        spdl::debug("[TileDBArray$open] Opening with initialisation {} '{}'", self$class(), self$uri, mode)
        private$initialize_object()
      }

      init_mode <- self$mode()
      private$.mode <- mode

      identical_mode <- init_mode == mode

      spdl::debug("[TileDBArray$open] Requested open mode is {}", ifelse(identical_mode, "identical, no mode switch", "not identical, switch mode"))

      if (isFALSE(identical_mode)) {


        if (tiledb::tiledb_array_is_open(private$.tiledb_array)) {

          spdl::debug("[TileDBArray$open] Closing {} '{}' to switch mode to {} from {}", self$class(), self$uri, mode, init_mode)

          tiledb::tiledb_array_close(self$object)
        }

        spdl::debug("[TileDBArray$open] Opening {} '{}' in {} mode", self$class(), self$uri, mode)

        private$.tiledb_array <- tiledb::tiledb_array_open(self$object, type = mode)

        private$update_metadata_cache()
      }

      invisible(self)
    },

    #' @description Close the object.
    #'
    #' @return The object, invisibly.
    #'
    close = function() {

      spdl::debug("[TileDBArray$close] Closing {} '{}'", self$class(), self$uri)

      tiledb::tiledb_array_close(self$object)
      private$.mode = "CLOSED"
      invisible(self)
    },

    #' @description Return a [tiledb_array()] object.
    #'
    #' If a `query_type` not provided then it will be inherited from class
    #' mode; in case the class mode is `"CLOSED"`, then the query type
    #' defaults to `"READ"`.
    #'
    #' @param ... Optional arguments to pass to `tiledb::tiledb_array()`
    #'
    #' @return A [tiledb_array()] object.
    #'
    tiledb_array = function(...) {

      private$check_object_exists()

      args <- list(...)
      args$uri <- self$uri
      # user has not supplied 'query_type'
      if(is.null(args$query_type)) {
        mode <- self$mode()
        args$query_type <- ifelse(mode == "CLOSED", "READ", mode)
      }
      args$query_layout <- "UNORDERED"
      args$ctx <- self$ctx

      spdl::debug("[TileDBArray$tiledb_array] for uri '{}' mode '{}' layout '{}'", args$uri, args$query_type, args$query_layout)

      do.call(tiledb::tiledb_array, args)
    },

    #' @description Retrieve metadata from the TileDB array.
    #'
    #' The `TileDBArray` will be opened for `"READ"` to fetch its metadata.
    #'
    #' @param key The name of the metadata attribute to retrieve.
    #'   The default `NULL` returns all metadata.
    #'
    #' @return The key metadata value or a `list` of all metadata
    #'  values when `NULL`.
    #'
    get_metadata = function(key = NULL) {

      if (!self$is_open()) {
        self$open(mode = "READ", internal_use = "permit")
      }

      spdl::debug("[TileDBArray$get_metadata] Retrieving metadata for {} '{}'", self$class(), self$uri)

       private$fill_metadata_cache_if_null()
      if (!is.null(key)) {
        val <- private$.metadata_cache[[key]]
        if (is.list(val)) val <- unlist(val)
        val
      } else {
        private$.metadata_cache
      }
    },

    #' @description Add list of metadata to the specified TileDB array.
    #'
    #' `TileDBArray` should be open for `"WRITE"`.
    #'
    #' @param metadata Named list of metadata to add.
    #'
    #' @return `NULL` value, invisibly.
    #'
    set_metadata = function(metadata) {

      private$check_object_exists()
      private$check_metadata(metadata)
      private$check_open_for_write()

      spdl::debug("Writing metadata to {} '{}'", self$class(), self$uri)

      nms <- names(metadata)
      dev_null <- mapply(
        FUN = tiledb::tiledb_put_metadata,
        key = nms,
        val = metadata,
        MoreArgs = list(arr = self$object),
        SIMPLIFY = FALSE)

      dev_null <- mapply(
        FUN = private$add_cached_metadata,
        key = nms,
        val = metadata,
        SIMPLIFY = FALSE)

      invisible(NULL)

    },

    #' @description Retrieve the array schema as TileDB schema.
    #'
    #' @return A [`tiledb::tiledb_array_schema`] object.
    #'
    schema = function() {

      private$check_object_exists()

      tiledb::schema(self$object)
    },
    #' @description Retrieve schema information.
    #'
    #' @return A `data.frame` object with four column schema
    #'  information: `names`, `types`, `status` and `enum`.
    #'
    schema_info = function() {
      sch <- self$schema()
      data.frame(
        names = tiledb::tiledb_schema_get_names(sch),
        types = tiledb::tiledb_schema_get_types(sch),
        status = .tiledb_schema_get_dim_attr_status(sch),
        enum = .tiledb_schema_get_enumeration_status(sch)
      )
    },

    #' @description Retrieve the array dimensions
    #'
    #' @return A named list of [`tiledb::tiledb_dim`] objects.
    #'
    dimensions = function() {
      dims <- tiledb::dimensions(self$schema())
      setNames(dims, nm = vapply_char(dims, tiledb::name))
    },


    #' @description Retrieve the array attributes
    #'
    #' @return A list of [`tiledb::tiledb_attr`] objects.
    #'
    attributes = function() {
      tiledb::attrs(self$schema())
    },

    #' @description Retrieve dimension names
    #'
    #' @return A character vector with the array's dimension names.
    #'
    dimnames = function() {
      names(self$dimensions())
    },

    #' @description Retrieve attribute names
    #'
    #' @return A character vector with the array's attribute names.
    #'
    attrnames = function() {
      vapply_char(self$attributes(), FUN = tiledb::name, USE.NAMES = FALSE)
    },

    #' @description Retrieve the names of all columns, including dimensions and
    #' attributes
    #'
    #' @return A character vector with the array's column names.
    #'
    colnames = function() {

      c(self$dimnames(), self$attrnames())

    },

    #' @description Retrieve names of index (dimension) columns
    #'
    #' @return A character vector with the array index (dimension) names.
    #'
    index_column_names = function() {
      self$dimnames()
    },

    #' @description Print summary of the array.
    #'
    print = function() {

      if (self$exists()) {

        cli::cli_div(theme = list(.val = list(color = "cyan"),
                                  .emph = list(color = "orange")))

        dims <- cli::cli_vec(self$dimnames(), list("vec-trunc" = 3))
        attrs <- cli::cli_vec(self$attrnames(), list("vec-trunc" = 3))

        olid <- cli::cli_ol()
        ulid <- cli::cli_ul()
        cli::cli_inform("R6Class: {.cls {self$class()}}")
        cli::cli_bullets(c(">" = "URI Basename: {.emph {basename(self$uri)}}"))
        ulid <- cli::cli_ul()
        cli::cli_li(c("Dimensions: {.val {dims}}",
                      "Attributes: {.val {attrs}}"))
        cli::cli_end(olid)
        cli::cli_end(ulid)
      } else {
        cli::cli_inform(c("i" = "R6Class: {.cls {self$class()}} object does not exist."))
      }

      invisible(self)
    }
  ),

  active = list(
    #' @field object Access the underlying [`tiledb::tiledb_array()`] object. When
    #' used before open() method, the underlying array will be initialised at `"READ"`
    #' mode and kept open.
    object = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("object")
      }
      # If the array was created after the object was instantiated, we need to
      # initialize private$.tiledb_array
      if (is.null(private$.tiledb_array)) {
        private$initialize_object()
      }
      private$.tiledb_array
    }
  ),

  private = list(

    # Internal pointer to the TileDB array.
    #
    # Important implementation note:
    # * In TileDB-R there is an unopened handle obtained by tiledb::tiledb_array, which takes
    #   a URI as its argument.
    # * One may then open and close this using tiledb::tiledb_array_open (for read or write)
    #   and tiledb::tiledb_array_close, which take a tiledb_array handle as their first argument.
    #
    # However, for groups:
    # * tiledb::tiledb_group and tiledb::group_open both return an object opened for read or write.
    # * Therefore for groups we cannot imitate the behaviour for arrays.
    #
    # For this reason there is a limit to how much handle-abstraction we can do in the TileDBObject
    # parent class. In particular, we cannot have a single .tiledb_object shared by both TileDBArray
    # and TileDBGroup.
    .tiledb_array = NULL,

    # Initially NULL, once the array is created or opened, this is populated
    # with a list that's empty or contains the array metadata. Since the spec
    # requires that we allow readback of array metadata even when the array
    # is open for write, but the TileDB layer underneath us does not, we must
    # have this cache.
    .metadata_cache = NULL,

    # Once the array has been created this initializes the TileDB array object
    # and stores the reference in private$.tiledb_array.
    initialize_object = function() {

      private$.tiledb_array <- tiledb::tiledb_array(
        uri = self$uri,
        ctx = self$ctx,
        query_layout = "UNORDERED",
        keep_open = TRUE)
    },

    # ----------------------------------------------------------------
    # Metadata-caching

    fill_metadata_cache_if_null = function() {

      if (is.null(private$.metadata_cache)) {

        private$update_metadata_cache()

      }
    },

    update_metadata_cache = function() {

      spdl::debug("[TileDBArray$update_metadata_cache] updating metadata cache for {} '{}' in {}", self$class(), self$uri, private$.mode)

      # See notes above -- at the TileDB implementation level, we cannot read array metadata
      # while the array is open for read, but at the SOMA application level we must support
      # this. Therefore if the array is opened for write and there is no cache populated then
      # we must open a temporary handle for read, to fill the cache.

      if (is.null(private$.tiledb_array)) {
       array_handle <- private$initialize_object()
      } else {
        array_handle <- private$.tiledb_array
      }

      if (private$.mode == "WRITE") {

       spdl::debug("[TileDBArray$update_metadata_cache] getting object")

       array_object <- tiledb::tiledb_array(self$uri, ctx = private$.tiledb_ctx)

       array_handle <- tiledb::tiledb_array_open(array_object, type = "READ")
      }

      if (isFALSE(tiledb::tiledb_array_is_open(array_handle))) {

       spdl::debug("[TileDBArray$update_metadata_cache] reopening object")

       array_handle <- tiledb::tiledb_array_open(array_handle, type = "READ")

      }

      private$.metadata_cache <- tiledb::tiledb_get_all_metadata(array_handle)


      if (private$.mode == "WRITE") {
       tiledb::tiledb_array_close(array_handle)
      }

      invisible(NULL)
    },

    add_cached_metadata = function(key, value) {

      if (is.null(private$.metadata_cache)) {

        private$.metadata_cache <- list()

      }

      private$.metadata_cache[[key]] <- value
    },
    # Check schema names
    # nms: character vector with input names
    # sch: target schema object
    check_schema_names = function(nms, sch) {

      dims <- vapply_char(tiledb::dimensions(sch), tiledb::name)
      attrs <- vapply_char(tiledb::attrs(sch), tiledb::name)

      sch_names <- c(dims, unname(attrs))

      .idx <- sch_names %in% nms

      if (!all(.idx)) {

        not_found <- cli::cli_vec(sch_names[!.idx], list("vec-trunc" = 3))
        cli::cli_abort("Schema names missing or misspecified: {.val {not_found}}", call = NULL)
      }
    }

  )
)
