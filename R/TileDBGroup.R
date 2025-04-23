#' @title TileDB Group Base Class
#'
#' @description Base class for interacting with TileDB groups
#'
#' @returns An object `TileDBGroup` of class `R6`.
#'
#' @export
TileDBGroup <- R6::R6Class(
  classname = "TileDBGroup",
  inherit = TileDBObject,

  public = list(

    #' @description Create a TileDB Group object given the class URI path.
    #'
    #' @param mode Mode to open : either `"READ"` or `"WRITE"` (default).
    #'
    #' @return The object, invisibly.
    #'
    create = function(mode = "WRITE") {

      spdl::debug("[TileDBGroup$create] Creating new {} in '{}' at ({})",
                  self$class(),
                  self$uri,
                  self$tiledb_timestamp %||% "now")

      tiledb::tiledb_group_create(self$uri, ctx = private$.tiledb_ctx)

      mode <- match.arg(mode, choices = c("READ", "WRITE"))

      private$.tiledb_group <- tiledb::tiledb_group(self$uri, type = mode)
      private$.mode <- mode
      private$.object_type <- "GROUP"

      invisible(self)
    },

    #' @description Open TileDB group object for read or write.
    #'
    #' @param mode Mode to open : either `"READ"` or `"WRITE"`.  Default is `"READ"`.
    #'
    #' @return The object, invisibly.
    #'
    open = function(mode = c("READ", "WRITE")) {

      private$check_object_exists()
      mode <- match.arg(mode)

      if (is.null(private$.tiledb_group)) {
        spdl::debug("[TileDBGroup$open] Opening with initialisation {} '{}'", self$class(), self$uri, mode)
        private$initialize_object()
      }

      init_mode <- self$mode()
      private$.mode <- mode

      identical_mode <- init_mode == mode

      spdl::debug("[TileDBGroup$open] Requested open mode is {}", ifelse(identical_mode, "identical, no mode switch", "not identical, switch mode"))

      if (isFALSE(identical_mode)) {

        if (tiledb::tiledb_group_is_open(private$.tiledb_group)) {

          spdl::debug("[TileDBGroup$open] Closing {} '{}' to switch mode to {} from {}", self$class(), self$uri, mode, init_mode)

          tiledb::tiledb_group_close(self$object)
        }

        spdl::debug("Opening {} '{}' in {} mode", self$class(), self$uri, mode)

        private$.tiledb_group <- tiledb::tiledb_group_open(self$object, type = mode)

        private$update_member_cache()
        private$update_metadata_cache()

      }

      invisible(self)
    },

    #' @description Close the object.
    #'
    #' @return The object, invisibly.
    #'
    close = function() {

      if (self$is_open()) {

        # Close all members before closing group
        for (member in private$.member_cache) {

          if (!is.null(member$object)) {

            if (member$object$is_open()) {

              member$object$close()

            }
          }
        }

        spdl::debug("[TileDBGroup$close] Closing {} '{}'", self$class(), self$uri)

        tiledb::tiledb_group_close(private$.tiledb_group)

        private$.mode <- "CLOSED"

      }

      invisible(self)
    },

    #' @description Remove member.
    #'
    #' @param name Name of the member to remove.
    #'
    #' @return `NULL` value, invisibly.
    #'
    remove = function(name) {

      private$check_object_exists()
      private$check_scalar_character(name)
      private$check_open_for_write()

      spdl::debug("[TileDBGroup$remove] Removing '{}' member from {} '{}'", name, self$class(), self$uri)

       tiledb::tiledb_group_remove_member(private$.tiledb_group, uri = name)

      # Drop member if cache has been initialized
      if (is.list(private$.member_cache)) {
        private$.member_cache[[name]] <- NULL
      }

      invisible(NULL)
    },

    #' @description Count the number of members in the group.
    #'
    #' @return The number of members in the group.
    #'
    count_members = function() {

      private$check_object_exists()
      private$check_open_for_read_or_write()
      private$fill_member_cache_if_null()
      length(private$.member_cache)
    },

    #' @description List the members of the group.
    #'
    #' @param type Select type member, either`"ALL"`, `"GROUP"`
    #'  or `"ARRAY"`. By default all member types are listed.
    #'
    #' @return A `data.frame` with columns `uri`, `type`, and `name`.
    #'
    get_members_df = function(type = c("ALL", "GROUP", "ARRAY")) {

      private$check_object_exists()

      type <- match.arg(type)

      member_list <- self$to_list()

      count <- length(member_list)

      df <- data.frame(
        name = character(count),
        uri  = character(count),
        type = character(count)
      )

      df$type <- vapply_char(member_list, FUN = getElement, name = 1L)
      df$uri <- vapply_char(member_list, FUN = getElement, name = 2L)
      df$name <- vapply_char(member_list, FUN = getElement, name = 3L)

      if (type != "ALL") {
       df <- df[df$type %in% type, , drop = FALSE]
      }
      df
    },

    #' @description Retrieve a group member by name. If the member isn't already
    #' open, it is opened in the same mode as the parent.
    #'
    #' @param name The name of the member.
    #'
    #' @return A `TileDBArray` or `TileDBGroup`.
    #'
    get_member = function(name) {

      private$check_object_exists()
      private$check_scalar_character(name)
      private$check_open_for_read_or_write()
      private$fill_member_cache_if_null()

      member <- private$.member_cache[[name]]

      if (is.null(member)) {
        cli::cli_abort("No member named {.emph '{deparse(substitute(name))}'} found", call = NULL)
      }

      # Instantiate member object (i.e ARRAY, GROUP etc.)
      #
      #   - That needs to get the _actual object which is opened for write_.
      # So here if the object (maybe opened for read or write) was stored,
      # we return it. But if not (e.g. first access on read from storage)
      # then we invoke the appropriate constructor. Note: child classes
      # may override construct_member.
      #
      obj <- if (is.null(member$object)) {
       spdl::debug("[TileDBGroup$get_member] construct member {} type {}", member$uri, member$type)
        obj <- private$construct_member(member$uri, member$type)
      } else {
        member$object
      }

      spdl::debug("[TileDBGroup$get_member] open check, mode {}", self$mode())
      if (!obj$is_open()) {
        switch(
          EXPR = (mode <- self$mode()),
          READ = obj$open(mode),
          WRITE = obj$reopen(mode)
        )
      }

      # Explicitly add the new member to member_cache, see comments on
      # private method
      private$add_cached_member(name, obj)

      obj
    },

    #' @description Add new member to the group.
    #'
    #' @param object A `TileDBArray` or `TileDBGroup` object to add.
    #' @param name Name to use for the member. By default the base name of
    #' the object's URI is used.
    #' @param relative An optional logical value indicating whether the new
    #' object's URI is relative to the group's URI. If `NULL` (the
    #' default), the object's URI is assumed to be relative unless it is a
    #' `tiledb://` URI.
    #'
    #' @return `NULL` value, invisibly.
    #'
    set_member = function(object, name = NULL, relative = NULL) {

      private$check_object_exists()
      private$check_open_for_write()

      if ( isFALSE(inherits(object, "TileDBGroup") || inherits(object, "TileDBArray")) ) {
        cli::cli_abort(
          "{.emph '{deparse(substitute(object))}'} should be either
          {.cls TileDBArray} or {.cls TileDBGroup} object, not {.cls {class(object)}}.",
          call = NULL)
      }

      # sanitise NA
      if (isTRUE(is.na(name))) name <- NULL

      if ( isFALSE(is.null(name) || rlang::is_scalar_character(name)) ) {
        cli::cli_abort(
          "{.emph '{deparse(substitute(name))}'} argument should be a character name or NULL, not
          {.cls {class(name)}}.",
          call = NULL
        )
      }

      # sanitise NA
      if (isTRUE(is.na(relative))) relative <- NULL

      if (isFALSE(is.null(relative) || rlang::is_scalar_logical(relative)) ) {
        cli::cli_abort(
          "{.emph '{deparse(substitute(relative))}'} argument should be a logical or NULL value, not
          {.cls {class(relative)}}.",
          call = NULL)
      }

      if (is.null(relative)) {
        relative <- !startsWith(object$uri, "tiledb://")
      }

      # Because object$uri will always return an absolute URI, we need to
      # make it relative to the collection's URI before adding it
      if (relative) {
        uri <- make_uri_relative(object$uri, self$uri)
      } else {
        uri <- object$uri
      }

      # if NULL default to URI's basename
      name <- name %||% basename(uri)

      tiledb::tiledb_group_add_member(
        grp = private$.tiledb_group,
        uri = uri,
        relative = relative,
        name = name
      )

      private$add_cached_member(name, object)

      invisible(NULL)
   },

   #' @description Retrieve metadata from the TileDB group.
   #'
   #' The `TileDBGroup` will be opened for `"READ"` to fetch its metadata.
   #'
   #' @param key The name of the metadata attribute to retrieve.
   #' The default `NULL` returns all metadata.
   #'
   #' @return The key metadata value or a `list` of all metadata
   #'  values when `NULL`.
   #'
   get_metadata = function(key = NULL) {

     if (!self$is_open()) {
       self$open(mode = "READ")
     }

     private$fill_metadata_cache_if_null()

     spdl::debug("Retrieving metadata for {} '{}'", self$class(), self$uri)

     if (!is.null(key)) {
       private$.metadata_cache[[key]]
     } else {
       private$.metadata_cache
     }
   },

   #' @description Add list of metadata.
   #'
   #' `TileDBGroup` should be open for `"WRITE"`.
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
       FUN = tiledb::tiledb_group_put_metadata,
       key = nms,
       val = metadata,
       MoreArgs = list(grp = private$.tiledb_group),
       SIMPLIFY = FALSE)

     dev_null <- mapply(
       FUN = private$add_cached_metadata,
       key = nms,
       val = metadata,
       SIMPLIFY = FALSE )

     invisible(NULL)
   },

    #' @description Retrieve the members' names.
    #'
    #' @return A `character` vector of member names.
    #'
    names = function() {

      private$check_object_exists()
      private$check_open_for_read_or_write()
      private$fill_member_cache_if_null()

      names(private$.member_cache) %||% character(length = 0L)
    },

    #' @description Retrieve a `list` of members.
    #'
    #' @return A `list`.
    #'
    to_list = function() {

      private$check_object_exists()

      if (!self$is_open()) {
        self$open(mode = "READ")
        on.exit(self$close())
      }

      private$fill_member_cache_if_null()

      private$.member_cache
    },

   #' @description Check if a member exists.
   #'
   #' @param name Name of the member to check.
   #'
   #' @return A logical value.
   #'
    member_exists = function(name) {

      private$check_scalar_character(name)

      members <- names(self$to_list())

      name %in% members

    },

   #' @description Print summary of the group.
   #'
   print = function() {

     if (self$exists()) {

       members <- self$get_members_df()

       if (nrow(members) > 0) {

         sp <- split(members, members$type)

         cli::cli_div(theme = list(.val = list(color = "cyan"),
                                   .emph = list(color = "orange")))

         arrs <- sp$ARRAY$name %||% ''
         grps <- sp$GROUP$name %||% ''

         arrs <- cli::cli_vec(arrs, list("vec-trunc" = 3))
         grps <- cli::cli_vec(grps, list("vec-trunc" = 3))

         olid <- cli::cli_ol()
         ulid <- cli::cli_ul()
         cli::cli_inform("R6Class: {.cls {self$class()}}")
         cli::cli_bullets(c(">" = "URI Basename: {.emph {basename(self$uri)}}"))
         ulid <- cli::cli_ul()
         cli::cli_li(c("Arrays: {.val {arrs}}",
                       "Groups: {.val {grps}}"))
         cli::cli_end(olid)
         cli::cli_end(ulid)

       } else {
         cli::cli_alert_info("R6Class: {.cls {self$class()}} is empty.")
       }
     } else {
       cli::cli_inform(c("i" = "R6Class: {.cls {self$class()}} object does not exist."))
     }

     invisible(self)
   },
   #' @description Dump the TileDB Group structure to string.
   #'
   #' @param recursive  Should the nested uris be returned recursively?
   #'  Default is `TRUE`.
   #'
   #' @return A `character` string.
   #'
   dump = function(recursive = TRUE) {

     private$check_object_exists()
     private$check_open_for_read_or_write()

     uri <- self$uri
     bsname <- basename(self$uri)
     xuri <- self$get_members_df()
     xuri$bsn <- basename(xuri$uri)
     cli::cat_line()
     cli::cat_rule("Model Store Directory")
     cli::cat_line()
     dmp <- tiledb::tiledb_group_member_dump(private$.tiledb_group, recursive = recursive)
     dmp_raw <- dmp

     dmp <- gsub(bsname, paste(cli::col_grey("->"),
                               cli::col_cyan(bsname)), dmp)
     dmp <- gsub("GROUP", cli::col_red("GROUP"), dmp)
     dmp <- gsub("ARRAY", cli::col_br_blue("ARRAY"), dmp)
     dev_null <- lapply(xuri$bsn, \(.x) {dmp <<- gsub(.x, cli::col_yellow(.x), dmp); invisible(NULL)})
     cat(dmp)
     invisible(dmp_raw)
   }
  ),

  active = list(
    #' @field object Access the underlying [`tiledb::tiledb_group()`] object.
    object = function(value) {
      if (!missing(value)) {
        .emit_read_only_error("object")
      }
      # If the group was created after the object was instantiated, we need to
      # initialize private$.tiledb_group
      if (is.null(private$.tiledb_group)) {
        private$initialize_object()
      }
      private$.tiledb_group
    }
  ),

  private = list(

    # @description This is a handle at the TileDB-R level
    #
    # Important implementation note:
    # * In TileDB-R there is an unopened handle obtained by tiledb::tiledb_array, which takes
    #   a URI as its argument.
    # * One may then open and close this using tiledb::tiledb_array_open (for read or write)
    #   and tiledb::tiledb_array_close, which take a tiledb_array handle as their first argument.
    #
    # However, for groups:
    # * tiledb::tiledb_group and tiledb::group_open both return an object opened for read or write.
    # * Therefore for groups we cannot imitate the behavior for arrays.
    #
    # For this reason there is a limit to how much handle-abstraction we can do in the TileDBObject
    # parent class. In particular, we cannot have a single .tiledb_object shared by both TileDBArray
    # and TileDBGroup.
    .tiledb_group = NULL,

    # This field stores the timestamp with which we opened the group, whether we used the
    # opener-supplied self$tiledb_timestamp, or defaulted to the time of opening, or neither
    # (NULL). This is the timestamp to propagate to accessed members.
    .group_open_timestamp = NULL,

    # @description List of cached group members
    # Initially NULL, once the group is created or opened, this is populated
    # with a list that's empty or contains the group members.
    .member_cache = NULL,

    # Initially NULL, once the group is created or opened, this is populated
    # with a list that's empty or contains the group metadata.
    .metadata_cache = NULL,

    # Once the group has been created this initializes the TileDB group object
    # and stores the reference in private$.tiledb_group.
    initialize_object = function() {

      private$.tiledb_group <- tiledb::tiledb_group(self$uri, ctx = self$ctx)
      private$.mode <- "READ"

    },
    # Instantiate a group member object.
    # Responsible for calling the appropriate R6 class constructor.
    construct_member = function(uri, type) {

      check_uri(uri)

      private$check_scalar_character(type)

      spdl::debug("[TileDBGroup$construct_member] entered, uri {} type {}", uri, type)

      constructor <- switch(type,
        ARRAY = TileDBArray$new,
        GROUP = TileDBGroup$new,
        cli::cli_abort("Unknown member type: {.emph '{deparse(substitute(type))}'.}", call = NULL))

      obj <- constructor(uri, ctx = self$ctx,
                         tiledb_timestamp = private$.group_open_timestamp,
                         internal_use = "permit")
      obj
    },

    # ----------------------------------------------------------------
    # Important implementation note about caching:
    #
    # The caching layer is not solely a performance-enhancer. It's a necessary part of the
    # implementation.
    #
    # At the SOMA application level, the SOMA spec requires that we allow users to read array schema
    # and metadata even when the array is opened for write, and it requires that we allow users to
    # read group members and metadata even when the group is opened for write.
    #
    # At the TileDB implementation level, for arrays, we can read the array schema but not the array
    # metadata if the array is opened for write. For groups in TileDB-R, we can read the group
    # member list or the group metadata if the group is opened for read but we cannot access them
    # when the group is opened for write.

    # ----------------------------------------------------------------
    # Member caching

    # @description Retrieve all group members.
    # @return A list indexed by group member names where each element is a
    # list with names: name, uri, and type.
    get_all_members_uncached_read = function(group_handle) {

      count <- tiledb::tiledb_group_member_count(group_handle)
      if (count == 0) return(list())

      members <- vector(mode = "list", length = count)

      for (i in seq_len(count)) {
        members[[i]] <- setNames(
          object = as.list(tiledb::tiledb_group_member(group_handle, i - 1L)),
          nm = c("type", "uri", "name")
        )
      }

      # Key the list by group member name
      names(members) <- vapply_char(members, FUN = getElement, name = "name")

      members
    },

    fill_member_cache_if_null = function() {
      if (is.null(private$.member_cache)) {
        private$update_member_cache()
      }
    },

    update_member_cache = function() {

      spdl::debug("Updating member cache for {} '{}'", self$class(), self$uri)

      # See notes above -- at the TileDB implementation level, we cannot read anything about the
      # group while the group is open for read, but at the SOMA application level we must support
      # this. Therefore if the group is opened for write and there is no cache populated then
      # we must open a temporary handle for read, to fill the cache.
      group_handle <- private$.tiledb_group

      if (private$.mode == "WRITE") {

        # FIXME: when we add write timestamps we should open this temp handle with tiledb_timestamp
        # too. The stopifnot is currently "unreachable" since open() stops if called with WRITE
        # mode and non-null tiledb_timestamp.

        stopifnot("FIXME" = is.null(private$.group_open_timestamp))

        group_handle <- tiledb::tiledb_group(self$uri, type = "READ", ctx = private$.tiledb_ctx)
      }

      members <- private$get_all_members_uncached_read(group_handle)

      if (is.null(private$.member_cache)) {

        private$.member_cache <- members

      } else {

        # Don't clobber existing cache members, in order to retain original URIs
        members <- members[setdiff(names(members), names(private$.member_cache))]
        private$.member_cache <- utils::modifyList(private$.member_cache, members)

      }

      if (private$.mode == "WRITE") {
        tiledb::tiledb_group_close(group_handle)
      }
    },

    add_cached_member = function(name, object) {

      # We explicitly add the new member to member_cache in order to preserve the
      # original URI. Otherwise TileDB Cloud creation URIs are retrieved from
      # using tiledb_group_member() in the form tiledb://namespace/uuid. In this
      # form it's not possible to append new children, which is necessary during
      # ingestion.

      if (is.null(private$.member_cache)) {
        private$.member_cache <- list()
      }

      private$.member_cache[[name]] <- list(
        type = object$object_type,
        uri = object$uri,
        name = name,
        object = object
      )

      # We still need to update member_cache to pick up existing members.
      # Otherwise if you open a group with existing members and add a new
      # member, the initially empty member_cache will only contain the new
      # member.
      private$update_member_cache()
    },

    # ----------------------------------------------------------------
    # Metadata-caching

    fill_metadata_cache_if_null = function() {
      if (is.null(private$.metadata_cache)) {
        private$update_metadata_cache()
      }
    },

    update_metadata_cache = function() {

      spdl::debug("Updating metadata cache for {} '{}'", self$class(), self$uri)

      # See notes above -- at the TileDB implementation level, we cannot read anything about the
      # group while the group is open for read, but at the SOMA application level we must support
      # this. Therefore if the group is opened for write and there is no cache populated then
      # we must open a temporary handle for read, to fill the cache.

      group_handle <- private$.tiledb_group

      if (private$.mode == "WRITE") {
        group_handle <- tiledb::tiledb_group(self$uri, type = "READ", ctx = private$.tiledb_ctx)
      }

      # NOTE: Strip off key attribute; see https://github.com/TileDB-Inc/TileDB-R/issues/775
      .m <- lapply(tiledb::tiledb_group_get_all_metadata(group_handle),
                   function(.x) {attr(.x, "key") <- NULL; .x})
      private$.metadata_cache <- .m

      if (private$.mode == "WRITE") {
        tiledb::tiledb_group_close(group_handle)
      }

      invisible(NULL)
    },

    add_cached_metadata = function(key, value) {

      if (is.null(private$.metadata_cache)) {
        private$.metadata_cache <- list()
      }

      private$.metadata_cache[[key]] <- value
    }
  )
)
