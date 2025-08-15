#' @title Generate a `TileDBGroup` Object
#'
#' @description Base class for representing a `TileDB` Group.
#'
#' ## Initialization
#' A new `TileDBGroup` instance is initialized using the `new()` method:
#'
#' ```r
#'  # uri path
#'  uri <- tempdir()
#'  # new instance
#'  obj <- TileDBGroup$new(uri = uri)
#'  # does array exist at this uri
#'  obj$exists() # FALSE
#'
#'  unlink(uri)
#' ```
#'
#' @returns An object of class `TileDBGroup`, `R6`.
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

      # TODO: create group using sys.time via config
      #  Then
      # TODO: open read @ tiledb_stamp range  via config
      #       open write @ sys.time via config

      mode <- match.arg(mode, choices = c("READ", "WRITE"))

      private$log_debug("create", "Creating new group with timestamp ({})", self$tiledb_timestamp %||% "now")

      tiledb::tiledb_group_create(self$uri, ctx = private$.tiledb_ctx)

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

      # TODO: open read @ tiledb_stamp range  via config
      #       open write @ sys.time via config
      private$check_object_exists()
      mode <- match.arg(mode)

      if (is.null(private$.tiledb_group)) {
        private$log_debug("open", "Opening with mode '{}'", mode)
        private$initialize_object()
      }

      init_mode <- self$mode

      is_identical_mode <- init_mode == mode

      private$log_debug0("open", "Requested open mode is {}", ifelse(is_identical_mode, "identical, no mode switch", "not identical, switch mode"))

      if (isFALSE(is_identical_mode)) {

        if (tiledb::tiledb_group_is_open(private$.tiledb_group)) {

          private$log_debug("open", "Closing to switch from {} to {} mode", init_mode, mode)

          tiledb::tiledb_group_close(self$object)
        }

        private$log_debug("open", "Opening in {} mode", mode)

        private$.tiledb_group <- tiledb::tiledb_group_open(self$object, type = mode)
        private$.mode <- mode
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

        private$log_debug("close", "Closing group")

        tiledb::tiledb_group_close(private$.tiledb_group)

        private$.mode <- "CLOSED"
        private$.tiledb_timestamp <- set_tiledb_timestamp()
      }

      invisible(self)
    },

    #' @description Remove member.
    #'
    #' Deletes an array or group resource from `TileDBGroup` member list.
    #'
    #' @param name Name of the member to remove.
    #'
    #' @return `NULL` value, invisibly.
    #'
    remove = function(name) {

      private$check_object_exists()
      private$check_scalar_character(name)
      private$check_open_for_write()

      member <- self$get_member(name)

      private$log_debug("remove", "Removing '{}' member", name)

       tiledb::tiledb_group_remove_member(private$.tiledb_group, uri = name)

      # Drop member if cache has been initialized
      if (is.list(private$.member_cache)) {
        private$.member_cache[[name]] <- NULL
      }

      self$reopen("WRITE")

      invisible(NULL)
    },

    #' @description Delete member.
    #'
    #' Deletes a `TileDBGroup`'s member from disk and removes it from its member list.
    #'
    #' @param name Name of the member to delete.
    #'
    #' @return `NULL` value, invisibly.
    #'
    delete = function(name) {

      private$check_object_exists()
      private$check_scalar_character(name)
      private$check_open_for_write()

      # get_member instantiates the group member
      member <- self$get_member(name)

      private$log_debug("delete", "Deleting '{}' member", name)

      uri_member <- member$uri

      group_handle <- private$.tiledb_group

      # Remove group member
      tiledb::tiledb_group_remove_member(group_handle, uri = name)

      tiledb::tiledb_group_close(group_handle)

      # Remove TileDB resource
      tiledb::tiledb_object_rm(uri_member, private$.tiledb_ctx)

      # Drop member if cache has been initialized
      if (is.list(private$.member_cache)) {
        private$.member_cache[[name]] <- NULL
      }

      self$reopen("WRITE")

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
    #' @return A `data.frame` with columns `name`, `type`, and `uri`.
    #'
    get_members_df = function(type = c("ALL", "GROUP", "ARRAY")) {

      private$check_object_exists()

      type <- match.arg(type)

      member_list <- self$members

      count <- length(member_list)

      df <- data.frame(
        name = character(count),
        type = character(count),
        uri  = character(count)
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
        cli::cli_abort("No member named {.arg {deparse(substitute(name))}} found", call = NULL)
      }

      # Instantiate member object (i.e ARRAY, GROUP etc.)
      #
      # So here if the object (maybe opened for read or write) was stored,
      # we return it. But if not (e.g. first access on read from storage)
      # then we invoke the appropriate constructor. Note: child classes
      # may override construct_member.
      #
      obj <- if (is.null(member$object)) {
        private$log_debug0("get_member", "Construct member with uri {} and type '{}'", member$uri, member$type)
        obj <- private$construct_member(member$uri, member$type)
      } else {
        member$object
      }

      private$log_debug0("get_member", "Check is open,  mode is {}", self$mode)

      if (!obj$is_open()) {
        switch(
          EXPR = (mode <- self$mode),
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
          "{.arg {deparse(substitute(object))}} should be either
          {.cls TileDBArray} or {.cls TileDBGroup} object, not {.cls {class(object)}}.",
          call = NULL)
      }

      # sanitise NA
      if (isTRUE(is.na(name))) name <- NULL

      if ( isFALSE(is.null(name) || rlang::is_scalar_character(name)) ) {
        cli::cli_abort(
          "{.arg {deparse(substitute(name))}} argument should be a character name or NULL, not
          {.cls {class(name)}}.",
          call = NULL
        )
      }

      # sanitise NA
      if (isTRUE(is.na(relative))) relative <- NULL

      if (isFALSE(is.null(relative) || rlang::is_scalar_logical(relative)) ) {
        cli::cli_abort(
          "{.arg {deparse(substitute(relative))}} argument should be a logical or NULL value, not
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


   #' @description Check if a member exists.
   #'
   #' @param name Name of the member to check.
   #'
   #' @return A logical value.
   #'
    member_exists = function(name) {

      private$check_scalar_character(name)

      members <- names(self$members)

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
   #' @param title A character string for title header. Set `NULL` to
   #' omit.
   #' @param recursive  Should the nested uris be returned recursively?
   #'  Default is `TRUE`.
   #'
   #' @return A `character` string, invisibly.
   #'
   dump = function(title = "TileDB Directory", recursive = TRUE) {

     private$check_object_exists()
     private$check_open_for_read_or_write()

     uri <- self$uri
     bsname <- basename(self$uri)

     if(isFALSE(is.null(title))) {
       cli::cat_line()
       cli::cat_rule(title)
       cli::cat_line()
     }
     dmp <- tiledb::tiledb_group_member_dump(private$.tiledb_group, recursive = recursive)
     dmp_raw <- dmp

     s <- unlist(strsplit(dmp, "\n"))

     temp <- sapply(s[-1], function(.dump_str) {
       br <- strsplit(.dump_str, split = " ")[[1]][1]

       if (nchar(br) < 4) {
         .txt <- sub(" GROUP.*", "", .dump_str)
         .txt <- sub(" ARRAY.*", "", .txt)
         .txt <- sub(".* ", "", .txt)
         .txt <- gsub(.txt, cli::col_yellow(.txt), .dump_str)
         return(.txt)
       } else {
         return(.dump_str)
       }
     })

     temp <- c(s[1], temp)
     temp <- gsub(bsname, paste(cli::col_grey("->"),
                               cli::col_cyan(bsname)), temp)
     temp <- gsub("GROUP", cli::col_red("GROUP"), temp)
     temp <- gsub("ARRAY", cli::col_br_blue("ARRAY"), temp)
     cat(temp, sep = "\n")

     invisible(dmp_raw)
   }
  ),

  active = list(
    #' @field object Access the underlying [tiledb::tiledb_group()] object.
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
    },
    #' @field members Access the `list` of group members.
    members = function(value) {

      if (!missing(value)) {
        .emit_read_only_error("members")
      }

      private$check_object_exists()

      if (!self$is_open()) {
        self$open(mode = "READ")
        on.exit(self$close())
      }

      private$fill_member_cache_if_null()

      private$.member_cache
    },

    #'@field tiledb_timestamp Set or retrieve a `TileDB` timestamp range that
    #'  the group members will be opened at.Effective in `"READ"` mode only.
    #'
    #'  This is a **mutable** field to set timestamps dynamically
    #'  for time-travelling. Valid options:
    #'  - A `NULL` value (default)
    #'  - An `R` object coercible to `POSIXct` with length 1 which used for end timestamp,
    #'  or length 2 with start, end timestamps
    #'  - An object of class `tiledb_timestamp`. See [set_tiledb_timestamp()]
    #'
    #'
    #' **Note:** Setting a new timestamp will clear the member cache and will reopen
    #' the group resource so as to propagate the `TileDB` time-stamp to members.
    #'
    tiledb_timestamp = function(value) {

      if (!missing(value)) {

        if (is.null(value)) {
          .time_stamp <- set_tiledb_timestamp()
        } else if (length(value) == 1L) {
          .time_stamp <- set_tiledb_timestamp(end_time = value)
        } else if (length(value) == 2L & !inherits(value, "tiledb_timestamp")) {
          .time_stamp <- set_tiledb_timestamp(start_time = value[1], end_time = value[2])
        } else if (inherits(value, "tiledb_timestamp")) {
          .time_stamp <- value
        } else {
          cli::cli_abort("Invalid 'tiledb_timestamp' input", call = NULL)
        }

        if (self$mode != "WRITE") {
          # Clear cache in order to reopen members with new timestamps
          private$.member_cache <- NULL
          # tiledb::tiledb_group_close(private$.tiledb_group)
          # private$.mode <- "CLOSED"
          # private$initialize_object()

          self$close()
          private$.tiledb_timestamp <- .time_stamp
          self$open()
        }

      }
      private$.tiledb_timestamp
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

    # @description List of cached group members
    # Initially NULL, once the group is created or opened, this is populated
    # with a list that's empty or contains the group members.
    .member_cache = NULL,

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

      private$log_debug0("construct_member", "Entered, uri {} type {}", uri, type)

      constructor <- switch(type,
        ARRAY = TileDBArray$new,
        GROUP = TileDBGroup$new,
        cli::cli_abort("Unknown member type: {.arg {deparse(substitute(type))}.}", call = NULL))

      obj <- constructor(uri, ctx = self$ctx, tiledb_timestamp = self$tiledb_timestamp)
      obj
    },

    # ----------------------------------------------------------------
    # Important implementation note about caching:
    #
    # The caching layer is not solely a performance-enhancer. It's a necessary part of the
    # implementation.
    #
    # We allow users to read array schema and metadata even when the array is opened for write,
    # and also to read group members and metadata even when the group is opened for write.
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

      private$log_debug("update_member_cache", "Updating member cache")

      # See notes above -- at the TileDB implementation level, we cannot read anything about the
      # group while the group is open for read, but at the SOMA application level we must support
      # this. Therefore if the group is opened for write and there is no cache populated then
      # we must open a temporary handle for read, to fill the cache.
      group_handle <- private$.tiledb_group

      if (private$.mode == "WRITE") {

        group_handle <- tiledb::tiledb_group(self$uri, type = "READ", ctx = private$.tiledb_ctx)
        on.exit({tiledb::tiledb_group_close(group_handle)})
      }

      members <- private$get_all_members_uncached_read(group_handle)

      if (is.null(private$.member_cache)) {

        private$.member_cache <- members

      } else {

        # Don't clobber existing cache members, in order to retain original URIs
        members <- members[setdiff(names(members), names(private$.member_cache))]
        private$.member_cache <- utils::modifyList(private$.member_cache, members)

      }
    },

    add_cached_member = function(name, object) {

      # TODO: Review legacy notes:
      #
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
    }
  )
)
