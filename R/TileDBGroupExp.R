#' @title Generate a `TileDBGroupExp` Object
#'
#' @description
#' An enhanced version of [TileDBGroup] with additional methods to
#' operate on the group.
#'
#' ## Initialization
#' A new `TileDBGroupExp` instance is initialised using the `new()` method.
#' Alternatively use `tdb_group()` to create a new instance and open the group
#' at `READ` mode.
#'
#' ```r
#'  # uri path
#'  uri <- tempdir()
#'  # new instance
#'  obj <- TileDBGroupExp$new(uri = uri)
#'  # does group exist at this uri
#'  obj$exists() # FALSE
#'
#'  unlink(uri)
#' ```
#' @returns An object of class `TileDBGroupExp`, `R6`.
#'
#' @export
TileDBGroupExp <- R6::R6Class(
  classname = "TileDBGroupExp",
  inherit = TileDBGroup,
  public = list(

    #' @description Checks for non group members at group's uri
    #' path.
    #'
    #' This function compares the `TileDB` resources at group's
    #' uri path to its members to determine if there are any
    #' non member.
    #'
    #' @return `TRUE` for having non members, `FALSE` otherwise.
    #'
    has_non_members = function() {
      ls_uris <- tiledb::tiledb_object_ls(self$uri)$URI
      m_uris <- self$get_members_df()$uri
      idx <- ls_uris %in% m_uris
      isFALSE(all(idx))
    },

    #' @description List `TileDB` resources which are not
    #' members at group's uri path.
    #'
    #' This function compares the `TileDB` resources at group's
    #' uri path to its members and returns a `data.frame` with
    #' non members object type and uri path.
    #'
    #' @return An object of class `data.frame` with columns `TYPE`
    #' and `URI` of non member `TileDB` resources.
    #'
    non_members = function(){
      obj_df <- tiledb::tiledb_object_ls(self$uri)
      m_df <- self$get_members_df()

      idx <- obj_df$URI %in% m_df$uri

      obj_df[!idx, ]
    },

    #' @description Delete `TileDB` resources which are not members.
    #'
    #' @return An character vector of deleted uri paths.
    #'
    prune_non_members = function() {
      df <- self$non_members()

      if (nrow(df) == 0) {
        invisible(return(NULL))
      }

      non_member_uris <- df$URI

      out <- vapply_char(non_member_uris, function(.uri) {
        tiledb::tiledb_object_rm(.uri, private$.tiledb_ctx)
      })

      unname(out)
    },

    #' @description Delete written data from Group.
    #'
    #' This function deletes all written data from a `tiledb_group` object,
    #' i.e., the folder will not be consider as a TileDB Group after the
    #' operation. Any other data will not be deleted unless we set
    #' `recursive = TRUE`.
    #'
    #'
    #' @param recursive Should all data be deleted inside the group?
    #'  Default is `FALSE`.
    #'
    #' @return The object, invisibly.
    #'
    delete_group = function(recursive = FALSE) {

      on.exit({
        private$.object_type <- "INVALID"
      })

      grp <- self$object
      self$close()

      # grp pointer now is closed, opening as "MODIFY_EXCLUSIVE"
      grp <- tiledb::tiledb_group_open(grp, type = "MODIFY_EXCLUSIVE")

      tiledb::tiledb_group_delete(grp, uri = self$uri, recursive = recursive)
      tiledb::tiledb_group_close(grp)

      invisible(self)
    },
    #' @description List all `TileDB` resources at group's uri path.
    #'
    #' @param order Traversal order, either `pre` (default) or `post`.
    #'
    #' @return An object of class `data.frame` with columns `TYPE`
    #' and `URI` with all `TileDB` resources under group's uri path.
    #'
    walk_group = function(order = c("pre", "post")) {

      order <- match.arg(order)
      xi <- c(pre = "PREORDER", post = "POSTORDER")
      tiledb::tiledb_object_walk(self$uri, order = xi[[order]], ctx = private$.tiledb_ctx)

    },

    #' @description Print directory contents.
    #'
    #' @return A character vector with file paths, invisibly.
    dir_tree = function() {

      vfs_dir_tree(self$uri, vfs = private$vfs())

    }
  ),

  active = list(

    #' @field size Directory size.
    #'
    size = function(value) {

      if (!missing(value)) {
        private$check_read_only("size")
      }
      vfs_size(self$uri, vfs = private$vfs())
    }
  ),

  private = list(

    # Cache a tiledb_vfs object
    #
    .vfs = NULL,

    # Get the tiledb_vfs object
    #
    vfs = function() {

      if (is.null(private$.vfs)) {
        private$.vfs <- tiledb::tiledb_vfs(ctx = self$ctx)
      }
      private$.vfs
    }

  ) # private

)
