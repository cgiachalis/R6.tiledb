# TODO: demo group, list public method, unit tests

#' Open `TileDBGroupExp`
#'
#' Functional interface that initialises a [TileDBGroupExp] instance and opens
#' a group object at `READ` or `WRITE` mode.
#'
#' @section Active bindings:
#'
#' - `ctx` : A TileDB Context. See [tiledb::tiledb_ctx()]
#' - `tiledb_timestamp` : A `TileDB` timestamp range that
#'  the resource will be opened at. See [set_tiledb_timestamp()]
#' - `uri` : The URI of the `TileDB` object
#' - `mode`: Get the mode of the object: one of the following:
#'  `"CLOSED"`, `"READ"` or `"WRITE"`
#' - `object_type` : The `TileDB` object type: `"ARRAY"`,`"GROUP"` or `"INVALID"`
#' - `object` : Access the underlying [tiledb::tiledb_group()] object.
#' - `members`: Access the list of group members.
#'
#' @section Methods:
#'
#' **Public Methods**
#'
#' \if{html}{\out{
#' <ul>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="new"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-new'><code>$new()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="class"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-class'><code>$class()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="is_open"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-is_open'><code>$is_open()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="exists"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-exists'><code>$exists()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="get_metadata"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-get_metadata'><code>$get_metadata()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="set_metadata"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-set_metadata'><code>$set_metadata()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="create"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-create'><code>$create()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="open"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-open'><code>$open()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="close"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-close'><code>$close()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="remove"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-remove'><code>$remove()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="delete"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-delete'><code>$delete()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="count_members"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-count_members'><code>$count_members()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="get_members_df"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-get_members_df'><code>$get_members_df()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="get_member"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-get_member'><code>$get_member()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="set_member"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-set_member'><code>$set_member()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="names"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-names'><code>$names()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="member_exists"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-member_exists'><code>$member_exists()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="print"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-print'><code>$print()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroup" data-id="dump"><a href='../../R6.tiledb/html/TileDBGroup.html#method-TileDBGroup-dump'><code>$dump()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroupExp" data-id="has_non_members"><a href='../../R6.tiledb/html/TileDBGroupExp.html#method-TileDBGroupExp-has_non_members'><code>$has_non_members()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroupExp" data-id="non_members"><a href='../../R6.tiledb/html/TileDBGroupExp.html#method-TileDBGroupExp-non_members'><code>$non_members()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroupExp" data-id="prune_non_members"><a href='../../R6.tiledb/html/TileDBGroupExp.html#method-TileDBGroupExp-prune_non_members'><code>$prune_non_members()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroupExp" data-id="delete_group"><a href='../../R6.tiledb/html/TileDBGroupExp.html#method-TileDBGroupExp-delete_group'><code>$delete_group()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBGroupExp" data-id="walk_group"><a href='../../R6.tiledb/html/TileDBGroupExp.html#method-TileDBGroupExp-walk_group'><code>$walk_group()</code></a></span></li>
#' </ul>
#' }}
#'
#' @inheritParams tdb_array
#'
#' @returns A `TileDBGroupExp`, `R6` object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  uri <- tempdir()
#'
#'  # create demo array on disk
#'  demo_create_array(uri)
#'
#'  arrobj <- tdb_array(uri)
#'
#'  arrobj$object_type
#'  #> "ARRAY"
#'
#'  arrobj$frag_num()
#'  #> 3
#'
#'  arrobj$colnames()
#'  #> "Dept"   "Gender" "Admit"  "Freq"
#'
#'  arrobj$has_enumeration()
#'  #> Admit  Freq
#'  #> TRUE FALSE
#'}
#'
tdb_group <- function(uri,
                      mode = "READ",
                      ctx = NULL,
                      tiledb_timestamp = NULL) {
  obj <- TileDBGroupExp$new(uri, ctx = ctx, tiledb_timestamp = tiledb_timestamp)
  obj$open(mode = mode)
}
