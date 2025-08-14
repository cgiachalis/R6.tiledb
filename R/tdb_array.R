#' Open `TileDBArrayExp`
#'
#' Functional interface that initialise a `TileDBArrayExp` instance and opens the
#' array at `READ` mode.
#'
#' @section Active bindings:
#'
#' - `ctx` : A TileDB Context. See [tiledb::tiledb_ctx()]
#' - `tiledb_timestamp` : A `TileDB` time-stamp range that
#'  the resource will be opened at. See [set_tiledb_timestamp()]
#' - `uri` : The URI of the `TileDB` object
#' - `mode`: Get the mode of the object: one of the following:
#'  `"CLOSED"`, `"READ"` or `"WRITE"`
#' - `object_type` : The `TileDB` object type: `"ARRAY"`,`"GROUP"` or `"INVALID"`
#' - `object` : Access the underlying [tiledb::tiledb_array()] object. When
#' used before open() method, the underlying array will be initialised at `"READ"`
#' mode and kept open.
#' - `fragments_object` : Access the [TileDBFragments] instance for this
#' array
#' - `schema_version` : Retrieve the schema version for this array
#' - `is_sparse` :  Check array schema for sparsity
#'
#' @section Methods:
#'
#' **Public Methods**
#'
#' \if{html}{\out{
#' <ul>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="class"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-class'><code>$class()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="is_open"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-is_open'><code>$is_open()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="exists"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-exists'><code>$exists()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="get_metadata"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-get_metadata'><code>$get_metadata()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBObject" data-id="set_metadata"><a href='../../R6.tiledb/html/TileDBObject.html#method-TileDBObject-set_metadata'><code>$set_metadata()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="open"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-open'><code>$open()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="close"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-close'><code>$close()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="tiledb_array"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-tiledb_array'><code>$tiledb_array()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="schema"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-schema'><code>$schema()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="schema_info"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-schema_info'><code>$schema_info()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="dimensions"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-dimensions'><code>$dimensions()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="attributes"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-attributes'><code>$attributes()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="dimnames"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-dimnames'><code>$dimnames()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="attrnames"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-attrnames'><code>$attrnames()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="colnames"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-colnames'><code>$colnames()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArray" data-id="print"><a href='../../R6.tiledb/html/TileDBArray.html#method-TileDBArray-print'><code>$print()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="new"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-new'><code>$new()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="reopen"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-reopen'><code>$reopen()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="any_enums"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-any_enums'><code>$any_enums()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="enum_columns"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-enum_columns'><code>$enum_columns()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="enum_levels"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-enum_levels'><code>$enum_levels()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="has_enumeration"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-has_enumeration'><code>$has_enumeration()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="consolidate"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-consolidate'><code>$consolidate()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="consolidate_async"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-consolidate_async'><code>$consolidate_async()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="vacuum"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-vacuum'><code>$vacuum()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="vacuum_async"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-vacuum_async'><code>$vacuum_async()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="consolidate_and_vacuum"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-consolidate_and_vacuum'><code>$consolidate_and_vacuum()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="consolidate_and_vacuum_async"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-consolidate_and_vacuum_async'><code>$consolidate_and_vacuum_async()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="drop_attribute"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-drop_attribute'><code>$drop_attribute()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="frag_num"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-frag_num'><code>$frag_num()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="frag_to_vacuum"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-frag_to_vacuum'><code>$frag_to_vacuum()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="frag_dump"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-frag_dump'><code>$frag_dump()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="frag_uris"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-frag_uris'><code>$frag_uris()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBArrayExp" data-id="schema_upgrade"><a href='../../R6.tiledb/html/TileDBArrayExp.html#method-TileDBArrayExp-schema_upgrade'><code>$schema_upgrade()</code></a></span></li>
#' </ul>
#' }}
#'
#' @param uri URI path for the `TileDB` object.
#' @param mode Mode to open : either `"READ" (default) or "WRITE"`.
#' @param ctx Optional [tiledb::tiledb_ctx()] object.
#' @param tiledb_timestamp Set a `TileDB` time-stamp range that
#'  the resource will be opened at. Effective only for `"READ"` mode.
#'  Valid options:
#'  - A `NULL` value (default)
#'  - An `R` object coercible to `POSIXct`, must be of length 1 and used as end time-stamp
#'  - An object of class `tiledb_timestamp`. See [set_tiledb_timestamp()]
#'
#' Also, it can be set through active field `$tiledb_timestamp`.
#'
#' @returns An `TileDBArrayExp`, `R6` object.
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
tdb_array <- function(uri,
                      mode = "READ",
                      ctx = NULL,
                      tiledb_timestamp = NULL) {
  obj <- TileDBArrayExp$new(uri, ctx = ctx, tiledb_timestamp = tiledb_timestamp)
  obj$open(mode = mode)
}
