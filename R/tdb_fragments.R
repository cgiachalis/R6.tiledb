#' Open `TileDBFragments`
#'
#' Functional interface that initialises a [TileDBFragments] instance.
#'
#' @section Active bindings:
#'
#' - `uri` : The URI of the `TileDB` object
#' - `fragment_info` : Get the TileDB Fragment Info object as returned by
#' [tiledb::tiledb_fragment_info].
#'
#' @section Methods:
#'
#' **Public Methods**
#'
#' \if{html}{\out{
#' <ul>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="class"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-class'><code>$class()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="frag_num"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-frag_num'><code>$frag_num()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="frag_uris"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-frag_uris'><code>$frag_uris()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="reload_finfo"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-reload_finfo'><code>$reload_finfo()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="to_vacuum"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-to_vacuum'><code>$to_vacuum()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="to_vacuum_num"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-to_vacuum_num'><code>$to_vacuum_num()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="delete_fragment_range"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-delete_fragment_range'><code>$delete_fragment_range()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="delete_fragment_list"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-delete_fragment_list'><code>$delete_fragment_list()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="delete_fragment"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-delete_fragment'><code>$delete_fragment()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="get_ifragment"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-get_ifragment'><code>$get_ifragment()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="get_first_ifragments"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-get_first_ifragments'><code>$get_first_ifragments()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="get_last_ifragments"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-get_last_ifragments'><code>$get_last_ifragments()</code></a></span></li>
#' <li><span class="pkg-link" data-pkg="R6.tiledb" data-topic="TileDBFragments" data-id="dump"><a href='../../R6.tiledb/html/TileDBFragments.html#method-TileDBFragments-dump'><code>$dump()</code></a></span></li>
#' </ul>
#' }}
#'
#' @inheritParams tdb_array
#'
#' @returns A `TileDBFragments`, `R6` object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' uri <- tempdir()
#' fobj <- demo_array_UCBAdmissions(uri)
#'
#' # Get metadata information for first fragment
#' fobj$get_ifragment(1)
#'}
#'
tdb_fragments <- function(uri, ctx = NULL) {
  TileDBFragments$new(uri, ctx = ctx)
}
