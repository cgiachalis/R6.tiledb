# Open `TileDBFragments`

Functional interface that initialises a
[TileDBFragments](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.md)
instance.

## Usage

``` r
tdb_fragments(uri, ctx = NULL)
```

## Arguments

- uri:

  URI path for the `TileDB` object.

- ctx:

  Optional
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

## Value

A `TileDBFragments`, `R6` object.

## Active bindings

- `uri` : The URI of the `TileDB` object

- `fragment_info` : Get the TileDB Fragment Info object as returned by
  [tiledb::tiledb_fragment_info](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_fragment_info.html).

## Methods

**Public Methods**

- [`$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-class)
- [`$frag_num()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-frag_num)
- [`$frag_uris()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-frag_uris)
- [`$reload_finfo()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-reload_finfo)
- [`$to_vacuum()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-to_vacuum)
- [`$to_vacuum_num()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-to_vacuum_num)
- [`$delete_fragment_range()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-delete_fragment_range)
- [`$delete_fragment_list()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-delete_fragment_list)
- [`$delete_fragment()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-delete_fragment)
- [`$get_ifragment()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-get_ifragment)
- [`$get_first_ifragments()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-get_first_ifragments)
- [`$get_last_ifragments()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-get_last_ifragments)
- [`$dump()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-dump)
- [`$print()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html#method-print)

## Examples

``` r
if (FALSE) { # \dontrun{
uri <- tempdir()
fobj <- demo_array_UCBAdmissions(uri)

# Get metadata information for first fragment
fobj$get_ifragment(1)
} # }
```
