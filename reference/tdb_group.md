# Open `TileDBGroupExp`

Functional interface that initialises a
[TileDBGroupExp](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroupExp.md)
instance and opens a group object at `READ` or `WRITE` mode.

## Usage

``` r
tdb_group(uri, mode = "READ", ctx = NULL, tiledb_timestamp = NULL)
```

## Arguments

- uri:

  URI path for the `TileDB` object.

- mode:

  Mode to open : either `"READ" (default) or "WRITE"`.

- ctx:

  Optional
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

- tiledb_timestamp:

  Set a `TileDB` timestamp range that the resource will be opened at.
  Effective only for `"READ"` mode. Valid options:

  - A `NULL` value (default)

  - An `R` object coercible to `POSIXct` with length 1 which used for
    end timestamp, or length 2 with start, end timestamps

  - An object of class `tiledb_timestamp`. See
    [`set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.md)

  Also, it can be set through active field `$tiledb_timestamp`.

## Value

A `TileDBGroupExp`, `R6` object.

## Active bindings

- `ctx` : A TileDB Context. See
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)

- `tiledb_timestamp` : A `TileDB` timestamp range that the resource will
  be opened at. See
  [`set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.md)

- `uri` : The URI of the `TileDB` object

- `mode`: Get the mode of the object: one of the following: `"CLOSED"`,
  `"READ"` or `"WRITE"`

- `object_type` : The `TileDB` object type: `"ARRAY"`,`"GROUP"` or
  `"INVALID"`

- `object` : Access the underlying
  [`tiledb::tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.html)
  object

- `members`: Access the list of group members.

- `size` : Directory size

## Methods

**Public Methods**

- [`$new()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-new)
- [`$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-class)
- [`$is_open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-is_open)
- [`$exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-exists)
- [`$get_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-get_metadata)
- [`$set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-set_metadata)
- [`$create()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-create)
- [`$open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-open)
- [`$close()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-close)
- [`$remove()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-remove)
- [`$delete()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-delete)
- [`$count_members()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-count_members)
- [`$get_members_df()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-get_members_df)
- [`$get_member()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-get_member)
- [`$set_member()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-set_member)
- [`$names()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-names)
- [`$member_exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-member_exists)
- [`$print()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-print)
- [`$dump()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-dump)
- [`$has_non_members()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroupExp.html#method-has_non_members)
- [`$non_members()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroupExp.html#method-non_members)
- [`$prune_non_members()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroupExp.html#method-prune_non_members)
- [`$delete_group()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroupExp.html#method-delete_group)
- [`$walk_group()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroupExp.html#method-walk_group)
- [`$dir_tree()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroupExp.html#method-dir_tree)

## Examples

``` r
if (FALSE) { # \dontrun{
 # uri path
 uri <- tempdir()

 obj <- TileDBGroup$new(uri)

 obj$create()

 obj$close()

 # new instance
 newobj <- tdb_group(uri)

 newobj$is_open() # TRUE

 newobj$mode # "READ"
} # }
```
