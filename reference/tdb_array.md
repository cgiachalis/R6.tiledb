# Open `TileDBArrayExp`

Functional interface that initialises a
[TileDBArrayExp](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.md)
instance and opens an array object at `READ` or `WRITE` mode.

## Usage

``` r
tdb_array(uri, mode = "READ", ctx = NULL, tiledb_timestamp = NULL)
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

A `TileDBArrayExp`, `R6` object.

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
  [`tiledb::tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.html)
  object. When used before open() method, the underlying array will be
  initialised at `"READ"` mode and kept open

- `fragments_object` : Access the
  [TileDBFragments](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.md)
  instance for this array

- `schema_version` : Retrieve the schema version for this array

- `is_sparse` : Check array schema for sparsity

- `size` : Directory size

## Methods

**Public Methods**

- [`$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-class)
- [`$is_open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-is_open)
- [`$exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-exists)
- [`$get_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-get_metadata)
- [`$set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-set_metadata)
- [`$new()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-new)
- [`$open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-open)
- [`$close()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-close)
- [`$tiledb_array()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-tiledb_array)
- [`$schema()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-schema)
- [`$schema_info()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-schema_info)
- [`$dimensions()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-dimensions)
- [`$attributes()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-attributes)
- [`$dimnames()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-dimnames)
- [`$attrnames()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-attrnames)
- [`$colnames()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-colnames)
- [`$print()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-print)
- [`$create()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-create)
- [`$delete_array()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-delete_array)
- [`$reopen()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-reopen)
- [`$any_enums()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-any_enums)
- [`$enum_columns()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-enum_columns)
- [`$enum_levels()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-enum_levels)
- [`$has_enumeration()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-has_enumeration)
- [`$consolidate()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-consolidate)
- [`$consolidate_async()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-consolidate_async)
- [`$vacuum()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-vacuum)
- [`$vacuum_async()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-vacuum_async)
- [`$consolidate_and_vacuum()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-consolidate_and_vacuum)
- [`$consolidate_and_vacuum_async()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-consolidate_and_vacuum_async)
- [`$drop_attribute()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-drop_attribute)
- [`$frag_num()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-frag_num)
- [`$frag_to_vacuum()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-frag_to_vacuum)
- [`$frag_dump()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-frag_dump)
- [`$frag_uris()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-frag_uris)
- [`$schema_upgrade()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-schema_upgrade)
- [`$dir_tree()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.html#method-dir_tree)

## Examples

``` r
if (FALSE) { # \dontrun{
 uri <- tempdir()

 # create demo array on disk
 demo_array_UCBAdmissions(uri)

 arrobj <- tdb_array(uri)

 arrobj$object_type
 #> "ARRAY"

 arrobj$frag_num()
 #> 3

 arrobj$colnames()
 #> "Dept"   "Gender" "Admit"  "Freq"

 arrobj$has_enumeration()
 #> Admit  Freq
 #> TRUE FALSE
} # }
```
