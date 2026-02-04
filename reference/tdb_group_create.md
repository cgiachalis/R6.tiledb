# Create a TileDB Group

Create a group and instantiate a `TileDBGroupExp` object. The group will
be opened at the given mode and kept opened. It can be accessed via
active field `$object`.

## Usage

``` r
tdb_group_create(uri, mode = "WRITE", ctx = NULL)
```

## Arguments

- uri:

  URI path for the `TileDB` object.

- mode:

  Mode to open: either `"READ"` or `"WRITE"` (default).

- ctx:

  A TileDB Context. See
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html).

## Value

A `TileDBGroupExp`, `R6` object.

## Examples

``` r
if (FALSE) { # interactive()
# uri path
uri <- tempfile()

# create and open array at WRITE mode
grpobj <- tdb_group_create(uri,mode = "WRITE")

grpobj$mode # WRITE

grpobj$count_members() # 0
}
```
