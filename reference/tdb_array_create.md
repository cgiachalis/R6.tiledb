# Create a TileDB Array

Given a TileDB schema, it will create an array and instantiate a
`TileDBArrayExp` object. The array will be opened at the given mode and
kept opened. It can be accessed via active field `$object`.

## Usage

``` r
tdb_array_create(uri, sch, mode = "WRITE", ctx = NULL)
```

## Arguments

- uri:

  URI path for the `TileDB` object.

- sch:

  A TileDB schema. See constructor
  [`tiledb::tiledb_array_schema()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array_schema.html).

- mode:

  Mode to open: either `"READ"` or `"WRITE"` (default).

- ctx:

  A TileDB Context. See
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html).

## Value

A `TileDBArrayExp`, `R6` object.

## Examples

``` r
if (FALSE) { # interactive()
suppressMessages(library(tiledb))

# construct schema
schema <- tiledb_array_schema(
 dom = tiledb_domain(dims = tiledb_dim("id", c(1L, 4L), 4L, "INT32")),
 attrs = tiledb_attr("model", type = "ASCII"))

# uri path
uri <- tempfile()

# create and open array at WRITE mode
arrobj <- tdb_array_create(uri, sch = schema, mode = "WRITE")

arrobj$schema_info()

arrobj$mode # WRITE
}
```
