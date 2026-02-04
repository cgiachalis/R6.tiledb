# Close a TileDB Resource

Close a
[`tiledb::tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.html),
[`tiledb::tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.html),
[TileDBArray](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.md)
or
[TileDBGroup](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.md)
object.

## Usage

``` r
# S3 method for class 'tiledb_array'
close(con, ...)

# S3 method for class 'TileDBArray'
close(con, ...)

# S3 method for class 'tiledb_group'
close(con, ...)

# S3 method for class 'TileDBGroup'
close(con, ...)
```

## Arguments

- con:

  An `R` object that contains a `TileDB` resource pointer.

- ...:

  Other arguments passed to methods. Not used.

## Value

Invisibly, a logical `TRUE` on success.
