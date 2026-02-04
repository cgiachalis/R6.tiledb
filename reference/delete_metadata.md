# Delete TileDB Metadata

Delete metadata using keys names for a `TileDB` array or group.

## Usage

``` r
# S3 method for class 'TileDBArray'
delete_metadata(x, keys)

# S3 method for class 'TileDBGroup'
delete_metadata(x, keys)

# S3 method for class 'tiledb_array'
delete_metadata(x, keys)

# S3 method for class 'tiledb_group'
delete_metadata(x, keys)

# S3 method for class 'character'
delete_metadata(x, keys)
```

## Arguments

- x:

  An `R` object that points to a `TileDB` resource whose metadata are to
  be accessed.

- keys:

  A character vector of metadata key names to be accessed.

## Value

A logical `TRUE`, invisibly.

## Details

`delete_metadata()` works similar to
[`metadata<-()`](https://cgiachalis.github.io/R6.tiledb/reference/metadata-set.md)
with `NULL` value but works with a character vector of keys.

The character method requires a valid URI path.

The methods will not alter the mode of the `TileDB` object; also, the
object will be opened temporarily to access the metadata if it is
closed.

## See also

[`set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/set_metadata.md)
and
[`metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md).
