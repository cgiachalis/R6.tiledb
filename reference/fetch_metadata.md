# Fetch TileDB Metadata

Fetch metadata using a list of key-value pairs for a `TileDB` array or
group. Optionally, you can access metadata at specific point in time
(time-travelling).

## Usage

``` r
# S3 method for class 'TileDBArray'
fetch_metadata(x, keys = NULL, timestamp = NULL)

# S3 method for class 'TileDBGroup'
fetch_metadata(x, keys = NULL, timestamp = NULL)

# S3 method for class 'tiledb_array'
fetch_metadata(x, keys = NULL, timestamp = NULL)

# S3 method for class 'tiledb_group'
fetch_metadata(x, keys = NULL, timestamp = NULL)

# S3 method for class 'character'
fetch_metadata(x, keys = NULL, timestamp = NULL)
```

## Arguments

- x:

  An `R` object that points to a `TileDB` resource whose metadata are to
  be accessed.

- keys:

  A character vector of metadata key names to be accessed. When `NULL`
  (default) all metadata will be accessed.

- timestamp:

  Optional datetime object of class `"POSIXct"` to write at this
  timestamp.

## Value

A named list of class `tdb_metadata`.

## Details

`fetch_metadata()` works similar to
[`metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
but works with a list of key value pairs and returns a list always.

The character method requires a valid URI path.

The methods will not alter the mode of the `TileDB` object; also, the
object will be opened temporarily to access the metadata if it is
closed.

## See also

[`set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/set_metadata.md)
and
[`metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md).
