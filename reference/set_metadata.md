# Set TileDB Metadata

Set metadata using a list of key-value pairs for a `TileDB` array or
group. Optionally, you can write metadata at specific point in time
(time-travelling).

## Usage

``` r
# S3 method for class 'TileDBArray'
set_metadata(x, keys, timestamp = NULL)

# S3 method for class 'TileDBGroup'
set_metadata(x, keys, timestamp = NULL)

# S3 method for class 'tiledb_array'
set_metadata(x, keys, timestamp = NULL)

# S3 method for class 'tiledb_group'
set_metadata(x, keys, timestamp = NULL)

# S3 method for class 'character'
set_metadata(x, keys, timestamp = NULL)
```

## Arguments

- x:

  An `R` object that points to a `TileDB` resource whose metadata are to
  be written.

- keys:

  A named list of key value metadata.

- timestamp:

  Optional datetime object of class `"POSIXct"` to write at this
  timestamp.

## Value

A logical `TRUE`, invisibly.

## Details

`set_metadata()` works similar to
[`metadata<-()`](https://cgiachalis.github.io/R6.tiledb/reference/metadata-set.md)
but works with a list of key value pairs.

The optional argument `timestamp` can be used to set metadata at
specific point in time.

The character method requires a valid URI path.

The methods will not alter the mode of the `TileDB` object.

The object will be opened in write mode temporarily and on exit will
revert back to previous mode; in the case where the object is already in
write mode then it will be reopened at the same mode in order to flush
the metadata on disk.

## See also

[`fetch_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/fetch_metadata.md)
and
[`metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md).
