# Get, Set a TileDB Metadata Key

Get or set a metadata key for a `TileDB` array or group.

## Usage

``` r
# S3 method for class 'TileDBArray'
metadata(x, which)

# S3 method for class 'TileDBGroup'
metadata(x, which)

# S3 method for class 'tiledb_array'
metadata(x, which)

# S3 method for class 'tiledb_group'
metadata(x, which)

# S3 method for class 'character'
metadata(x, which)

# S3 method for class 'TileDBArray'
metadata(x, which) <- value

# S3 method for class 'TileDBGroup'
metadata(x, which) <- value

# S3 method for class 'tiledb_array'
metadata(x, which) <- value

# S3 method for class 'tiledb_group'
metadata(x, which) <- value

# S3 method for class 'character'
metadata(x, which) <- value
```

## Arguments

- x:

  An `R` object that points to a `TileDB` resource whose metadata are to
  be accessed.

- which:

  A non-empty character string specifying which metadata key is to be
  accessed.

- value:

  An object, the new value of the metadata, or `NULL` to remove the key.
  Note that character vectors should be of length one (scalar).

## Value

For the extractor, the key value of the metadata matched, or `NULL` if
no exact match is found.

## Details

These functions work similar to
[`attr()`](https://rdrr.io/r/base/attr.html) and provide access to a
single metadata key on an TileDB object. The replacement form (setter)
puts a metadata key with the value specified or creates a new metadata
with the value given.

The character method is intended for a valid URI path.

The methods will not alter the mode of the `TileDB` object.

For the extractor the object will be opened temporarily to access the
metadata if it is closed.

For the replacement, the object will be opened in write mode temporarily
and on exit will revert back to previous mode; in the case where the
object is already in write mode then it will be reopened at the same
mode in order to flush the metadata on disk.

## See also

For a list of metadata and time-travelling use
[`set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/set_metadata.md)
and
[`fetch_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/fetch_metadata.md).
