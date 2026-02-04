# Open a TileDB Resource at Write Mode

Open a URI,
[`tiledb::tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.html),
[`tiledb::tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.html),
[TileDBArray](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.md)
or
[TileDBGroup](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.md)
at write mode with an optional timestamp.

## Usage

``` r
# S3 method for class 'TileDBArray'
open_write(object, timestamp = NULL, ...)

# S3 method for class 'tiledb_array'
open_write(object, timestamp = NULL, ...)

# S3 method for class 'TileDBGroup'
open_write(object, timestamp = NULL, ...)

# S3 method for class 'tiledb_group'
open_write(object, timestamp = NULL, ...)

# S3 method for class 'character'
open_write(object, timestamp = NULL, ...)
```

## Arguments

- object:

  An `R` object that contains a `TileDB` resource pointer.

- timestamp:

  Optional datetime object of class `"POSIXct"` to write at this
  timestamp.

- ...:

  Other arguments passed to methods. Not used.

## Value

An object of class `tiledb_array` or `tiledb_group` depending on the
method; the object is opened in `‘WRITE’` mode.

## Details

Opening a character string should be a valid URI path for a TileDB
resource.

Objects other than a URI character are implicitly closed if found opened
and re-opened at write mode.

Note that when using the `timestamp` argument, the `TileDBArray`,
`TileDBGroup` and their subclasses will have different timestamps from
the returning TileDB object. This is by design, you can not write at
timestamp using `TileDBArray` and `TileDBGroup` interface only via
`open_write()` method.
