# Get Array Timestamps

Get Array Timestamps

## Usage

``` r
# S3 method for class 'tiledb_array'
array_timestamps(object, tz = "", ...)

# S3 method for class 'TileDBArray'
array_timestamps(object, tz = "", ...)
```

## Arguments

- object:

  An `R` object that contains a `TileDB` resource pointer.

- tz:

  A character string for the time zone specification to be used for the
  conversion in print method only. Defaults to
  [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html).

- ...:

  Other arguments passed to methods. Not used.

## Value

An object of class `array_timestamps` that is a list that holds the user
supplied temporal timestamps and the time range the array is opened at.

## See also

[`group_timestamps()`](https://cgiachalis.github.io/R6.tiledb/reference/group_timestamps.md),
[`set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.md)
