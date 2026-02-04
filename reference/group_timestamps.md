# Get Group Timestamps

Get Group Timestamps

## Usage

``` r
# S3 method for class 'tiledb_ctx'
group_timestamps(object, tz = "", ...)

# S3 method for class 'tiledb_group'
group_timestamps(object, tz = "", ...)

# S3 method for class 'tiledb_config'
group_timestamps(object, tz = "", ...)

# S3 method for class 'TileDBGroup'
group_timestamps(object, tz = "", from = c("ctx",
  "cfg"), ...)
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

- from:

  TileDBGroup's source of timestamps: either from context `ctx`, or
  group object configuration `cfg`.

## Value

An object of class `group_timestamps` that is a list with the time range
the group is opened at.

## See also

[`array_timestamps()`](https://cgiachalis.github.io/R6.tiledb/reference/array_timestamps.md),
[`set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.md)
