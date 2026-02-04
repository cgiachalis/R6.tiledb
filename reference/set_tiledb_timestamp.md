# Set a `TileDB` timestamp

Define a list of start,end timestamps intended for opening a `TileDB`
object, either at initialisation or via active field `tiledb_timestamp`.

## Usage

``` r
set_tiledb_timestamp(start_time, end_time, tz = "")
```

## Arguments

- start_time, end_time:

  An object coercible to `POSIXct`. See
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html).

- tz:

  A character string for the time zone specification to be used for the
  conversion. Defaults to
  [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html).

## Value

An list object of class `tiledb_timestamp`.

## See also

[TileDBObject](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.md)

## Examples

``` r
# Character input
set_tiledb_timestamp("2025-01-01", "2025-08-01", tz = "UTC")
#> TileDB Timestamp (user trng) • TZ (UTC)
#>  • start: 2025-01-01 00:00:00
#>  • end  : 2025-08-01 00:00:00

# Numeric input
set_tiledb_timestamp(1000000, 1000000*4, tz = "UTC")
#> TileDB Timestamp (user trng) • TZ (UTC)
#>  • start: 1970-01-12 13:46:40
#>  • end  : 1970-02-16 07:06:40

# Date end point
set_tiledb_timestamp(end_time = as.Date("1990-01-01"), tz = "UTC")
#> TileDB Timestamp (user tpnt) • TZ (UTC)
#>  • start: 1970-01-01 00:00:00
#>  • end  : 1990-01-01 00:00:00

# Default
set_tiledb_timestamp(tz = "Europe/London")
#> TileDB Timestamp (default) • TZ (Europe/London)
#>  • start: 1970-01-01 01:00:00
#>  • end  : 2026-02-04 08:49:58

# Invalid: start_time > end_time
# set_tiledb_timestamp(start_time = 1, end_time = 0)
# Error: `start_time` is greater than `end_time`.
```
