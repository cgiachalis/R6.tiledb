# Modify Group Start/End Open Timestamps

Set or unset group opening start, end timestamps to the given
configuration object.

## Usage

``` r
set_group_timestamps(cfg, start_time = NULL, end_time = NULL)

unset_group_timestamps(cfg)
```

## Arguments

- cfg:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html).

- start_time, end_time:

  Optional time stamp values. A date time objects of class `POSIXt`.

## Value

The modified `tiledb_config` object.

## See also

[`set_config_params()`](https://cgiachalis.github.io/R6.tiledb/reference/set_config_params.md)

## Examples

``` r
cfg <- tiledb::tiledb_config()
cfg <- set_group_timestamps(cfg, as.POSIXct(1), as.POSIXct(1000))
cfg["sm.group.timestamp_start"] # 1000 (ms)
#> sm.group.timestamp_start 
#>                   "1000" 

# reset
cfg <- unset_group_timestamps(cfg)
cfg["sm.group.timestamp_start"] # 0
#> sm.group.timestamp_start 
#>                      "0" 
```
