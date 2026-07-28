# Modify Config Parameters

These functions will set or unset the parameters of TileDB Config object
and return the modified configuration object.

## Usage

``` r
set_config_params(cfg, keyval)

unset_config_params(cfg, keys)
```

## Arguments

- cfg:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html).

- keyval:

  A named character vector with configuration parameters.

- keys:

  A character with configuration keys to unset.

## Value

The modified `tiledb_config` object.

## See also

[`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html)

## Examples

``` r
origparams <- c("sm.consolidation.timestamp_start" = "100",
                "sm.consolidation.timestamp_end" = "20000")

# Create a context with a config setting the params above
cfg <- tiledb::tiledb_config(origparams)
ctx <- tiledb::tiledb_ctx(config = cfg)

# Do something with ctx

# Get configs from context
cfg <- tiledb::config(ctx)

# New params
params <-  c("sm.consolidation.timestamp_start" = "100",
             "sm.consolidation.timestamp_end" = "10000")

cfg <- set_config_params(cfg, params)

cfg["sm.consolidation.timestamp_end"] # "10000"
#> sm.consolidation.timestamp_end 
#>                        "10000" 

# Update context with modified config object
ctx <- tiledb::tiledb_ctx(config = cfg)


# Reset consolidation timestamps
cfg <- unset_config_params(cfg, names(params))

# check the default values
cfg["sm.consolidation.timestamp_start"] # "0"
#> sm.consolidation.timestamp_start 
#>                              "0" 
cfg["sm.consolidation.timestamp_end"]   # "18446744073709551615"
#> sm.consolidation.timestamp_end 
#>         "18446744073709551615" 

# Update context with modified config object
ctx <- tiledb::tiledb_ctx(config = cfg)
```
