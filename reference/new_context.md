# Create a new TileDB Context

Compared to `tiledb_ctx()`, this variant does not cache the context and
does not accept a character vector of configuration parameters.

## Usage

``` r
new_context(cfg = NULL)
```

## Arguments

- cfg:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html).
