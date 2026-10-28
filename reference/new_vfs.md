# Create a new TileDB VFS

Compared to
[`tiledb::tiledb_vfs()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_vfs.html),
this variant does not cache the `tiledb_vfs` and does not accept
configuration parameters.

## Usage

``` r
new_vfs(ctx = new_context())
```

## Arguments

- ctx:

  A TileDB Context (optional).
