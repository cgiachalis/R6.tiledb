# VFS Directory Size

VFS Directory Size

## Usage

``` r
vfs_size(uri, vfs = NULL)
```

## Arguments

- uri:

  URI path for the `TileDB` object.

- vfs:

  A
  [`tiledb::tiledb_vfs()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_vfs.html)
  object. If `NULL` (default) will create a new VFS object.

## Value

The size of the directory.

## Examples

``` r
if (FALSE) { # \dontrun{
# URI path
uri <- tempfile()

# Demo array
demo_UCBAdmissions_array(uri)

# Directory size
vfs_size(uri)
} # }
```
