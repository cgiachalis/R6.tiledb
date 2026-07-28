# Print Directory Contents

A modified version of
[`fs::dir_tree()`](https://fs.r-lib.org/reference/dir_tree.html) to work
with TileDB VFS.

## Usage

``` r
vfs_dir_tree(uri, recursive = TRUE, vfs = NULL)
```

## Arguments

- uri:

  URI path for the `TileDB` object.

- recursive:

  Should it recurse fully? Defaults to `TRUE`.

- vfs:

  A
  [`tiledb::tiledb_vfs()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_vfs.html)
  object. If `NULL` (default) will create a new VFS object.

## Value

A character vector with file paths, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
# URI path
uri <- tempfile()

# Demo array
demo_array_UCBAdmissions(uri)

# Array instance
arrobj <- tdb_array(uri)

# Print directory contents
arrobj$dir_tree()
} # }
```
