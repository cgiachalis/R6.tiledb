# Getting Started with R6.tiledb

This vignette will provide an overview of key functionality. Other
vignettes will focus on specific topics in detail including how-to
examples.

### 1. Overview

`R6.tiledb` offers `TileDBArray` and `TileDBGroup` R6 objects that
represent a TileDB array and group respectively. These objects have
minimum encapsulated functionality so they can be further extended and
used by other packages.

The package also offers expanded versions built on top of the above
classes and can be accessed via the functional wrappers
[`tdb_array()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_array.md)
and
[`tdb_group()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_group.md).

### 2. Group Creation

Use
[`tdb_group_create()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_group_create.md)
to create a group and instantiate the `TileDBGroupExp` object.

``` r
# group uri
uri_root <- tempfile()

# create a group
grp <- tdb_group_create(uri_root)

# check if created
grp$exists()
# [1] TRUE
```

Checking for members:

``` r
# no members (empty list)
grp$members
# list()

# count members
grp$count_members()
# [1] 0

# group's uri path
grp$uri
# [1] "C:\\Users\\Constantine\\AppData\\Local\\Temp\\RtmpWwFs4u\\fileef012da1882"
```

### 3. Array Creation

Create two arrays with URI paths relative to group.

#### Array 1

[`tdb_array_create()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_array_create.md)
is a convenient way to create an array given a schema and instantiate a
`TileDBArrayExp` object.

``` r
# array uri
uri_array1 <- file.path(grp$uri, "array_1")

schema <- tiledb::tiledb_array_schema(
             dom = tiledb::tiledb_domain(
               dims = tiledb::tiledb_dim("id", c(1L, 4L), 4L, "INT32")),
               attrs = tiledb::tiledb_attr("model", type = "ASCII")
             )

# create array_1
arrobj_1 <- tdb_array_create(uri_array1, sch = schema)
```

#### Array 2

``` r
# array uri
uri_array2 <- file.path(grp$uri, "array_2")

# demo array
demo_array_UCBAdmissions(uri_array2)

# create array_2
arrobj_2 <- tdb_array(uri_array2)
```

### 4. Group Members

Having created a TileDB group and two arrays, we can organise everything
as a collection by adding the above arrays as group members:

``` r
# add array_1
grp$set_member(arrobj_1)

# add array_2
grp$set_member(arrobj_2)

# reopen to READ mode
grp$reopen()
```

Checking for members:

``` r
# members names
grp$names()
# [1] "array_1" "array_2"

# count members
grp$count_members()
# [1] 2
```

Print group structure:

``` r
# dump raw string
grp$dump()
# 
# ── TileDB Directory ──────────────────────────────────────────────────────────────────────────
# 
# -> fileef012da1882 GROUP
# |-- array_1 ARRAY
# |-- array_2 ARRAY

# print
grp
# R6Class: <TileDBGroupExp>
# → URI Basename: fileef012da1882
#   • Arrays: "array_1" and "array_2"
```

#### Access a Group Member

``` r

# get array_1
arr1 <- grp$get_member("array_1")

# print 
arr1
# R6Class: <TileDBArray>
# → URI Basename: array_1
#   • Dimensions: "id"
#   • Attributes: "model"
```

#### Remove a Group Member

Let’s remove `array_1` from group (but do not delete it from disk).

``` r
# reopen group in WRITE mode
grp$reopen("WRITE")

# remove array
grp$remove("array_1")

# print
grp
# R6Class: <TileDBGroupExp>
# → URI Basename: fileef012da1882
#   • Arrays: "array_2"
```

List all `TileDB` resources at group’s uri path:

``` r
# Find all TileDB resources (members and non-members)
grp$walk_group()
#    TYPE                                                                                URI
# 1 ARRAY file:///C:/Users/Constantine/AppData/Local/Temp/RtmpWwFs4u/fileef012da1882/array_1
# 2 ARRAY file:///C:/Users/Constantine/AppData/Local/Temp/RtmpWwFs4u/fileef012da1882/array_2
```

#### Get a non Group Member

Checking group folder for non-members:

``` r
# check for non-members
grp$has_non_members()
# [1] TRUE

# data.frame with non-members info
grp$non_members()
#    TYPE                                                                                URI
# 1 ARRAY file:///C:/Users/Constantine/AppData/Local/Temp/RtmpWwFs4u/fileef012da1882/array_1
```

#### Prune a non Group Member

Now delete group non-members:

``` r
# delete non-members
grp$prune_non_members()
# [1] "file:///C:/Users/Constantine/AppData/Local/Temp/RtmpWwFs4u/fileef012da1882/array_1"

# data.frame with non-members info
grp$non_members()
# [1] TYPE URI 
# <0 rows> (or 0-length row.names)
```

### 5. Metadata

You can work with metadata, both for arrays and group, in two ways: 1.
using the OO interface via `$get_metadata()` and `$set_metadata()` and
2. using the functional interface with S3 methods:

``` r

# set a single key metadata
metadata(grp, "description") <- "metadata value"

# set multiple metadata
set_metadata(grp, list("a" = 1, "b" = 2))

# get a key metadata
metadata(grp, "a")
# [1] 1


# fetch all metadata
fetch_metadata(grp)
# TileDB GROUP: <R6 Class: TileDBGroupExp>
# Metadata: <key,value> • total 3
#  • a: 1
#  • b: 2
#  • description: 'metadata value'

# fetch a subset of metadata
fetch_metadata(grp, c("a", "b"))
# TileDB GROUP: <R6 Class: TileDBGroupExp>
# Metadata: <key,value> • total 2
#  • a: 1
#  • b: 2


# delete metadata
metadata(grp, "description") <- NULL

# delete multiple keys
delete_metadata(grp, c("a", "b"))

# fetch all metadata (none)
fetch_metadata(grp)
# TileDB GROUP: <R6 Class: TileDBGroupExp>
# Metadata: <key,value> • total 0
```

### 6. Working with Arrays

Using either `TileDBArray` or `TileDBArrayExp` makes it easier to work
with arrays:

``` r
# schema info
arrobj_2$schema_info()
#    names   types status  enum
# 1   Dept   ASCII    Dim FALSE
# 2 Gender   ASCII    Dim FALSE
# 3  Admit   INT32   Attr  TRUE
# 4   Freq FLOAT64   Attr FALSE

# Has factor columns
arrobj_2$has_enumeration()
# Admit  Freq 
#  TRUE FALSE

# fragments
arrobj_2$frag_num()
# [1] 3

# schema version
arrobj_2$schema_version
# [1] 22
```

#### Query Array

You can built queries by using the underlying array object accessed by
active field `$object` and then using the standard `tiledb` interface or
use the `$tiledb_array()` that opens a separate array handle which
invokes the
[`tiledb::tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.html).

``` r
arr <- arrobj_2$object
tiledb::selected_points(arr) <- list(Dept = "A")
tiledb::return_as(arr) <- "data.frame"

arr[]
#   Dept Gender    Admit Freq
# 1    A Female Rejected   19
# 2    A Female Admitted   89
# 3    A   Male Rejected  313
# 4    A   Male Admitted  512
```

Using separate array handle:

``` r
arr <- arrobj_2$tiledb_array(selected_points = list(Dept = "A"),
                            return_as = "data.frame")

arr[]
#   Dept Gender    Admit Freq
# 1    A Female Rejected   19
# 2    A Female Admitted   89
# 3    A   Male Rejected  313
# 4    A   Male Admitted  512
```

#### Fragments

`R6.tiledb` has dedicated R6 object `TileDBFragments` to work with
fragments. It can be accessed either from `TileDBArrayExp` ’s
`fragments_object` field or via its wrapper
[`tdb_fragments()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_fragments.md).

``` r
# fragment object
fobj <- arrobj_2$fragments_object

# or directly
fobj <- tdb_fragments(arrobj_2$uri)

# last i fragments
fobj$get_last_ifragments(1)
# ── FRAGMENT #3 ───────────────────────────────────────────────────────────────────────────────
#  ❯ URI: file:///C:/Users/Constantine/AppData/Local/Temp/RtmpWwFs4u/fileef012da1882/array_2/__fragments/__1770118472534_1770118472534_5fc68549e3245830f7696b445007e221_22
#  ❯ Type: sparse
#  ❯ Non-empty domain: 
#    • Dept: [E, F] (ASCII)
#    • Gender: [Female, Male] (ASCII)
#  ❯ Size: 5.24 KiB
#  ❯ Cell num: 8
#  ❯ Timestamp range: [2026-02-03 11:34:32.533999 UTC, 2026-02-03 11:34:32.533999 UTC]
#  ❯ Format version: 22
#  ❯ Has consolidated metadata: FALSE
```

#### Consolidation and Vacuum

`TileDBArrayExp` class encapsulates consolidation and vacuum operations,
either stand-alone or combined and with asynchronous variations
(provided the
[mirai](https://cran.r-project.org/web/packages/mirai/index.html)
package is found in your system).

``` r
# consolidate
arrobj_2$consolidate()

# num fragments
arrobj_2$frag_num()
# [1] 1

# consolidated fragments to be removed
arrobj_2$frag_to_vacuum(trunc_uri = TRUE)
#    Fragment     start_timestamp       end_timestamp
#      <char>              <POSc>              <POSc>
# 1:       #1 2026-02-03 11:34:31 2026-02-03 11:34:31
# 2:       #2 2026-02-03 11:34:31 2026-02-03 11:34:31
# 3:       #3 2026-02-03 11:34:32 2026-02-03 11:34:32
#                                                                  URI
#                                                               <char>
# 1: __1770118471062_1770118471062_798814059abc057515f57bed7b01832d_22
# 2: __1770118471712_1770118471712_613630033a5372429b0d6c486f7e3c8b_22
# 3: __1770118472534_1770118472534_5fc68549e3245830f7696b445007e221_22

# refresh fragment object
fobj$reload_finfo()

# fragment for clean up (vacuum)
fobj$to_vacuum_num()
# [1] 3

# vacuum
arrobj_2$vacuum()

# consolidated fragments to be removed
arrobj_2$frag_to_vacuum() # none
# [1] Fragment        start_timestamp end_timestamp   URI            
# <0 rows> (or 0-length row.names)
```

Inspect array directory

``` r
arrobj_2$dir_tree()
# C:/Users/Constantine/AppData/Local/Temp/RtmpWwFs4u/fileef012da1882/array_2
# ├── __commits
# │   └── __1770118471062_1770118472534_2be8cf29a9392254f2bdbe1bf92c2972_22.wrt
# ├── __fragments
# │   └── __1770118471062_1770118472534_2be8cf29a9392254f2bdbe1bf92c2972_22
# │       ├── a0.tdb
# │       ├── a1.tdb
# │       ├── d0.tdb
# │       ├── d0_var.tdb
# │       ├── d1.tdb
# │       ├── d1_var.tdb
# │       ├── t.tdb
# │       └── __fragment_metadata.tdb
# ├── __fragment_meta
# ├── __labels
# ├── __meta
# └── __schema
#     ├── __1770118471026_1770118471026_678ac3aab092feb96d012ae1728ce83b
#     └── __enumerations
#         └── __408d8cf2ec5d6c7bafc5efae9078bec0_0
# 
# ❯ directories (6) • total size (6.77 KiB)
```

Get fragment info

``` r
fobj$get_ifragment(1)
# ── FRAGMENT #1 ───────────────────────────────────────────────────────────────────────────────
#  ❯ URI: file:///C:/Users/Constantine/AppData/Local/Temp/RtmpWwFs4u/fileef012da1882/array_2/__fragments/__1770118471062_1770118472534_2be8cf29a9392254f2bdbe1bf92c2972_22
#  ❯ Type: sparse
#  ❯ Non-empty domain: 
#    • Dept: [A, F] (ASCII)
#    • Gender: [Female, Male] (ASCII)
#  ❯ Size: 6.36 KiB
#  ❯ Cell num: 24
#  ❯ Timestamp range: [2026-02-03 11:34:31.062000 UTC, 2026-02-03 11:34:32.533999 UTC]
#  ❯ Format version: 22
#  ❯ Has consolidated metadata: FALSE
```
