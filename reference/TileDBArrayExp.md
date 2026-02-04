# Generate a `TileDBArrayExp` Object

An enhanced version of
[TileDBArray](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.md)
with additional methods to operate on the array.

### Initialization

A new `TileDBArrayExp` instance is initialised using the `new()` method.
Alternatively use
[`tdb_array()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_array.md)
to create a new instance and open the array at `READ` mode.

     # uri path
     uri <- tempdir()
     # new instance
     obj <- TileDBArrayExp$new(uri = uri)
     # does array exist at this uri
     obj$exists() # FALSE

     unlink(uri)

## Value

An object of class `TileDBArrayExp`, `R6`.

## Super classes

[`R6.tiledb::TileDBObject`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.md)
-\>
[`R6.tiledb::TileDBArray`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.md)
-\> `TileDBArrayExp`

## Active bindings

- `fragments_object`:

  Access the
  [TileDBFragments](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.md)
  instance for this array.

- `schema_version`:

  Retrieve the schema version for this array.

- `is_sparse`:

  Check array schema for sparsity.

- `size`:

  Directory size.

## Methods

### Public methods

- [`TileDBArrayExp$create()`](#method-TileDBArrayExp-create)

- [`TileDBArrayExp$delete_array()`](#method-TileDBArrayExp-delete_array)

- [`TileDBArrayExp$reopen()`](#method-TileDBArrayExp-reopen)

- [`TileDBArrayExp$any_enums()`](#method-TileDBArrayExp-any_enums)

- [`TileDBArrayExp$enum_columns()`](#method-TileDBArrayExp-enum_columns)

- [`TileDBArrayExp$enum_levels()`](#method-TileDBArrayExp-enum_levels)

- [`TileDBArrayExp$has_enumeration()`](#method-TileDBArrayExp-has_enumeration)

- [`TileDBArrayExp$consolidate()`](#method-TileDBArrayExp-consolidate)

- [`TileDBArrayExp$consolidate_async()`](#method-TileDBArrayExp-consolidate_async)

- [`TileDBArrayExp$vacuum()`](#method-TileDBArrayExp-vacuum)

- [`TileDBArrayExp$vacuum_async()`](#method-TileDBArrayExp-vacuum_async)

- [`TileDBArrayExp$consolidate_and_vacuum()`](#method-TileDBArrayExp-consolidate_and_vacuum)

- [`TileDBArrayExp$consolidate_and_vacuum_async()`](#method-TileDBArrayExp-consolidate_and_vacuum_async)

- [`TileDBArrayExp$drop_attribute()`](#method-TileDBArrayExp-drop_attribute)

- [`TileDBArrayExp$frag_num()`](#method-TileDBArrayExp-frag_num)

- [`TileDBArrayExp$frag_to_vacuum()`](#method-TileDBArrayExp-frag_to_vacuum)

- [`TileDBArrayExp$frag_dump()`](#method-TileDBArrayExp-frag_dump)

- [`TileDBArrayExp$frag_uris()`](#method-TileDBArrayExp-frag_uris)

- [`TileDBArrayExp$schema_upgrade()`](#method-TileDBArrayExp-schema_upgrade)

- [`TileDBArrayExp$dir_tree()`](#method-TileDBArrayExp-dir_tree)

- [`TileDBArrayExp$clone()`](#method-TileDBArrayExp-clone)

Inherited methods

- [`R6.tiledb::TileDBObject$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-class)
- [`R6.tiledb::TileDBObject$exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-exists)
- [`R6.tiledb::TileDBObject$get_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-get_metadata)
- [`R6.tiledb::TileDBObject$initialize()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-initialize)
- [`R6.tiledb::TileDBObject$is_open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-is_open)
- [`R6.tiledb::TileDBObject$set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-set_metadata)
- [`R6.tiledb::TileDBArray$attributes()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-attributes)
- [`R6.tiledb::TileDBArray$attrnames()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-attrnames)
- [`R6.tiledb::TileDBArray$close()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-close)
- [`R6.tiledb::TileDBArray$colnames()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-colnames)
- [`R6.tiledb::TileDBArray$dimensions()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-dimensions)
- [`R6.tiledb::TileDBArray$dimnames()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-dimnames)
- [`R6.tiledb::TileDBArray$open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-open)
- [`R6.tiledb::TileDBArray$print()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-print)
- [`R6.tiledb::TileDBArray$schema()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-schema)
- [`R6.tiledb::TileDBArray$schema_info()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-schema_info)
- [`R6.tiledb::TileDBArray$tiledb_array()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.html#method-tiledb_array)

------------------------------------------------------------------------

### Method `create()`

Create an empty Array with user defined schema.

The array will be opened at the given mode and kept opened; it can be
accessed via active field `$object`.

#### Usage

    TileDBArrayExp$create(sch, mode = "WRITE")

#### Arguments

- `sch`:

  A TileDB schema. See constructor
  [`tiledb::tiledb_array_schema()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array_schema.html).

- `mode`:

  Mode to open: either `"READ"` or `"WRITE"` (default).

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method `delete_array()`

Delete the array.

#### Usage

    TileDBArrayExp$delete_array()

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method `reopen()`

Close and reopen the TileDB object in a new mode.

#### Usage

    TileDBArrayExp$reopen(mode = c("READ", "WRITE"))

#### Arguments

- `mode`:

  New mode to open the object in; choose from: `"READ"` or `"WRITE"`.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method `any_enums()`

Checks array for factors (enumerations).

#### Usage

    TileDBArrayExp$any_enums()

#### Returns

A boolean. `TRUE` indicating the array has factors and `FALSE`
otherwise.

------------------------------------------------------------------------

### Method `enum_columns()`

Retrieve factor columns (attributes).

#### Usage

    TileDBArrayExp$enum_columns()

#### Returns

A character vector with factor columns (enumeration attributes).

------------------------------------------------------------------------

### Method `enum_levels()`

Retrieve factor levels for a given attribute.

#### Usage

    TileDBArrayExp$enum_levels(x)

#### Arguments

- `x`:

  An attribute name.

#### Returns

A character vector with levels (enumeration values).

------------------------------------------------------------------------

### Method `has_enumeration()`

Check columns for factors.

#### Usage

    TileDBArrayExp$has_enumeration()

#### Returns

A logical vector indicating which column (attribute) is encoded as
factor (enumeration).

------------------------------------------------------------------------

### Method `consolidate()`

Consolidates the fragments of the array into a single fragment.

#### Usage

    TileDBArrayExp$consolidate(mode = c("fragments", "commits", "fragment_meta",
      "array_meta"), cfg = NULL, start_time = NULL, end_time = NULL)

#### Arguments

- `mode`:

  The consolidate mode, one of the following:

  - `"fragments"`: - consolidate all fragments (default)

  - `"commits"`: - consolidate all commit files

  - `"fragment_meta"`: - consolidate only fragment metadata footers to a
    single file

  - `"array_meta"`: - consolidate array metadata only

- `cfg`:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html)
  to set parameters for the consolidation. When `NULL` (default) the
  configuration parameters will be retrieved from object's context.

- `start_time, end_time`:

  Optional time stamp values. A date time objects of class `POSIXt`. If
  not provided, the default values from configuration object will be
  used.

#### Returns

`TRUE`, invisibly.

------------------------------------------------------------------------

### Method `consolidate_async()`

Consolidate fragments of the array into a single fragment
asynchronously.

The consolidation will run in a separate R process in a clean
environment.

**Note this function requires the
[mirai](https://cran.r-project.org/web/packages/mirai/index.html)
package**.

#### Usage

    TileDBArrayExp$consolidate_async(mode = c("fragments", "commits",
      "fragment_meta", "array_meta"), cfg = NULL, start_time = NULL,
      end_time = NULL)

#### Arguments

- `mode`:

  The consolidate mode, one of the following:

  - `"fragments"`: - consolidate all fragments (default)

  - `"commits"`: - consolidate all commit files

  - `"fragment_meta"`: - consolidate only fragment metadata footers to a
    single file

  - `"array_meta"`: - consolidate array metadata only

- `cfg`:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html)
  to set parameters for the consolidation. When `NULL` (default) the
  configuration parameters will be retrieved from object's context.

- `start_time, end_time`:

  Optional time stamp values. A date time objects of class `POSIXt`. If
  not provided, the default values from configuration object will be
  used.

#### Returns

This function will return a
[`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) object
immediately. When it is resolved, it returns `TRUE` indicating
consolidation success.

------------------------------------------------------------------------

### Method `vacuum()`

Clean up consolidated fragments and array metadata.

#### Usage

    TileDBArrayExp$vacuum(mode = c("fragments", "commits", "fragment_meta",
      "array_meta"), cfg = NULL)

#### Arguments

- `mode`:

  The vacuum mode, one of the following:

  - `"fragments"`: - vacuum all fragments (default)

  - `"commits"`: - vacuum all commit files

  - `"fragment_meta"`: - vacuum only fragment metadata footers to a
    single file

  - `"array_meta"`: - vacuum array metadata only

- `cfg`:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html)
  to set parameters for the vacuum. When `NULL` (default) the
  configuration parameters will be retrieved from object's context.

#### Returns

`TRUE`, invisibly.

------------------------------------------------------------------------

### Method `vacuum_async()`

Asynchronously clean up consolidated fragments and array metadata.

The clean up will run in a separate R process in a clean environment.

**Note this function requires the
[mirai](https://cran.r-project.org/web/packages/mirai/index.html)
package**.

#### Usage

    TileDBArrayExp$vacuum_async(mode = c("fragments", "commits", "fragment_meta",
      "array_meta"), cfg = NULL)

#### Arguments

- `mode`:

  The vacuum mode, one of the following:

  - `"fragments"`: - vacuum all fragments (default)

  - `"commits"`: - vacuum all commit files

  - `"fragment_meta"`: - vacuum only fragment metadata footers to a
    single file

  - `"array_meta"`: - vacuum array metadata only

- `cfg`:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html)
  to set parameters for the vacuum process. When `NULL` (default) the
  configuration parameters will be retrieved from object's context.

#### Returns

This function will return a
[`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) object
immediately. When it is resolved, it returns `TRUE` indicating vacuum
success.

------------------------------------------------------------------------

### Method `consolidate_and_vacuum()`

Consolidates and vacuums the fragments.

#### Usage

    TileDBArrayExp$consolidate_and_vacuum(mode = c("fragments", "commits",
      "fragment_meta", "array_meta"), cfg = NULL, start_time = NULL,
      end_time = NULL)

#### Arguments

- `mode`:

  The consolidate and vacuum mode, one of the following:

  - `"fragments"`: - consolidate all fragments (default)

  - `"commits"`: - consolidate all commit files

  - `"fragment_meta"`: - consolidate only fragment metadata footers to a
    single file

  - `"array_meta"`: - consolidate array metadata only

- `cfg`:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html)
  to set parameters for the consolidation and vacuum. When `NULL`
  (default) the configuration parameters will be retrieved from object's
  context.

- `start_time, end_time`:

  Optional time stamp values. A date time objects of class `POSIXt`. If
  not provided, the default values from configuration object will be
  used.

#### Returns

`TRUE`, invisibly.

------------------------------------------------------------------------

### Method `consolidate_and_vacuum_async()`

Consolidate and vacuum fragments asynchronously.

The consolidation and vacuum will run in a separate R process in a clean
environment.

**Note this function requires the
[mirai](https://cran.r-project.org/web/packages/mirai/index.html)
package**.

#### Usage

    TileDBArrayExp$consolidate_and_vacuum_async(mode = c("fragments", "commits",
      "fragment_meta", "array_meta"), cfg = NULL, start_time = NULL,
      end_time = NULL)

#### Arguments

- `mode`:

  The consolidate and vacuum mode, one of the following:

  - `"fragments"`: - consolidate all fragments (default)

  - `"commits"`: - consolidate all commit files

  - `"fragment_meta"`: - consolidate only fragment metadata footers to a
    single file

  - `"array_meta"`: - consolidate array metadata only

- `cfg`:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html)
  to set parameters for the consolidation and vacuum. When `NULL`
  (default) the configuration parameters will be retrieved from object's
  context.

- `start_time, end_time`:

  Optional time stamp values. A date time objects of class `POSIXt`. If
  not provided, the default values from configuration object will be
  used.

#### Returns

This function will return a
[`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) object
immediately. When it is resolved, it returns `TRUE` indicating
consolidation success.

------------------------------------------------------------------------

### Method `drop_attribute()`

Remove an attribute from array.

#### Usage

    TileDBArrayExp$drop_attribute(x)

#### Arguments

- `x`:

  An attribute name.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method `frag_num()`

The number of fragments.

#### Usage

    TileDBArrayExp$frag_num()

------------------------------------------------------------------------

### Method `frag_to_vacuum()`

Consolidated fragments to be removed.

#### Usage

    TileDBArrayExp$frag_to_vacuum(trunc_uri = TRUE)

#### Arguments

- `trunc_uri`:

  `TRUE` to truncate uri path.

#### Returns

An object of class `data.frame` with four columns:

- `Fragment`: the fragment index (starts at 1)

- `start_timestamp`: fragment's start time stamp

- `end_timestamp`: fragment's end time stamp

- `URI`: fragment's truncated uri path (fragment name) when
  `trunc_uri = TRUE` (default), otherwise the full uri path

Note that the return object will be of class `data.table` if the package
is found in your system.

------------------------------------------------------------------------

### Method `frag_dump()`

Dump to console the commit fragments.

#### Usage

    TileDBArrayExp$frag_dump()

------------------------------------------------------------------------

### Method `frag_uris()`

Return a `data.frame` with time stamps and fragments uris.

#### Usage

    TileDBArrayExp$frag_uris(trunc_uri = TRUE)

#### Arguments

- `trunc_uri`:

  `TRUE` to truncate uri path.

#### Returns

An object of class `data.frame` with four columns:

- `Fragment`: the fragment index (start at 1)

- `start_timestamp`: start time-stamp of when fragment was written

- `end_timestamp`: end time-stamp of when fragment was written

- `URI`: fragment's truncated uri path (fragment name) when
  `trunc_uri = TRUE` (default), otherwise the full uri path

Note that the return object will be of class `data.table` if the package
is found in your system.

------------------------------------------------------------------------

### Method `schema_upgrade()`

Upgrade the array to the latest format version.

#### Usage

    TileDBArrayExp$schema_upgrade(cfg = NULL, ctx = NULL)

#### Arguments

- `cfg`:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html).

- `ctx`:

  Optional
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object. By default, object's context is used.

------------------------------------------------------------------------

### Method `dir_tree()`

Print directory contents.

#### Usage

    TileDBArrayExp$dir_tree(recursive = TRUE)

#### Arguments

- `recursive`:

  Should it recurse fully? Defaults to `TRUE`.

#### Returns

A character vector with file paths, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TileDBArrayExp$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
