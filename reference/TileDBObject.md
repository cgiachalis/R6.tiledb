# Generate a `TileDBObject` Object

A parent class to implement shared functionality for
[TileDBArray](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.md)
and
[TileDBGroup](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.md)
classes.

**This class is not intended to be used directly**.

## Value

An object of class `TileDBObject`, `R6`.

## Active bindings

- `ctx`:

  TileDB Context.

- `tiledb_timestamp`:

  Set or retrieve a `TileDB` timestamp range that the resource will be
  opened at. Effective in `"READ"` mode only.

  This is a **mutable** field to set timestamps dynamically for
  time-travelling. Valid options:

  - A `NULL` value (default)

  - An `R` object coercible to `POSIXct` with length 1 which used for
    end timestamp, or length 2 with start, end timestamps

  - An object of class `tiledb_timestamp`. See
    [`set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.md)

  **Note:** Setting a new timestamp, the object will be reopened only if
  it is in `"READ"` mode. For `TileDBGroup` objects will clear the
  member cache and will reopen the group resource so as to propagate the
  `TileDB` time-stamp to members.

- `uri`:

  The URI of the `TileDB` object.

- `mode`:

  Get the mode of the object: one of the following: `"CLOSED"`, `"READ"`
  or `"WRITE"`.

- `object_type`:

  The TileDB object type:

  - `"ARRAY"`, for dense or sparse array resource

  - `"GROUP"`, for group resource

  - `"INVALID"`, for not a TileDB resource

## Methods

### Public methods

- [`TileDBObject$new()`](#method-TileDBObject-new)

- [`TileDBObject$class()`](#method-TileDBObject-class)

- [`TileDBObject$is_open()`](#method-TileDBObject-is_open)

- [`TileDBObject$reopen()`](#method-TileDBObject-reopen)

- [`TileDBObject$exists()`](#method-TileDBObject-exists)

- [`TileDBObject$get_metadata()`](#method-TileDBObject-get_metadata)

- [`TileDBObject$set_metadata()`](#method-TileDBObject-set_metadata)

- [`TileDBObject$clone()`](#method-TileDBObject-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `TileDB` object.

#### Usage

    TileDBObject$new(uri, ctx = NULL, tiledb_timestamp = NULL)

#### Arguments

- `uri`:

  URI path for the `TileDB` object.

- `ctx`:

  Optional
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

- `tiledb_timestamp`:

  Set a `TileDB` timestamp range that the resource will be opened at.
  Effective in `"READ"` mode only. Valid options:

  - A `NULL` value (default)

  - An `R` object coercible to `POSIXct` with length 1 which is used for
    end timestamp, or length 2 with start, end timestamps

  - An object of class `tiledb_timestamp`. See
    [`set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.md)

  **Note:** When setting new a time-stamp, the object will be reopened
  only if it is in `"READ"` mode.

------------------------------------------------------------------------

### Method [`class()`](https://rdrr.io/r/base/class.html)

Print the name of the R6 class.

#### Usage

    TileDBObject$class()

------------------------------------------------------------------------

### Method `is_open()`

Determine if the object is open for reading or writing.

#### Usage

    TileDBObject$is_open()

#### Returns

`TRUE` if the object is open, otherwise `FALSE`.

------------------------------------------------------------------------

### Method `reopen()`

Close and reopen the TileDB object in a new mode.

#### Usage

    TileDBObject$reopen(mode = c("READ", "WRITE"))

#### Arguments

- `mode`:

  New mode to open the object in; choose from: `"READ"` or `"WRITE"`.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method [`exists()`](https://rdrr.io/r/base/exists.html)

Check if the object exists.

#### Usage

    TileDBObject$exists()

#### Returns

`TRUE` if the object exists, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `get_metadata()`

Retrieve metadata from a `TileDB` Object.

When a `TileDB` object (array or group) is in `"CLOSED"` mode, then it
will be opened in `"READ"` mode in order to fetch the metadata; and be
kept opened until is closed by the user.

#### Usage

    TileDBObject$get_metadata(keys = NULL)

#### Arguments

- `keys`:

  A character vector of metadata keys to retrieve. For `NULL` (default),
  it returns all metadata.

#### Returns

- For scalar key, it returns the key metadata value; if nothing is found
  it returns `NULL`

- For character vector, it returns `list` of metadata with valid keys
  only; if nothing is found it returns an empty `list`

- For `NULL` (default), it returns a list of all metadata

------------------------------------------------------------------------

### Method [`set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/set_metadata.md)

Add list of metadata to a `TileDB` Object.

The `TileDB` object should be open in `"WRITE"` mode.

#### Usage

    TileDBObject$set_metadata(metadata)

#### Arguments

- `metadata`:

  A named list of metadata.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TileDBObject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
