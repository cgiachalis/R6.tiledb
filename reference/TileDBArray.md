# Generate a `TileDBArray` Object

Base class for representing a `TileDB` Array.

### Initialization

A new `TileDBArray` instance is initialized using the `new()` method:

     # uri path
     uri <- tempdir()
     # new instance
     obj <- TileDBArray$new(uri = uri)
     # does array exist at this uri
     obj$exists() # FALSE

     unlink(uri)

## Value

An object of class `TileDBArray`, `R6`.

## Super class

[`TileDBObject`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.md)
-\> `TileDBArray`

## Active bindings

- `object`:

  Access the underlying
  [`tiledb::tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.html)
  object. When used before open() method, the underlying array will be
  initialised at `"READ"` mode and kept open.

## Methods

### Public methods

- [`TileDBArray$open()`](#method-TileDBArray-open)

- [`TileDBArray$close()`](#method-TileDBArray-close)

- [`TileDBArray$tiledb_array()`](#method-TileDBArray-tiledb_array)

- [`TileDBArray$schema()`](#method-TileDBArray-schema)

- [`TileDBArray$schema_info()`](#method-TileDBArray-schema_info)

- [`TileDBArray$dimensions()`](#method-TileDBArray-dimensions)

- [`TileDBArray$attributes()`](#method-TileDBArray-attributes)

- [`TileDBArray$dimnames()`](#method-TileDBArray-dimnames)

- [`TileDBArray$attrnames()`](#method-TileDBArray-attrnames)

- [`TileDBArray$colnames()`](#method-TileDBArray-colnames)

- [`TileDBArray$print()`](#method-TileDBArray-print)

- [`TileDBArray$clone()`](#method-TileDBArray-clone)

Inherited methods

- [`TileDBObject$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-class)
- [`TileDBObject$exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-exists)
- [`TileDBObject$get_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-get_metadata)
- [`TileDBObject$initialize()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-initialize)
- [`TileDBObject$is_open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-is_open)
- [`TileDBObject$reopen()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-reopen)
- [`TileDBObject$set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-set_metadata)

------------------------------------------------------------------------

### `TileDBArray$open()`

Open TileDB array object for read or write.

This methods opens the underlying
[`tiledb::tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.html)
object in the new mode if it is different from the current mode.

When the new mode and current mode is the same, no action is taken. To
force close and then open again use `reopen()` method.

When a time-stamp is specified, it will be effective in `"READ"` mode
only.

#### Usage

    TileDBArray$open(mode = c("READ", "WRITE"))

#### Arguments

- `mode`:

  Mode to open : either `"READ"` or `"WRITE"`. Default is `"READ"`.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### `TileDBArray$close()`

Close the object.

#### Usage

    TileDBArray$close()

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### `TileDBArray$tiledb_array()`

Return a
[tiledb::tiledb_array](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.html)
object.

If a `query_type` not provided then it will be inherited from class
mode; in case the class mode is `"CLOSED"`, then the query type defaults
to `"READ"`.

#### Usage

    TileDBArray$tiledb_array(...)

#### Arguments

- `...`:

  Optional arguments to pass to
  [`tiledb::tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.html).

#### Returns

A
[tiledb::tiledb_array](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.html)
object.

------------------------------------------------------------------------

### `TileDBArray$schema()`

Retrieve the array schema as TileDB schema.

#### Usage

    TileDBArray$schema()

#### Returns

A
[tiledb::tiledb_array_schema](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array_schema.html)
object.

------------------------------------------------------------------------

### `TileDBArray$schema_info()`

Retrieve schema information.

#### Usage

    TileDBArray$schema_info()

#### Returns

A `data.frame` object with four column schema information: `names`,
`types`, `status` and `enum`.

------------------------------------------------------------------------

### `TileDBArray$dimensions()`

Retrieve the array dimensions.

#### Usage

    TileDBArray$dimensions()

#### Returns

A named list of
[tiledb::tiledb_dim](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_dim.html)
objects.

------------------------------------------------------------------------

### `TileDBArray$attributes()`

Retrieve the array attributes.

#### Usage

    TileDBArray$attributes()

#### Returns

A list of
[tiledb::tiledb_attr](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_attr.html)
objects.

------------------------------------------------------------------------

### `TileDBArray$dimnames()`

Retrieve dimension names.

#### Usage

    TileDBArray$dimnames()

#### Returns

A character vector with the array's dimension names.

------------------------------------------------------------------------

### `TileDBArray$attrnames()`

Retrieve attribute names.

#### Usage

    TileDBArray$attrnames()

#### Returns

A character vector with the array's attribute names.

------------------------------------------------------------------------

### `TileDBArray$colnames()`

Retrieve the names of all columns, including dimensions and attributes.

#### Usage

    TileDBArray$colnames()

#### Returns

A character vector with the array's column names.

------------------------------------------------------------------------

### `TileDBArray$print()`

Print summary of the array.

#### Usage

    TileDBArray$print()

------------------------------------------------------------------------

### `TileDBArray$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TileDBArray$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
