# Generate a `TileDBGroup` Object

Base class for representing a `TileDB` Group.

### Initialization

A new `TileDBGroup` instance is initialized using the `new()` method:

     # uri path
     uri <- tempdir()
     # new instance
     obj <- TileDBGroup$new(uri = uri)
     # does array exist at this uri
     obj$exists() # FALSE

     unlink(uri)

## Value

An object of class `TileDBGroup`, `R6`.

## Super class

[`R6.tiledb::TileDBObject`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.md)
-\> `TileDBGroup`

## Active bindings

- `object`:

  Access the underlying
  [`tiledb::tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.html)
  object.

- `members`:

  Access the `list` of group members.

  If TileDB group object is closed, it will be opened to get members and
  close on exit.

## Methods

### Public methods

- [`TileDBGroup$create()`](#method-TileDBGroup-create)

- [`TileDBGroup$open()`](#method-TileDBGroup-open)

- [`TileDBGroup$close()`](#method-TileDBGroup-close)

- [`TileDBGroup$remove()`](#method-TileDBGroup-remove)

- [`TileDBGroup$delete()`](#method-TileDBGroup-delete)

- [`TileDBGroup$count_members()`](#method-TileDBGroup-count_members)

- [`TileDBGroup$get_members_df()`](#method-TileDBGroup-get_members_df)

- [`TileDBGroup$get_member()`](#method-TileDBGroup-get_member)

- [`TileDBGroup$set_member()`](#method-TileDBGroup-set_member)

- [`TileDBGroup$names()`](#method-TileDBGroup-names)

- [`TileDBGroup$member_exists()`](#method-TileDBGroup-member_exists)

- [`TileDBGroup$print()`](#method-TileDBGroup-print)

- [`TileDBGroup$dump()`](#method-TileDBGroup-dump)

- [`TileDBGroup$clone()`](#method-TileDBGroup-clone)

Inherited methods

- [`R6.tiledb::TileDBObject$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-class)
- [`R6.tiledb::TileDBObject$exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-exists)
- [`R6.tiledb::TileDBObject$get_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-get_metadata)
- [`R6.tiledb::TileDBObject$initialize()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-initialize)
- [`R6.tiledb::TileDBObject$is_open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-is_open)
- [`R6.tiledb::TileDBObject$reopen()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-reopen)
- [`R6.tiledb::TileDBObject$set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-set_metadata)

------------------------------------------------------------------------

### Method `create()`

Create a TileDB Group object at the given URI path.

#### Usage

    TileDBGroup$create(mode = "WRITE")

#### Arguments

- `mode`:

  Mode to open : either `"READ"` or `"WRITE"` (default).

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method [`open()`](https://rdrr.io/r/base/connections.html)

Open TileDB group object in read or write mode.

#### Usage

    TileDBGroup$open(mode = c("READ", "WRITE"))

#### Arguments

- `mode`:

  Mode to open : either `"READ"` or `"WRITE"`. Default is `"READ"`.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method [`close()`](https://cgiachalis.github.io/R6.tiledb/reference/close.md)

Close the group object.

All instantiated group members will be closed if opened, and before
closing the group object.

#### Usage

    TileDBGroup$close()

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method [`remove()`](https://rdrr.io/r/base/rm.html)

Remove member.

Removes an array or group resource from `TileDBGroup` member list.

#### Usage

    TileDBGroup$remove(name)

#### Arguments

- `name`:

  Name of the member to remove.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method `delete()`

Delete member.

Deletes a `TileDBGroup`'s member from disk and removes it from member
list.

#### Usage

    TileDBGroup$delete(name)

#### Arguments

- `name`:

  Name of the member to delete.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method `count_members()`

Count the number of members in the group.

#### Usage

    TileDBGroup$count_members()

#### Returns

The number of members in the group.

------------------------------------------------------------------------

### Method `get_members_df()`

List group members.

#### Usage

    TileDBGroup$get_members_df(type = c("ALL", "GROUP", "ARRAY"))

#### Arguments

- `type`:

  Select type member, either`"ALL"`, `"GROUP"` or `"ARRAY"`. By default
  all member types are listed.

#### Returns

A `data.frame` with columns `name`, `type`, and `uri`.

------------------------------------------------------------------------

### Method `get_member()`

Retrieve a group member by name. If the member isn't already open, it is
opened in the same mode as the parent.

#### Usage

    TileDBGroup$get_member(name)

#### Arguments

- `name`:

  The name of the member.

#### Returns

A `TileDBArray` or `TileDBGroup`.

------------------------------------------------------------------------

### Method `set_member()`

Add new member to the group.

#### Usage

    TileDBGroup$set_member(object, name = NULL, relative = NULL)

#### Arguments

- `object`:

  A `TileDBArray` or `TileDBGroup` object to add.

- `name`:

  Name to use for the member. By default the base name of the object's
  URI is used.

- `relative`:

  An optional logical value indicating whether the new object's URI is
  relative to the group's URI. If `NULL` (the default), the object's URI
  is assumed to be relative unless it is a `tiledb://` URI.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method [`names()`](https://rdrr.io/r/base/names.html)

Retrieve the members' names.

#### Usage

    TileDBGroup$names()

#### Returns

A `character` vector of member names.

------------------------------------------------------------------------

### Method `member_exists()`

Check if a member exists.

#### Usage

    TileDBGroup$member_exists(name)

#### Arguments

- `name`:

  Name of the member to check.

#### Returns

A logical value.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print summary of the group.

#### Usage

    TileDBGroup$print()

------------------------------------------------------------------------

### Method [`dump()`](https://rdrr.io/r/base/dump.html)

Dump the TileDB Group structure to string.

#### Usage

    TileDBGroup$dump(title = "TileDB Directory", recursive = TRUE)

#### Arguments

- `title`:

  A character string for title header. Set `NULL` to omit.

- `recursive`:

  Should the nested uris be returned recursively? Default is `TRUE`.

#### Returns

A `character` string, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TileDBGroup$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
