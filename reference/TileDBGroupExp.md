# Generate a `TileDBGroupExp` Object

An enhanced version of
[TileDBGroup](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.md)
with additional methods to operate on the group.

### Initialization

A new `TileDBGroupExp` instance is initialised using the `new()` method.
Alternatively use
[`tdb_group()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_group.md)
to create a new instance and open the group at `READ` mode.

     # uri path
     uri <- tempdir()
     # new instance
     obj <- TileDBGroupExp$new(uri = uri)
     # does group exist at this uri
     obj$exists() # FALSE

     unlink(uri)

## Value

An object of class `TileDBGroupExp`, `R6`.

## Super classes

[`R6.tiledb::TileDBObject`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.md)
-\>
[`R6.tiledb::TileDBGroup`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.md)
-\> `TileDBGroupExp`

## Active bindings

- `size`:

  Directory size.

## Methods

### Public methods

- [`TileDBGroupExp$has_non_members()`](#method-TileDBGroupExp-has_non_members)

- [`TileDBGroupExp$non_members()`](#method-TileDBGroupExp-non_members)

- [`TileDBGroupExp$prune_non_members()`](#method-TileDBGroupExp-prune_non_members)

- [`TileDBGroupExp$delete_group()`](#method-TileDBGroupExp-delete_group)

- [`TileDBGroupExp$walk_group()`](#method-TileDBGroupExp-walk_group)

- [`TileDBGroupExp$dir_tree()`](#method-TileDBGroupExp-dir_tree)

- [`TileDBGroupExp$clone()`](#method-TileDBGroupExp-clone)

Inherited methods

- [`R6.tiledb::TileDBObject$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-class)
- [`R6.tiledb::TileDBObject$exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-exists)
- [`R6.tiledb::TileDBObject$get_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-get_metadata)
- [`R6.tiledb::TileDBObject$initialize()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-initialize)
- [`R6.tiledb::TileDBObject$is_open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-is_open)
- [`R6.tiledb::TileDBObject$reopen()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-reopen)
- [`R6.tiledb::TileDBObject$set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-set_metadata)
- [`R6.tiledb::TileDBGroup$close()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-close)
- [`R6.tiledb::TileDBGroup$count_members()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-count_members)
- [`R6.tiledb::TileDBGroup$create()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-create)
- [`R6.tiledb::TileDBGroup$delete()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-delete)
- [`R6.tiledb::TileDBGroup$dump()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-dump)
- [`R6.tiledb::TileDBGroup$get_member()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-get_member)
- [`R6.tiledb::TileDBGroup$get_members_df()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-get_members_df)
- [`R6.tiledb::TileDBGroup$member_exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-member_exists)
- [`R6.tiledb::TileDBGroup$names()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-names)
- [`R6.tiledb::TileDBGroup$open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-open)
- [`R6.tiledb::TileDBGroup$print()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-print)
- [`R6.tiledb::TileDBGroup$remove()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-remove)
- [`R6.tiledb::TileDBGroup$set_member()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-set_member)

------------------------------------------------------------------------

### Method `has_non_members()`

Checks for non group members at group's uri path.

This function compares the `TileDB` resources at group's uri path to its
members to determine if there are any non member.

#### Usage

    TileDBGroupExp$has_non_members()

#### Returns

`TRUE` for having non members, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `non_members()`

List `TileDB` resources which are not members at group's uri path.

This function compares the `TileDB` resources at group's uri path to its
members and returns a `data.frame` with non members object type and uri
path.

#### Usage

    TileDBGroupExp$non_members()

#### Returns

An object of class `data.frame` with columns `TYPE` and `URI` of non
member `TileDB` resources.

------------------------------------------------------------------------

### Method `prune_non_members()`

Delete `TileDB` resources which are not members.

#### Usage

    TileDBGroupExp$prune_non_members()

#### Returns

An character vector of deleted uri paths.

------------------------------------------------------------------------

### Method `delete_group()`

Delete written data from Group.

This function deletes all written data from a `tiledb_group` object,
i.e., the folder will not be consider as a TileDB Group after the
operation. Any other data will not be deleted unless we set
`recursive = TRUE`.

#### Usage

    TileDBGroupExp$delete_group(recursive = FALSE)

#### Arguments

- `recursive`:

  Should all data be deleted inside the group? Default is `FALSE`.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method `walk_group()`

List all `TileDB` resources at group's uri path.

#### Usage

    TileDBGroupExp$walk_group(order = c("pre", "post"))

#### Arguments

- `order`:

  Traversal order, either `pre` (default) or `post`.

#### Returns

An object of class `data.frame` with columns `TYPE` and `URI` with all
`TileDB` resources under group's uri path.

------------------------------------------------------------------------

### Method `dir_tree()`

Print directory contents.

#### Usage

    TileDBGroupExp$dir_tree(recursive = TRUE)

#### Arguments

- `recursive`:

  Should it recurse fully? Defaults to `TRUE`.

#### Returns

A character vector with file paths, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TileDBGroupExp$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
