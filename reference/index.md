# Package index

## Functional Interface

- [`tdb_array()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_array.md)
  :

  Open `TileDBArrayExp`

- [`tdb_array_create()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_array_create.md)
  : Create a TileDB Array

- [`tdb_group()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_group.md)
  :

  Open `TileDBGroupExp`

- [`tdb_group_create()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_group_create.md)
  : Create a TileDB Group

- [`tdb_fragments()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_fragments.md)
  :

  Open `TileDBFragments`

- [`set_config_params()`](https://cgiachalis.github.io/R6.tiledb/reference/set_config_params.md)
  [`unset_config_params()`](https://cgiachalis.github.io/R6.tiledb/reference/set_config_params.md)
  : Modify Config Parameters

- [`vfs_dir_tree()`](https://cgiachalis.github.io/R6.tiledb/reference/vfs_dir_tree.md)
  : Print Directory Contents

- [`vfs_size()`](https://cgiachalis.github.io/R6.tiledb/reference/vfs_size.md)
  : VFS Directory Size

### Open,Close

Open, close methods

- [`open_write(`*`<TileDBArray>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/open_write.md)
  [`open_write(`*`<tiledb_array>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/open_write.md)
  [`open_write(`*`<TileDBGroup>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/open_write.md)
  [`open_write(`*`<tiledb_group>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/open_write.md)
  [`open_write(`*`<character>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/open_write.md)
  : Open a TileDB Resource at Write Mode
- [`close(`*`<tiledb_array>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/close.md)
  [`close(`*`<TileDBArray>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/close.md)
  [`close(`*`<tiledb_group>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/close.md)
  [`close(`*`<TileDBGroup>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/close.md)
  : Close a TileDB Resource

### Metadata

Metadata methods

- [`metadata(`*`<TileDBArray>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  [`metadata(`*`<TileDBGroup>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  [`metadata(`*`<tiledb_array>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  [`metadata(`*`<tiledb_group>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  [`metadata(`*`<character>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  [`` `metadata<-`( ``*`<TileDBArray>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  [`` `metadata<-`( ``*`<TileDBGroup>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  [`` `metadata<-`( ``*`<tiledb_array>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  [`` `metadata<-`( ``*`<tiledb_group>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  [`` `metadata<-`( ``*`<character>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/metadata.md)
  : Get, Set a TileDB Metadata Key
- [`fetch_metadata(`*`<TileDBArray>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/fetch_metadata.md)
  [`fetch_metadata(`*`<TileDBGroup>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/fetch_metadata.md)
  [`fetch_metadata(`*`<tiledb_array>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/fetch_metadata.md)
  [`fetch_metadata(`*`<tiledb_group>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/fetch_metadata.md)
  [`fetch_metadata(`*`<character>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/fetch_metadata.md)
  : Fetch TileDB Metadata
- [`set_metadata(`*`<TileDBArray>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/set_metadata.md)
  [`set_metadata(`*`<TileDBGroup>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/set_metadata.md)
  [`set_metadata(`*`<tiledb_array>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/set_metadata.md)
  [`set_metadata(`*`<tiledb_group>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/set_metadata.md)
  [`set_metadata(`*`<character>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/set_metadata.md)
  : Set TileDB Metadata
- [`delete_metadata(`*`<TileDBArray>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/delete_metadata.md)
  [`delete_metadata(`*`<TileDBGroup>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/delete_metadata.md)
  [`delete_metadata(`*`<tiledb_array>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/delete_metadata.md)
  [`delete_metadata(`*`<tiledb_group>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/delete_metadata.md)
  [`delete_metadata(`*`<character>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/delete_metadata.md)
  : Delete TileDB Metadata

## R6 Interface

- [`TileDBArray`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArray.md)
  :

  Generate a `TileDBArray` Object

- [`TileDBArrayExp`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBArrayExp.md)
  :

  Generate a `TileDBArrayExp` Object

- [`TileDBFragments`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.md)
  :

  Generate a `TileDBFragments` Object

- [`TileDBGroup`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.md)
  :

  Generate a `TileDBGroup` Object

- [`TileDBGroupExp`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroupExp.md)
  :

  Generate a `TileDBGroupExp` Object

## Utilities

### Demo Arrays

Create demo arrays

- [`demo_array_UCBAdmissions()`](https://cgiachalis.github.io/R6.tiledb/reference/demo_array_UCBAdmissions.md)
  :

  Create a demo array for `UCBAdmissions` dataset

### Timestamps

Helpers to work with timestamps

- [`set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.md)
  :

  Set a `TileDB` timestamp

- [`set_consolidation_timestamps()`](https://cgiachalis.github.io/R6.tiledb/reference/set_consolidation_timestamps.md)
  [`unset_consolidation_timestamps()`](https://cgiachalis.github.io/R6.tiledb/reference/set_consolidation_timestamps.md)
  : Modify Consolidation Start/End Timestamps

- [`set_group_timestamps()`](https://cgiachalis.github.io/R6.tiledb/reference/set_group_timestamps.md)
  [`unset_group_timestamps()`](https://cgiachalis.github.io/R6.tiledb/reference/set_group_timestamps.md)
  : Modify Group Start/End Open Timestamps

- [`array_timestamps(`*`<tiledb_array>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/array_timestamps.md)
  [`array_timestamps(`*`<TileDBArray>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/array_timestamps.md)
  : Get Array Timestamps

- [`group_timestamps(`*`<tiledb_ctx>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/group_timestamps.md)
  [`group_timestamps(`*`<tiledb_group>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/group_timestamps.md)
  [`group_timestamps(`*`<tiledb_config>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/group_timestamps.md)
  [`group_timestamps(`*`<TileDBGroup>`*`)`](https://cgiachalis.github.io/R6.tiledb/reference/group_timestamps.md)
  : Get Group Timestamps
