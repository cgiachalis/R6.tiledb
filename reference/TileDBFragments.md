# Generate a `TileDBFragments` Object

A class for working with `TileDB` Fragments.

### Initialization

A new `TileDBFragments` instance is initialised using the `new()` method
or with the functional interface
[`tdb_fragments()`](https://cgiachalis.github.io/R6.tiledb/reference/tdb_fragments.md).

## Value

An object of class `TileDBFragments`, `R6`.

## Active bindings

- `uri`:

  The URI of the TileDB object.

- `fragment_info`:

  Get the TileDB Fragment Info object as returned by
  [tiledb::tiledb_fragment_info](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_fragment_info.html).

## Methods

### Public methods

- [`TileDBFragments$new()`](#method-TileDBFragments-new)

- [`TileDBFragments$class()`](#method-TileDBFragments-class)

- [`TileDBFragments$frag_num()`](#method-TileDBFragments-frag_num)

- [`TileDBFragments$frag_uris()`](#method-TileDBFragments-frag_uris)

- [`TileDBFragments$reload_finfo()`](#method-TileDBFragments-reload_finfo)

- [`TileDBFragments$to_vacuum()`](#method-TileDBFragments-to_vacuum)

- [`TileDBFragments$to_vacuum_num()`](#method-TileDBFragments-to_vacuum_num)

- [`TileDBFragments$delete_fragment_range()`](#method-TileDBFragments-delete_fragment_range)

- [`TileDBFragments$delete_fragment_list()`](#method-TileDBFragments-delete_fragment_list)

- [`TileDBFragments$delete_fragment()`](#method-TileDBFragments-delete_fragment)

- [`TileDBFragments$get_ifragment()`](#method-TileDBFragments-get_ifragment)

- [`TileDBFragments$get_first_ifragments()`](#method-TileDBFragments-get_first_ifragments)

- [`TileDBFragments$get_last_ifragments()`](#method-TileDBFragments-get_last_ifragments)

- [`TileDBFragments$dump()`](#method-TileDBFragments-dump)

- [`TileDBFragments$print()`](#method-TileDBFragments-print)

------------------------------------------------------------------------

### Method `new()`

Create a new `TileDBFragments` instance.

#### Usage

    TileDBFragments$new(uri, ctx = NULL)

#### Arguments

- `uri`:

  URI path for the `TileDB` Array.

- `ctx`:

  Optional
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

------------------------------------------------------------------------

### Method [`class()`](https://rdrr.io/r/base/class.html)

Print the name of the R6 class.

#### Usage

    TileDBFragments$class()

------------------------------------------------------------------------

### Method `frag_num()`

Get the number of fragments.

#### Usage

    TileDBFragments$frag_num()

------------------------------------------------------------------------

### Method `frag_uris()`

Return a `data.frame` with time stamps and fragments uris.

#### Usage

    TileDBFragments$frag_uris(trunc_uri = TRUE)

#### Arguments

- `trunc_uri`:

  `TRUE` for fragment name in the form: *`__ts1_ts2_<label>_<ver>`*,
  otherwise `FALSE` for full path.

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

### Method `reload_finfo()`

Refresh the TileDB Fragment Info object.

#### Usage

    TileDBFragments$reload_finfo()

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method `to_vacuum()`

Consolidated fragments to be removed.

#### Usage

    TileDBFragments$to_vacuum(trunc_uri = TRUE)

#### Arguments

- `trunc_uri`:

  `TRUE` for fragment name in the form: *`__ts1_ts2_<label>_<ver>`*,
  otherwise `FALSE` for full path.

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

### Method `to_vacuum_num()`

Return the number of fragments to vacuum

#### Usage

    TileDBFragments$to_vacuum_num()

#### Returns

An numeric value.

------------------------------------------------------------------------

### Method `delete_fragment_range()`

Delete fragments using a time-stamp range.

#### Usage

    TileDBFragments$delete_fragment_range(start_time, end_time)

#### Arguments

- `start_time, end_time`:

  Time stamp values. A date time objects of class `POSIXct`.

#### Returns

A logical `TRUE`, invisibly. Note that if time stamps fall outside
fragments' time range no deletion will incur.

------------------------------------------------------------------------

### Method `delete_fragment_list()`

Delete fragments using a vector of fragment uris.

Use `$frag_uris(trunc_uri = FALSE)` method to get a `data.frame` with
all fragment uri paths.

#### Usage

    TileDBFragments$delete_fragment_list(frag_uris)

#### Arguments

- `frag_uris`:

  A vector of fragment uris.

#### Returns

`TRUE` for successful deletion, invisibly.

------------------------------------------------------------------------

### Method `delete_fragment()`

Delete a fragment by index.

#### Usage

    TileDBFragments$delete_fragment(n)

#### Arguments

- `n`:

  A fragment index to be deleted (starts at 1).

#### Returns

A boolean, invisibly. `TRUE` for successful deletion and `FALSE` for no
fragment to delete.

------------------------------------------------------------------------

### Method `get_ifragment()`

Get a fragment by index.

#### Usage

    TileDBFragments$get_ifragment(fid)

#### Arguments

- `fid`:

  A fragment index (starts at 1).

#### Returns

An object of class `ifragment`.

------------------------------------------------------------------------

### Method `get_first_ifragments()`

Get the first `n` fragments.

#### Usage

    TileDBFragments$get_first_ifragments(n)

#### Arguments

- `n`:

  An numeric value.

#### Returns

An object of class `ifragment_list`.

------------------------------------------------------------------------

### Method `get_last_ifragments()`

Get the last `n` fragments.

#### Usage

    TileDBFragments$get_last_ifragments(n)

#### Arguments

- `n`:

  An numeric value.

#### Returns

An object of class `ifragment_list`.

------------------------------------------------------------------------

### Method [`dump()`](https://rdrr.io/r/base/dump.html)

Dump to console the commit fragments.

#### Usage

    TileDBFragments$dump()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print Fragments class.

#### Usage

    TileDBFragments$print()
