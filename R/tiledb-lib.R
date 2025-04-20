# NOTE: This is done to avoid R CHECK NOTEs when accessing un-exported functions from tiledb package

#'
.libtiledb_array_is_open_for_reading <- utils::getFromNamespace("libtiledb_array_is_open_for_reading", "tiledb")
.libtiledb_array_is_open_for_writing <- utils::getFromNamespace("libtiledb_array_is_open_for_writing", "tiledb")

.isArray <- function(arr){
  methods::is(arr, "tiledb_sparse") || methods::is(arr, "tiledb_dense") || methods::is(arr, "tiledb_array")
}

.tiledb_array_is_open_for_reading <- function(arr) {

  stopifnot(`The 'arr' argument must be a tiledb_array object` = .isArray(arr))
  .libtiledb_array_is_open_for_reading(arr@ptr)
}

.tiledb_array_is_open_for_writing <- function(arr) {

  stopifnot(`The 'arr' argument must be a tiledb_array object` = .isArray(arr))
  .libtiledb_array_is_open_for_writing(arr@ptr)
}
