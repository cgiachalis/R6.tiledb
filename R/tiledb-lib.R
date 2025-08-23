
.isArray <- function(arr){
  methods::is(arr, "tiledb_sparse") || methods::is(arr, "tiledb_dense") || methods::is(arr, "tiledb_array")
}

.libtiledb_array_close <- utils::getFromNamespace("libtiledb_array_close", "tiledb")
.libtiledb_array_open_timestamp_start <- utils::getFromNamespace("libtiledb_array_open_timestamp_start", "tiledb")
.libtiledb_array_open_timestamp_end <- utils::getFromNamespace("libtiledb_array_open_timestamp_end", "tiledb")


# This reset timestamps slots
.tiledb_array_close2 <- function(arr) {

  stopifnot(`The 'arr' argument must be a tiledb_array object` = .isArray(arr))
  arr@timestamp_start <- as.POSIXct(double())
  arr@timestamp_end <- as.POSIXct(double())
  .libtiledb_array_close(arr@ptr)
  arr
}

# This reset timestamps slots
.tiledb_array_open_at2 <- function(arr, type, timestamp) {

  arr <- tiledb::tiledb_array_open_at(arr, type = type, timestamp = timestamp)
  arr@timestamp_start <- as.POSIXct(double())
  arr@timestamp_end <- timestamp
  arr
}
