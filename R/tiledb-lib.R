
.isArray <- function(arr){
  methods::is(arr, "tiledb_sparse") || methods::is(arr, "tiledb_dense") || methods::is(arr, "tiledb_array")
}

.libtiledb_array_consolidate <- utils::getFromNamespace("libtiledb_array_consolidate", "tiledb")
.libtiledb_array_vacuum <- utils::getFromNamespace("libtiledb_array_vacuum", "tiledb")
.libtiledb_array_open_at <- utils::getFromNamespace("libtiledb_array_open_at", "tiledb")
.libtiledb_array_close <- utils::getFromNamespace("libtiledb_array_close", "tiledb")
.libtiledb_array_open_timestamp_start <- utils::getFromNamespace("libtiledb_array_open_timestamp_start", "tiledb")
.libtiledb_array_open_timestamp_end <- utils::getFromNamespace("libtiledb_array_open_timestamp_end", "tiledb")
.libtiledb_ctx <- utils::getFromNamespace("libtiledb_ctx", "tiledb")
.libtiledb_config_set <- utils::getFromNamespace("libtiledb_config_set", "tiledb")
.libtiledb_config_unset <- utils::getFromNamespace("libtiledb_config_unset", "tiledb")


# This resets timestamps slots
.tiledb_array_close2 <- function(arr) {

  stopifnot(`The 'arr' argument must be a tiledb_array object` = .isArray(arr))
  arr@timestamp_start <- as.POSIXct(double())
  arr@timestamp_end <- as.POSIXct(double())
  .libtiledb_array_close(arr@ptr)
  arr
}

# This resets timestamps slots
.tiledb_array_open_at2 <- function(arr, type = c("READ", "WRITE"), timestamp) {

  stopifnot(`The 'arr' argument must be a tiledb_array object` = .isArray(arr),
            `The 'timestamp' argument must be a time object` = inherits(timestamp,
                                                                        "POSIXct"))
  type <- match.arg(type)

  # arr <- tiledb::tiledb_array_open_at(arr, type = type, timestamp = timestamp)
  arr@timestamp_start <- as.POSIXct(double())
  arr@timestamp_end <- timestamp

  if (!inherits(arr@ctx, "tiledb_ctx")) {
    stop("'arr@ctx' not a TileDB Context", call. = FALSE)
  }

  # NB: Use context stored in array slot
  arr@ptr <- .libtiledb_array_open_at(arr@ctx@ptr, arr@uri, type, timestamp)
  arr
}

