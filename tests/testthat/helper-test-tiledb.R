
create_empty_test_array <- function(uri) {
  stopifnot(!dir.exists(uri))
  dim <- tiledb::tiledb_dim("d0", type = "ASCII", domain = NULL, tile = NULL)
  dom <- tiledb::tiledb_domain(dims = dim)
  schema <- tiledb::tiledb_array_schema(
    domain = dom,
    attrs = c(tiledb::tiledb_attr("a", type = "INT32")),
    sparse = TRUE
  )
  tiledb::tiledb_array_create(uri, schema)

  invisible(uri)
}

write_test_array <- function(uri) {

  # Create an array
  idx_cols <- c("Dept", "Gender")
  df <- as.data.frame(UCBAdmissions)

  # Writes 3 parts
  tiledb::fromDataFrame(df[1:8, ], uri, col_index = idx_cols, sparse = TRUE)

  arr <- tiledb::tiledb_array(uri)
  arr[] <- df[9:16, ]
  arr[] <- df[17:24, ]

  invisible(TRUE)
}

# source: https://github.com/TileDB-Inc/TileDB-R/blob/main/inst/tinytest/test_timetravel.R

write_test_array_tstamps <- function(uri, frags = 3) {

  ts <- function(x) {
    as.POSIXct(x / 1000, tz = "UTC", origin = "1970-01-01")
  }

  df <- data.frame(id = 1L, val = 1.0)
  tiledb::fromDataFrame(df, uri, col_index = 1, mode = "schema_only")

  for (i in seq_len(frags) ) {
    arr <- tiledb::tiledb_array(uri, "WRITE", timestamp_end = ts(i))
    arr[] <- data.frame(id = 1, val = i)
  }
}
