create_ucb_array_fixture <- function(uri) {
  ctx <- new_context()

  dept_dim <- tiledb::tiledb_dim(
    "Dept", type = "ASCII", domain = NULL, tile = NULL, ctx = ctx
  )
  gender_dim <- tiledb::tiledb_dim(
    "Gender", type = "ASCII", domain = NULL, tile = NULL, ctx = ctx
  )
  dom <- tiledb::tiledb_domain(dims = c(dept_dim, gender_dim), ctx = ctx)

  schema <- tiledb::tiledb_array_schema(
    domain = dom,
    attrs = c(
      tiledb::tiledb_attr("Admit", type = "INT32", ctx = ctx),
      tiledb::tiledb_attr("Freq", type = "FLOAT64", ctx = ctx)
    ),
    enumerations = list(Admit = c("Admitted", "Rejected"),
                        Freq = NULL),
    allows_dups = TRUE,
    sparse = TRUE,
    ctx = ctx
  )

  tiledb::tiledb_array_create(uri, schema)

  df <- as.data.frame(UCBAdmissions)[1:4, c("Dept", "Gender", "Admit", "Freq")]
  arr <- tiledb::tiledb_array(uri, "WRITE", ctx = ctx)
  arr[] <- df

  invisible(uri)
}


create_time_travel_fixture <- function(uri) {
  ts <- as.POSIXct(c(
    "2025-08-18 16:12:50",
    "2025-08-18 16:12:55",
    "2025-08-18 16:13:01"
  ), tz = "UTC")

  ctx <- new_context()
  dim <- tiledb::tiledb_dim("id", type = "INT32", domain = c(1L, 1L), tile = 1L, ctx = ctx)
  dom <- tiledb::tiledb_domain(dims = dim, ctx = ctx)
  schema <- tiledb::tiledb_array_schema(
    domain = dom,
    allows_dups = TRUE,
    attrs = c(tiledb::tiledb_attr("val", type = "FLOAT64", ctx = ctx)),
    sparse = TRUE,
    ctx = ctx
  )

  tiledb::tiledb_array_create(uri, schema)

  arr <- tiledb::tiledb_array(uri, ctx = ctx)

  for (i in seq_along(ts)) {
    arr <- tiledb::tiledb_array_open_at(arr, "WRITE", timestamp = ts[i])
    arr[] <- data.frame(id = 1, val = i)
    arr <- tiledb::tiledb_array_open_at(arr, "WRITE", timestamp = ts[i])
    tiledb::tiledb_put_metadata(arr, paste0("key", i), as.character(ts[i]))
    arr <- tiledb::tiledb_array_close(arr)
  }

  ts
}
