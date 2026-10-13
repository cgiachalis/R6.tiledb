ucb_schema <- function(ctx) {

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

  schema

}
timetravel_schema <- function(ctx) {
  dim <- tiledb::tiledb_dim("id", type = "INT32", domain = c(1L, 1L), tile = 1L, ctx = ctx)
  dom <- tiledb::tiledb_domain(dims = dim, ctx = ctx)
  schema <- tiledb::tiledb_array_schema(
    domain = dom,
    allows_dups = TRUE,
    attrs = c(tiledb::tiledb_attr("val", type = "FLOAT64", ctx = ctx)),
    sparse = TRUE,
    ctx = ctx
  )
}

create_ucb_array_fixture <- function(uri) {
  ctx <- new_context()
  schema <- ucb_schema(ctx)

  tiledb::tiledb_array_create(uri, schema)

  df <- as.data.frame(UCBAdmissions)[1:4, c("Dept", "Gender", "Admit", "Freq")]
  arr <- tiledb::tiledb_array(uri, "WRITE", ctx = ctx)
  arr[] <- df

  invisible(uri)
}

create_array_fixture <- function(uri) {

  ctx <- new_context()
  schema <- ucb_schema(ctx)
  tiledb::tiledb_array_create(uri, schema)
  arr <- tiledb::tiledb_array(uri, ctx = ctx)
  df <- as.data.frame(UCBAdmissions)
  arr[] <- df[1:8, ]
  arr[] <- df[9:16, ]
  arr[] <- df[17:24, ]

  invisible(TRUE)
}


create_time_travel_fixture <- function(uri) {
  ts <- as.POSIXct(c(
    "2025-08-18 16:12:50",
    "2025-08-18 16:12:55",
    "2025-08-18 16:13:01"
  ), tz = "UTC")

  ctx <- new_context()
  schema <- timetravel_schema(ctx)

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

# write_test_array_tstamps

create_ts_array_fixture <- function(uri, frags = 3) {

  ts <- function(x) {
    as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
  }

  ctx <- new_context()
  schema <- timetravel_schema(ctx)
  tiledb::tiledb_array_create(uri, schema)

  for (i in seq_len(frags) ) {
    arr <- tiledb::tiledb_array(uri, "WRITE", timestamp_end = ts(i), ctx = ctx)
    arr[] <- data.frame(id = 1, val = i)
  }
}
