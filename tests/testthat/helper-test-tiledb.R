.get_group_timestamp_end <- function(x) {
  cfg <- tiledb::tiledb_group_get_config(x)

  tend <- cfg[c("sm.group.timestamp_end")]

  if (tend == "18446744073709551615") {
    tend <- NA
  }
  tend <-  as.POSIXct(as.numeric(tend) / 1000, tz = "UTC")

  tend
}


create_empty_test_array <- function(uri) {
  stopifnot(!dir.exists(uri))
  ctx <- new_context()
  dim <- tiledb::tiledb_dim("d0", type = "ASCII", domain = NULL, tile = NULL, ctx = ctx)
  dom <- tiledb::tiledb_domain(dims = dim, ctx = ctx)
  schema <- tiledb::tiledb_array_schema(
    domain = dom,
    attrs = c(tiledb::tiledb_attr("a", type = "INT32")),
    sparse = TRUE,
    ctx = ctx
  )

  tiledb::tiledb_array_create(uri, schema)

  invisible(uri)
}

.create_array <- function(uri, ctx) {

  flist <- tiledb::tiledb_filter_list(c(
    tiledb::tiledb_filter_set_option(
      tiledb::tiledb_filter("ZSTD", ctx = ctx),
      "COMPRESSION_LEVEL",
      -1
    )
  ),
  ctx =  ctx)

  flist_rle <- tiledb::tiledb_filter_list(c(
    tiledb::tiledb_filter_set_option(
      tiledb::tiledb_filter("RLE", ctx = ctx),
      "COMPRESSION_LEVEL",
      -1
    )
  ), ctx = ctx)

  sch <- tiledb::tiledb_array_schema(
    domain =  tiledb::tiledb_domain(c(
      tiledb::tiledb_dim(
        name = "id",
        domain = c(1L, 1L),
        tile = 1L,
        type = "INT32",
        filter_list = flist,
        ctx = ctx
      )
    ), ctx = ctx),
    attrs = c(
      tiledb::tiledb_attr(
        name = "val",
        type = "FLOAT64",
        ncells = 1,
        nullable = FALSE,
        filter_list = flist,
        ctx = ctx
      )
    ),
    cell_order = "COL_MAJOR",
    tile_order = "COL_MAJOR",
    capacity = 10000,
    sparse = TRUE,
    allows_dups = TRUE,
    coords_filter_list = flist,
    offsets_filter_list = flist,
    validity_filter_list = flist_rle,
    ctx = ctx

  )

  tiledb::tiledb_array_create(uri, sch)

}

create_empty_test_group <- function(uri) {

  stopifnot(!dir.exists(uri))

  tiledb::tiledb_group_create(uri, ctx = new_context())

  invisible(uri)
}


write_test_array <- function(uri) {

  # Create an array
  idx_cols <- c("Dept", "Gender")
  df <- as.data.frame(UCBAdmissions)

  # Writes 3 parts
  tiledb::fromDataFrame(df[1:8, ], uri, col_index = idx_cols, sparse = TRUE)

  arr <- tiledb::tiledb_array(uri, ctx = new_context())
  arr[] <- df[9:16, ]
  arr[] <- df[17:24, ]

  invisible(TRUE)
}

# source: https://github.com/TileDB-Inc/TileDB-R/blob/main/inst/tinytest/test_timetravel.R

write_test_array_tstamps <- function(uri, frags = 3) {

  ts <- function(x) {
    as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
  }

  df <- data.frame(id = 1L, val = 1.0)
  tiledb::fromDataFrame(df, uri, col_index = 1, mode = "schema_only")

  ctx <- new_context()
  for (i in seq_len(frags) ) {
    arr <- tiledb::tiledb_array(uri, "WRITE", timestamp_end = ts(i), ctx = ctx)
    arr[] <- data.frame(id = 1, val = i)
  }
}

write_test_array_tstamps2 <- function(uri, frags = 3) {

  ts <- as.POSIXct(c("2025-08-18 16:12:50", "2025-08-18 16:12:55", "2025-08-18 16:13:01"))

  df <- data.frame(id = 1L, val = 1.0)
  tiledb::fromDataFrame(df, uri, col_index = 1, mode = "schema_only")

  out <- vector("numeric", frags)

  arr <- tiledb::tiledb_array(uri, ctx = new_context())

  for (i in seq_len(frags) ) {

    tm <- ts[i]
    arr <- tiledb::tiledb_array_open_at(arr, "WRITE", timestamp = tm)
    arr[] <- data.frame(id = 1, val = i)
    arr <- tiledb::tiledb_array_open_at(arr, "WRITE", timestamp = tm)
    tiledb::tiledb_put_metadata(arr, paste0("key", i), as.character(tm))
    arr <- tiledb::tiledb_array_close(arr)
  }
  ts
}

write_test_group <- function(uri) {

  ctx <- new_context()

  group_uri <- uri
  uri1 <- R6.tiledb:::file_path(group_uri, "testarray1")
  uri2 <- R6.tiledb:::file_path(group_uri, "testarray2")

  # create group @ t0
  grp <- tiledb::tiledb_group_create(group_uri, ctx = ctx)
  t0 <- Sys.time()
  Sys.sleep(2)

  # create arr1 and add as member @ t1
  arr1 <- write_test_array_tstamps2(uri1)
  grp <- tiledb::tiledb_group(group_uri, type = "WRITE", ctx = ctx)
  tiledb::tiledb_group_add_member(
    grp = grp,
    uri = uri1,
    relative = FALSE,
    name = basename(uri1)
  )
  grp <- tiledb::tiledb_group_close(grp)
  t1 <- Sys.time()

  Sys.sleep(2)

  # create arr2 and add as member @ t2
  arr2 <- write_test_array_tstamps2(uri2)

  grp <- tiledb::tiledb_group_open(grp, type = "WRITE")
  tiledb::tiledb_group_add_member(
    grp = grp,
    uri = uri2,
    relative = FALSE,
    name = basename(uri2)
  )
  grp <- tiledb::tiledb_group_close(grp)
  t2 <- Sys.time()

  list(uri = group_uri,
       group_ts = list(t0 = t0, t1 = t1, t2 = t2),
       arr1_ts = arr1,
       arr2_ts = arr2)
}


write_test_group2 <- function(uri) {

  ctx <-   ctx <- new_context()

  group_uri <- uri
  uri1 <- R6.tiledb:::file_path(group_uri, "testarray1")
  uri2 <- R6.tiledb:::file_path(group_uri, "testarray2")

  # create group @ t0
  grp <- tiledb::tiledb_group_create(group_uri, ctx = ctx)

  arr1 <- create_empty_test_array(uri1)
  grp <- tiledb::tiledb_group(group_uri, type = "WRITE", ctx = ctx)
  tiledb::tiledb_group_add_member(
    grp = grp,
    uri = uri1,
    relative = FALSE,
    name = basename(uri1)
  )
  grp <- tiledb::tiledb_group_close(grp)

  arr2 <- create_empty_test_array(uri2)

  grp <- tiledb::tiledb_group_open(grp, type = "WRITE")
  tiledb::tiledb_group_add_member(
    grp = grp,
    uri = uri2,
    relative = FALSE,
    name = basename(uri2)
  )
  grp <- tiledb::tiledb_group_close(grp)

  invisible(NULL)
}

