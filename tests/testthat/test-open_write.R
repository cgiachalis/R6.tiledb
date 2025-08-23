

trg_tstamps <- list(timestamp_start = structure(0, class = c("POSIXct", "POSIXt")),
                    timestamp_end = structure(0, class = c("POSIXct", "POSIXt")))

trg_tstamps_t1 <- list(
  timestamp_start = structure(0, class = c("POSIXct", "POSIXt")),
  timestamp_end = structure(1755522770, class = c("POSIXct", "POSIXt"))
)

test_that("'open_write' method for Arrays works OK", {

  uri <- file.path(withr::local_tempdir(), "test-open_write")

  ts <- as.POSIXct("2025-08-18 13:12:50 UTC", tz = "UTC")

  df <- data.frame(id = 1L, val = 1.0)
  tiledb::fromDataFrame(df, uri, col_index = 1, mode = "schema_only")

  # default method
  expect_error(open_write(list(1)), label = " No method for object ‘list(1)’.")

  # Character (URI) method ---
  expect_error(open_write("file://boob"), label = "Invalid TileDB resource")
  expect_no_error(arr <- open_write(uri))
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr))

  tstamps <- array_timestamps(arr)
  expect_equal(tstamps$open_array, trg_tstamps)

  expect_no_error(arr <- open_write(uri, timestamp = ts[1]))
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr))

  tstamps <- array_timestamps(arr)
  expect_equal(tstamps$open_array, trg_tstamps_t1)

  arr <- .tiledb_array_close2(arr)

  # 'tiledb_array' method ---
  arrobj <- TileDBArray$new(uri)
  arr <- arrobj$object
  expect_no_error(arr <- open_write(arr))
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr))

  tstamps <- array_timestamps(arr)
  expect_equal(tstamps$open_array, trg_tstamps)

  expect_no_error(arr <- open_write(arr, timestamp = ts[1]))
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr))

  tstamps <- array_timestamps(arr)
  expect_equal(tstamps$open_array, trg_tstamps_t1)

  arr <- .tiledb_array_close2(arr)


  # 'TileDBArray' method ---
  uri_no <- file.path(withr::local_tempdir(), "test-array-no")
  arrobj_no <- TileDBArray$new(uri_no)
  expect_error(arr <- open_write(arrobj_no))

  arrobj <- TileDBArray$new(uri)
  expect_no_error(arr <- open_write(arrobj))
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr))

  tstamps <- array_timestamps(arr)
  expect_equal(tstamps$open_array, trg_tstamps)

  expect_no_error(arr <- open_write(arrobj, timestamp = ts[1]))
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr))

  tstamps <- array_timestamps(arr)
  expect_equal(tstamps$open_array, trg_tstamps_t1)

  arr <- .tiledb_array_close2(arr)

  })

test_that("'open_write' method for Groups works OK", {

  .get_group_timestamp_end <- function(x) {
    cfg <- tiledb::tiledb_group_get_config(x)

    tend <- cfg[c("sm.group.timestamp_end")]

    if (tend == "18446744073709551615") {
      tend <- NA
    }
    tend <-  as.POSIXct(as.numeric(tend) / 1000, tz = "UTC")

    tend
  }

  ts <- as.POSIXct("2025-08-18 13:12:50 UTC", tz = "UTC")

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)
  # Create a group object on disk
  group$create()

  # Character (URI) method ---

  expect_no_error(grp <- open_write(uri))
  expect_equal(tiledb::tiledb_group_query_type(grp), "WRITE")

  end_time <- .get_group_timestamp_end(grp)
  expect_equal(end_time, as.POSIXct(NA, tz = "UTC"))

  expect_no_error(grp <- open_write(uri, timestamp = ts[1]))
  expect_equal(tiledb::tiledb_group_query_type(grp), "WRITE")

  end_time <- .get_group_timestamp_end(grp)
  expect_equal(end_time, ts[1])

  grp <- tiledb::tiledb_group_close(grp)

  # 'tiledb_group' method ---
  group <- TileDBGroup$new(uri)
  grp <- group$object
  expect_no_error(grp <- open_write(grp))
  expect_equal(tiledb::tiledb_group_query_type(grp), "WRITE")

  end_time <- .get_group_timestamp_end(grp)
  expect_equal(end_time, as.POSIXct(NA, tz = "UTC"))

  expect_no_error(grp <- open_write(grp, timestamp = ts[1]))
  expect_equal(tiledb::tiledb_group_query_type(grp), "WRITE")

  end_time <- .get_group_timestamp_end(grp)
  expect_equal(end_time, ts[1])

  grp <- tiledb::tiledb_group_close(grp)


  # 'TileDBGroup' method ---
  uri_no <- file.path(withr::local_tempdir(), "test-group-no")
  group_no <- TileDBGroup$new(uri_no)
  expect_error(grp <- open_write(group_no))

  group <- TileDBGroup$new(uri)
  expect_no_error(grp <- open_write(group))
  expect_equal(tiledb::tiledb_group_query_type(grp), "WRITE")

  end_time <- .get_group_timestamp_end(grp)
  expect_equal(end_time, as.POSIXct(NA, tz = "UTC"))

  expect_no_error(grp <- open_write(group, timestamp = ts[1]))
  expect_equal(tiledb::tiledb_group_query_type(grp), "WRITE")

  end_time <- .get_group_timestamp_end(grp)
  expect_equal(end_time, ts[1])

  grp <- tiledb::tiledb_group_close(grp)

})
