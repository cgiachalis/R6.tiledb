

test_that("Test 'array_timestamps()' works as expected", {

  tz <- "Europe/London"
  withr::local_timezone(tz = tz)
  uri <- file.path(withr::local_tempdir(), "test-array")

  idx_cols <- c("Dept", "Gender")
  df <- as.data.frame(UCBAdmissions)[1:2,]

  tiledb::fromDataFrame(df, uri, col_index = idx_cols, sparse = TRUE)


  arrobj <- tdb_array(uri)
  end_time <- as.numeric(as.POSIXct("2020-08-20 21:00:00"))
  arrobj$tiledb_timestamp <- set_tiledb_timestamp(as.Date("1990-01-01") , end_time)


  arr_open_end <- .libtiledb_array_open_timestamp_end(arrobj$object@ptr)
  trg1 <- structure(list(
    user_query = list(
      timestamp_start = structure(
        631152000,
        class = c("POSIXct", "POSIXt"),
        tzone = tz
      ),
      timestamp_end = structure(
        end_time,
        class = c("POSIXct", "POSIXt"),
        tzone = tz
      )
    ),
    open_array = list(
      timestamp_start = structure(631152000, class = c("POSIXct", "POSIXt")),
      timestamp_end = structure(arr_open_end, class = c("POSIXct", "POSIXt"))
    )
  ),
  class = "array_timestamps",
  mode = "read",
  tzone = tz)

  # check class first, although we're checking identical object further below
  expect_s3_class(array_timestamps(arrobj), "array_timestamps")
  expect_s3_class(array_timestamps(arrobj$object), "array_timestamps")

  # errors are raised
  expect_error(array_timestamps(arrobj, tz = "invalid"))
  expect_error(array_timestamps(arrobj$object, tz = "invalid"))

  expect_error(array_timestamps(data.frame(1)))

  uri_none <- file.path(withr::local_tempdir(), "test-array-none")
  arrobj_none <- TileDBArray$new(uri_none)
  expect_error(array_timestamps(arrobj_none), label = "Object does not exist")

  # check equality
  expect_equal(array_timestamps(arrobj, tz), trg1)
  expect_equal(array_timestamps(arrobj$object), trg1)

  attr(trg1, "tzone") <- "UTC"
  expect_equal(array_timestamps(arrobj, tz = "UTC"), trg1)

  arrobj$close()

  trg2 <- structure(list(
    user_query = list(
      timestamp_start = structure(
        double(),
        class = c("POSIXct", "POSIXt"),
        tzone = ""
      ),
      timestamp_end = structure(
        double(),
        class = c("POSIXct", "POSIXt"),
        tzone = ""
      )
    ),
    open_array = list(
      timestamp_start = structure(.libtiledb_array_open_timestamp_start(arrobj$object@ptr),
                                  class = c("POSIXct", "POSIXt")),
      timestamp_end = structure(.libtiledb_array_open_timestamp_end(arrobj$object@ptr),
                                class = c("POSIXct", "POSIXt"))
    )
  ),
  class = "array_timestamps",
  mode = "closed",
  tzone = tz)

  expect_equal(array_timestamps(arrobj), trg2)
  expect_equal(array_timestamps(arrobj$object), trg2)


  # Test print method
  arrobj$tiledb_timestamp <- end_time
  expect_snapshot(array_timestamps(arrobj))
  arrobj$close()
  expect_snapshot(array_timestamps(arrobj))

})

