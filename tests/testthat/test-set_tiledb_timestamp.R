trg_timestamp1 <- structure(
  list(
    timestamp_start =
      structure(0, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    timestamp_end = structure(
      numeric(0),
      class = c("POSIXct", "POSIXt"),
      tzone = "UTC"
    )
  ),
  class = "tiledb_timestamp",
  user_tstamp = FALSE
)

trg_timestamp2 <- structure(
  list(
    timestamp_start = structure(0, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    timestamp_end = structure(1, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  ),
  class = "tiledb_timestamp",
  user_tstamp = TRUE
)


test_that("'set_tiledb_timestamp()' works as expected", {

  ts1 <- set_tiledb_timestamp()
  ts1_na <- set_tiledb_timestamp(NA, NA)
  ts1_null <- set_tiledb_timestamp(NULL, NULL)
  ts1_nanull <- set_tiledb_timestamp(NA, NULL)
  ts1_nullna <- set_tiledb_timestamp(NULL, NA)


  expect_s3_class(ts1, "tiledb_timestamp")
  expect_equal(ts1, trg_timestamp1)
  expect_equal(ts1_na, trg_timestamp1)
  expect_equal(ts1_null, trg_timestamp1)
  expect_equal(ts1_nanull, trg_timestamp1)
  expect_equal(ts1_nullna, trg_timestamp1)

  ts2 <- set_tiledb_timestamp(start_time = 0, end_time = 1)
  expect_equal(ts2, trg_timestamp2)

  # identical list but attributes differ (user supplied vs default)
  expect_equal(set_tiledb_timestamp(start_time = 0, end_time = NA),
               trg_timestamp1, ignore_attr = TRUE)

  expect_equal(set_tiledb_timestamp(end_time = "2025-08-15"),
               set_tiledb_timestamp(end_time = 1755216000))

  # errors are raised as expected
  expect_error(set_tiledb_timestamp(start_time = "a"))
  expect_error(set_tiledb_timestamp(end_time = "a"))

  expect_error(set_tiledb_timestamp(end_time = -1), label = "`start_time` is greater than `end_time`")
  expect_error(set_tiledb_timestamp(start_time = "2025-08-15", end_time = "2025-08-14"), label = "`start_time` is greater than `end_time`")


})

test_that("'print()' method for tiledb timestamps", {

  expect_snapshot(set_tiledb_timestamp(end_time = "1990-01-01"))
  expect_snapshot(print(set_tiledb_timestamp(0, 1), tz = "Europe/London"))
  expect_snapshot(print(set_tiledb_timestamp(0, 1), tz = "UTC"))

  })
