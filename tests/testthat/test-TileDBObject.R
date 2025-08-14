
test_that("'TileDBObject' abstract class works as expected", {

  uri <- withr::local_tempdir(pattern = "test-TileDBObject")

  arrObj <- TileDBObject$new(uri)

  expect_identical(arrObj$uri, uri)

  # Test methods ---
  expect_equal(arrObj$class(), "TileDBObject")
  expect_error(TileDBObject$new())
  expect_error(TileDBObject$new(uri, ctx = "bad_ctx"))
  expect_error(TileDBObject$new(uri, tiledb_timestamp = "bad_stamp"))
  expect_false(arrObj$is_open())
  expect_equal(arrObj$mode, "CLOSED")
  expect_false(arrObj$exists())

  # arrObj$reopen(mode = "READ") : tested on derived classes

  expect_equal(arrObj$object_type, "INVALID")

  # Test read only active fields ---
  expect_error(arrObj$uri <- "a")
  expect_error(arrObj$ctx <- "a")
  expect_error(arrObj$object_type <- "a")
  expect_error(arrObj$mode <- "a")

  # Test tiledb_timestamp active fields
   expect_no_error(arrObj$tiledb_timestamp <- NULL)
   expect_s3_class(arrObj$tiledb_timestamp, "tiledb_timestamp")

   expect_no_error(arrObj$tiledb_timestamp <- 10)
   expect_equal(arrObj$tiledb_timestamp, set_tiledb_timestamp(ts_end = 10))

   expect_no_error(arrObj$tiledb_timestamp <- "1990-01-01")
   expect_equal(arrObj$tiledb_timestamp, set_tiledb_timestamp(ts_end = "1990-01-01"))

   expect_no_error(arrObj$tiledb_timestamp <- as.POSIXct(10, tz = "UTC"))
   expect_equal(arrObj$tiledb_timestamp, set_tiledb_timestamp(ts_end = as.POSIXct(10)))

   ts <- set_tiledb_timestamp(ts_start = as.Date("1990-01-01"), ts_end = as.Date("2000-01-01"))
   expect_no_error(arrObj$tiledb_timestamp <- ts)
   expect_equal(arrObj$tiledb_timestamp, ts)

   expect_error(arrObj$tiledb_timestamp <- "bob", label = "character string is not in a standard unambiguous format")
   expect_error(arrObj$tiledb_timestamp <- c(1, 3), label = "Invalid 'tiledb_timestamp' input")

})
