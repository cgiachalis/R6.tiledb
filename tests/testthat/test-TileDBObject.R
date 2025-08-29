gc()

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

})
