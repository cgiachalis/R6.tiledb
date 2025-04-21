

test_that("'TileDBObject' abstract class works as expected", {

  uri <- withr::local_tempdir(pattern = "test-TileDBObject")

  arrObj <- TileDBObject$new(uri, internal_use = "permit")

  expect_identical(arrObj$uri, uri)

  # Test methods ---
  expect_equal(arrObj$class(), "TileDBObject")
  expect_error(TileDBObject$new(uri))
  expect_error(TileDBObject$new(internal_use = "permit"))
  expect_error(TileDBObject$new(uri, tiledbfoms_ctx = "bad_ctx", internal_use_only = "allowed_use"))
  expect_error(TileDBObject$new(uri, tiledb_timestamp = "bad_stamp", internal_use_only = "allowed_use"))
  expect_false(arrObj$is_open())
  expect_equal(arrObj$mode(), "CLOSED")
  expect_false(arrObj$exists())

  # arrObj$reopen(mode = "READ") : tested on derived classes

  expect_equal(arrObj$object_type, "INVALID")

  # Test read only active fields ---
  expect_error(arrObj$uri <- "a")
  expect_error(arrObj$tiledb_timestamp <- "a")
  expect_error(arrObj$tiledbfoms_ctx <- "a")
  expect_error(arrObj$object_type <- "a")

})
