gc()

test_that("'set_metadata' methods for arrays work as expected", {

  uri <- file.path(withr::local_tempdir(), "test-array")
  create_empty_test_array(uri)

  expect_error(set_metadata(list(1), keys = list(a = 1)))

  # Character method (URI)
  expect_error(set_metadata("file:invalid-path",  keys = list(a = 1)))
  expect_error(set_metadata(c(uri, uri), keys = list(a = 1)), label = "character string for uri path")

  arrobj <- TileDBArray$new(uri)
  origmode <- arrobj$mode

  expect_no_error(out <- set_metadata(arrobj, keys = list(a = 1, b = 2)))
  expect_true(out)
  expect_equal(arrobj$mode, origmode)
  expect_equal(unlist(arrobj$get_metadata()), c(a = 1, b = 2))

  arrobj$reopen("WRITE")
  expect_no_error(set_metadata(arrobj, keys = list(c = 3), timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_equal(arrobj$mode, "WRITE")
  expect_equal(unlist(arrobj$get_metadata()), c(a = 1, b = 2, c = 3))

   # check key at t 1000 is c=3
  arrobj$reopen()
  arrobj$tiledb_timestamp <- as.POSIXct(1000, tz = "UTC")
  expect_equal(unlist(arrobj$get_metadata()), c(c = 3))

  expect_error(set_metadata(arrobj, keys = list(c = 3), timestamp = 1000), label = "timestamp should be posixt")

  arrobj$reopen()
  expect_no_error(set_metadata(arrobj, keys = list(a = 1)))
  expect_equal(arrobj$mode, "READ")

  arrobj$reopen("WRITE")
  expect_no_error(set_metadata(arrobj, keys = list(a = 1)))
  expect_equal(arrobj$mode, "WRITE")

  arrobj$close()
  expect_no_error(set_metadata(uri, keys = list(d = 4)))
  expect_equal(arrobj$mode, "CLOSED")
  expect_equal(unlist(arrobj$get_metadata()), c(a = 1, b = 2, c = 3, d = 4))

  arrobj$reopen()
  arr <- arrobj$object
  origmode <- tiledb::tiledb_array_is_open_for_reading(arr)
  expect_true(origmode)
  expect_no_error(set_metadata(arr, keys = list(e = 5)))
  expect_true(tiledb::tiledb_array_is_open_for_reading(arr))
  close(arr)

  arrobj$reopen()
  expect_equal(unlist(arrobj$get_metadata()), c(a = 1, b = 2, c = 3, d = 4, e = 5))

  expect_false(tiledb::tiledb_array_is_open_for_reading(arr))
  expect_no_error(set_metadata(arr, keys = list(f = 6), timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_false(tiledb::tiledb_array_is_open_for_reading(arr))
  close(arr)

  arrobj$reopen()
  expect_equal(unlist(arrobj$get_metadata()), c(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6))

  rm(arr)
  rm(arrobj)
  gc()

})


test_that("'set_metadata' methods for groups work as expected", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  create_empty_test_group(uri)

  grpobj <- TileDBGroup$new(uri)
  origmode <- grpobj$mode

  expect_no_error(set_metadata(grpobj, keys = list(a = 1, b = 2)))
  expect_equal(grpobj$mode, origmode)
  expect_equal(unlist(grpobj$get_metadata()), c(a = 1, b = 2))

  grpobj$reopen("WRITE")
  expect_no_error(set_metadata(grpobj, keys = list(c = 3), timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_equal(grpobj$mode, "WRITE")
  expect_equal(unlist(grpobj$get_metadata()), c(a = 1, b = 2, c = 3))

  # check key at t 1000 is c=3
  grpobj$reopen()
  grpobj$tiledb_timestamp <- as.POSIXct(1000, tz = "UTC")
  expect_equal(unlist(grpobj$get_metadata()), c(c = 3))

  expect_error(set_metadata(grpobj, keys = list(c = 3), timestamp = 1000), label = "timestamp should be posixt")

  grpobj$reopen()
  expect_no_error(set_metadata(grpobj, keys = list(a = 1)))
  expect_equal(grpobj$mode, "READ")

  grpobj$reopen("WRITE")
  expect_no_error(set_metadata(grpobj, keys = list(a = 1)))
  expect_equal(grpobj$mode, "WRITE")

  grpobj$close()
  expect_no_error(set_metadata(uri, keys = list(d = 4)))
  expect_equal(grpobj$mode, "CLOSED")
  expect_equal(unlist(grpobj$get_metadata()), c(a = 1, b = 2, c = 3, d = 4))

  grpobj$reopen()
  grp <- grpobj$object
  expect_equal(tiledb::tiledb_group_query_type(grp), "READ")
  expect_no_error(set_metadata(grp, keys = list(e = 5)))
  expect_equal(tiledb::tiledb_group_query_type(grp), "READ")
  close(grp)

  grpobj$reopen()
  expect_equal(unlist(grpobj$get_metadata()), c(a = 1, b = 2, c = 3, d = 4, e = 5))

  expect_false(tiledb::tiledb_group_is_open(grp))
  expect_no_error(set_metadata(grp, keys = list(f = 6), timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_false(tiledb::tiledb_group_is_open(grp))
  close(grp)

  grpobj$reopen()
  expect_equal(unlist(grpobj$get_metadata()), c(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6))

  rm(grp)
  rm(grpobj)
  gc()
})

