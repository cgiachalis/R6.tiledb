
test_that("'fetch_metadata' methods for arrays work as expected", {

  uri <- file.path(withr::local_tempdir(), "test-array")
  create_empty_test_array(uri)

  expect_error(fetch_metadata(list(1), keys = list(a = 1)))

  # Character method (URI)
  expect_error(fetch_metadata("file:invalid-path",  keys = list(a = 1)))
  expect_error(fetch_metadata(c(uri, uri), keys = list(a = 1)), label = "character string for uri path")

  arrobj <- TileDBArray$new(uri)

  arrobj$open("WRITE")
  arrobj$set_metadata(list(a = 1, b = 2))

  origmode <- arrobj$mode
  expect_error(fetch_metadata(arrobj, keys = list(a = 1, b = 2)), label = "`keys` should be either character vector or `NULL`")
  expect_equal(arrobj$mode, origmode)

  res <- fetch_metadata(arrobj, keys = c("a", "b"))
  expect_s3_class(res, "tdb_metadata")
  expect_equal(unlist(res), c(a = 1, b = 2))

  # add metadata @ t=1000
  arrobj$reopen("WRITE")
  expect_no_error(set_metadata(arrobj, keys = list(c = 3), timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_equal(arrobj$mode, "WRITE")
  expect_equal(unlist(fetch_metadata(arrobj)), c(a = 1, b = 2, c = 3))

   # check key at t 1000 is c=3
  arrobj$reopen()
  arrobj$tiledb_timestamp <- as.POSIXct(1000, tz = "UTC")
  expect_equal(unlist(fetch_metadata(arrobj)), c(c = 3))
  expect_null(unlist(fetch_metadata(arrobj, keys = "a")))

  # or via fetch_metadata timestamp option
  arrobj$tiledb_timestamp <- NULL
  expect_equal(unlist(fetch_metadata(arrobj, timestamp = as.POSIXct(1000, tz = "UTC") )), c(c = 3))

  expect_error(fetch_metadata(arrobj, keys = "a", timestamp = 1000), label = "timestamp should be posixt")

  arrobj$reopen()
  expect_no_error(a1 <- fetch_metadata(arrobj, keys = "a"))
  expect_equal(arrobj$mode, "READ")

  arrobj$reopen("WRITE")
  expect_no_error(fetch_metadata(arrobj, keys = "a"))
  expect_equal(arrobj$mode, "WRITE")

  expect_no_error(aw3 <- fetch_metadata(arrobj, keys = "c", timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_equal(unlist(aw3), c(c = 3))
  expect_s3_class(aw3, "tdb_metadata")
  expect_equal(arrobj$mode, "WRITE")

  arrobj$close()
  expect_equal(unlist(fetch_metadata(uri)), c(a = 1, b = 2, c = 3))
  expect_equal(arrobj$mode, "CLOSED")

  arrobj$reopen()
  arr <- arrobj$object
  origmode <- tiledb::tiledb_array_is_open_for_reading(arr)
  expect_true(origmode)
  expect_no_error(fetch_metadata(arr, keys = "a"))
  expect_true(tiledb::tiledb_array_is_open_for_reading(arr))
  close(arr)

  expect_false(tiledb::tiledb_array_is_open_for_reading(arr))
  expect_no_error(fetch_metadata(arr, keys = "c", timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_false(tiledb::tiledb_array_is_open_for_reading(arr))

  close(arr)
  close(arrobj)

})


test_that("'fetch_metadata' methods for groups work as expected", {


  uri <- file.path(withr::local_tempdir(), "test-group")
  create_empty_test_group(uri)

  grpobj <- TileDBGroup$new(uri)

  grpobj$open("WRITE")
  grpobj$set_metadata(list(a = 1, b = 2))

  origmode <- grpobj$mode
  expect_error(fetch_metadata(grpobj, keys = list(a = 1, b = 2)), label = "`keys` should be either character vector or `NULL`")
  expect_equal(grpobj$mode, origmode)

  res <- fetch_metadata(grpobj, keys = c("a", "b"))
  expect_s3_class(res, "tdb_metadata")
  expect_equal(unlist(res), c(a = 1, b = 2))

  # add metadata @ t=1000
  grpobj$reopen("WRITE")
  expect_no_error(set_metadata(grpobj, keys = list(c = 3), timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_equal(grpobj$mode, "WRITE")
  expect_equal(unlist(fetch_metadata(grpobj)), c(a = 1, b = 2, c = 3))

  # check key at t 1000 is c=3
  grpobj$reopen()
  grpobj$tiledb_timestamp <- as.POSIXct(1000, tz = "UTC")
  expect_equal(unlist(fetch_metadata(grpobj)), c(c = 3))
  expect_null(unlist(fetch_metadata(grpobj, keys = "a")))

  # or via fetch_metadata timestamp option
  grpobj$tiledb_timestamp <- NULL
  expect_equal(unlist(fetch_metadata(grpobj, timestamp = as.POSIXct(1000, tz = "UTC") )), c(c = 3))

  expect_error(fetch_metadata(grpobj, keys = "a", timestamp = 1000), label = "timestamp should be posixt")

  grpobj$reopen()
  expect_no_error(fetch_metadata(grpobj, keys = "a"))
  expect_equal(grpobj$mode, "READ")

  grpobj$reopen("WRITE")
  expect_no_error(fetch_metadata(grpobj, keys = "a"))
  expect_equal(grpobj$mode, "WRITE")

  grpobj$reopen("WRITE")
  expect_no_error(gw3 <- fetch_metadata(grpobj, keys = "c", timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_equal(unlist(gw3), c(c = 3))
  expect_equal(grpobj$mode, "WRITE")

  grpobj$close()
  expect_equal(unlist(fetch_metadata(uri)), c(a = 1, b = 2, c = 3))
  expect_equal(grpobj$mode, "CLOSED")

  grpobj$reopen()
  grp <- grpobj$object
  expect_equal(tiledb::tiledb_group_query_type(grp), "READ")
  expect_no_error(fetch_metadata(grp, keys = "a"))
  expect_equal(tiledb::tiledb_group_query_type(grp), "READ")
  close(grp)

  expect_false(tiledb::tiledb_group_is_open(grp))
  expect_no_error(fetch_metadata(grp, keys = "c", timestamp = as.POSIXct(1000, tz = "UTC")))
  expect_false(tiledb::tiledb_group_is_open(grp))

})
