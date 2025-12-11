
test_that("'delete_metadata' methods for arrays work as expected", {

  uri <- file.path(withr::local_tempdir(), "test-array")
  create_empty_test_array(uri)
  set_metadata(uri, keys = list(a = 1, b = 2, c = 3, d = 4))

  expect_error(delete_metadata(list(1), keys = "a"))

  # Character method (URI)
  expect_error(delete_metadata("file:invalid-path",  keys = "a"))
  expect_error(delete_metadata(c(uri, uri), keys = "a"), label = "character string for uri path")

  arrobj <- TileDBArray$new(uri)
  origmode <- arrobj$mode

  expect_no_error(out <- delete_metadata(arrobj, keys = "c"))
  expect_true(out)
  expect_equal(arrobj$mode, origmode)
  expect_equal(unlist(arrobj$get_metadata()), c(a = 1, b = 2, d = 4))

  arrobj$reopen("WRITE")
  expect_no_error(delete_metadata(arrobj, keys = "a"))
  expect_equal(arrobj$mode, "WRITE")
  expect_equal(unlist(arrobj$get_metadata()), c(b = 2, d = 4))

  arrobj$reopen()
  expect_no_error(delete_metadata(arrobj, keys = c("b", "d")))
  expect_equal(arrobj$mode, "READ")


  arrobj$close()
  expect_no_error(delete_metadata(uri, keys = "not-key")) # no key but returns TRUE
  expect_equal(arrobj$mode, "CLOSED")
  expect_null(unlist(arrobj$get_metadata()))

  arrobj$reopen()
  arr <- arrobj$object
  origmode <- tiledb::tiledb_array_is_open_for_reading(arr)
  expect_true(origmode)
  expect_no_error(delete_metadata(arr, keys = "e"))
  expect_true(tiledb::tiledb_array_is_open_for_reading(arr))

})


test_that("'delete_metadata' methods for groups work as expected", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  create_empty_test_group(uri)

  grpobj <- TileDBGroup$new(uri)
  origmode <- grpobj$mode
  set_metadata(grpobj, keys = list(a = 1, b = 2, c= 3, d = 4))


  expect_no_error(delete_metadata(grpobj, keys = "c"))
  expect_equal(grpobj$mode, origmode)
  expect_equal(unlist(grpobj$get_metadata()), c(a = 1, b = 2, d = 4))

  grpobj$reopen("WRITE")
  expect_no_error(delete_metadata(grpobj, keys = "a"))
  expect_equal(grpobj$mode, "WRITE")
  expect_equal(unlist(grpobj$get_metadata()), c(b = 2, d = 4))

  grpobj$reopen()
  expect_no_error(delete_metadata(grpobj, keys = "boo"))
  expect_equal(grpobj$mode, "READ")

  grpobj$reopen("WRITE")
  expect_no_error(delete_metadata(grpobj, keys = "boo"))
  expect_equal(grpobj$mode, "WRITE")

  grpobj$close()
  expect_no_error(delete_metadata(uri, keys = c("b", "d")))
  expect_equal(grpobj$mode, "CLOSED")
  expect_null(unlist(grpobj$get_metadata()))

  grpobj$reopen()
  grp <- grpobj$object
  expect_equal(tiledb::tiledb_group_query_type(grp), "READ")
  expect_no_error(delete_metadata(grp, keys = "a"))
  expect_equal(tiledb::tiledb_group_query_type(grp), "READ")
  close(grp)


  expect_false(tiledb::tiledb_group_is_open(grp))
  expect_no_error(delete_metadata(grp, keys = "a"))
  expect_false(tiledb::tiledb_group_is_open(grp))
  close(grp)

})

