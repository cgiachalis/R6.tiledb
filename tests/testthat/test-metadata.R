
test_that("'metadata' methods getters/setters for arrays work as expected", {

  uri <- file.path(withr::local_tempdir(), "test-array")
  create_empty_test_array(uri)

  expect_error(metadata(list(1), "list"))

  # Character method (URI)
  expect_error(metadata("file:invalid-path", "key"))
  expect_error(metadata("file:invalid-path", "key") <- "a")
  expect_null(metadata(uri, "none"))
  expect_error(metadata(uri, c("none", "a")), label = "key should be a single character string")
  expect_error(metadata(c(uri, uri), "none"), label = "character string for uri path")

  arrobj <- TileDBArray$new(uri)
  origmode <- arrobj$mode

  expect_error(metadata(arrobj, c("key1", "key2")) <- "one")
  expect_no_error(metadata(arrobj, "key1") <- "one")
  expect_equal(arrobj$mode, "CLOSED")

  expect_error(metadata(arrobj, c("key1", "key2")))
  expect_equal(metadata(arrobj, "key1"), "one")
  expect_equal(arrobj$mode, "CLOSED")

  arrobj$open("READ")
  expect_no_error(metadata(arrobj, "key1") <- NULL)
  expect_equal(arrobj$mode, "READ")

  arrobj$reopen("WRITE")
  expect_null(metadata(arrobj, "key1"))
  expect_equal(arrobj$mode, "WRITE")

  expect_error(metadata(arrobj, "key2") <- c("one", "boo"), label = "Replacement value: `val` should be a scalar or NULL.")
  expect_equal(arrobj$mode, "WRITE")

  expect_no_error(metadata(arrobj, "key2") <- "boo")
  expect_equal(arrobj$mode, "WRITE")
  expect_equal(metadata(arrobj, "key2"), "boo")

  # a bit of reuse, uri and tiledb_array method calls TileDBArray
  arr <- arrobj$object
  expect_error(metadata(arr, c("key1", "key2")))
  expect_error(metadata(arr, c("key1", "key2")) <- "a")
  expect_error(metadata(arr, c("key1")) <- c("a", "b"))

  expect_no_error(metadata(arr, "key2") <- "boo")
  expect_equal(metadata(arrobj$object, "key2"), "boo")

  # uri
  expect_error(metadata(uri, c("key1", "key2")) <- "a")
  expect_error(metadata(uri, c("key1")) <- c("a", "b"))
  expect_no_error(metadata(uri, "key3") <- 100)
  expect_equal(metadata(uri, "key3"), 100)

})

test_that("'metadata' methods getters/setters for group work as expected", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  create_empty_test_group(uri)

  expect_error(metadata(list(1), "list"))

  # Character method (URI)
  expect_error(metadata("file:invalid-path", "key"))
  expect_error(metadata("file:invalid-path", "key") <- "a")
  expect_null(metadata(uri, "none"))
  expect_error(metadata(uri, c("none", "a")), label = "key should be a single character string")
  expect_error(metadata(c(uri, uri), "none"), label = "character string for uri path")

  grpobj <- TileDBGroup$new(uri)
  origmode <- grpobj$mode

  expect_error(metadata(grpobj, c("key1", "key2")) <- "one")
  expect_no_error(metadata(grpobj, "key1") <- "one")
  expect_equal(grpobj$mode, "CLOSED")

  expect_error(metadata(grpobj, c("key1", "key2")))
  expect_equal(metadata(grpobj, "key1"), "one")
  expect_equal(grpobj$mode, "CLOSED")

  grpobj$open("READ")
  expect_no_error(metadata(grpobj, "key1") <- NULL)
  expect_equal(grpobj$mode, "READ")

  grpobj$reopen("WRITE")
  expect_null(metadata(grpobj, "key1"))
  expect_equal(grpobj$mode, "WRITE")

  expect_error(metadata(grpobj, "key2") <- c("one", "boo"), label = "Replacement value: `val` should be a scalar or NULL.")
  expect_equal(grpobj$mode, "WRITE")

  expect_no_error(metadata(grpobj, "key2") <- "boo")
  expect_equal(grpobj$mode, "WRITE")
  expect_equal(metadata(grpobj, "key2"), "boo")

  # a bit of reuse, uri and tiledb_group method calls TileDBGroup
  grp <- grpobj$object
  expect_error(metadata(grp, c("key1", "key2")))
  expect_error(metadata(grp, c("key1", "key2")) <- "a")
  expect_error(metadata(grp, c("key1")) <- c("a", "b"))

  expect_no_error(metadata(grp, "key2") <- "boo")
  expect_equal(metadata(grpobj$object, "key2"), "boo")

  # uri
  expect_error(metadata(uri, c("key1", "key2")) <- "a")
  expect_error(metadata(uri, c("key1")) <- c("a", "b"))
  expect_no_error(metadata(uri, "key3") <- 100)
  expect_equal(metadata(uri, "key3"), 100)

})
