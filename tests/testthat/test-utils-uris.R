test_that("'file_path()' construction handles remote URLs", {
  expect_identical(
    file_path("path_01"),
    file.path("path_01")
  )
  expect_identical(
    file_path("path_01", "path_02"),
    file.path("path_01", "path_02")
  )
  expect_identical(
    file_path("s3://my", "bucket", fsep = "\\"),
    "s3://my/bucket"
  )
  expect_identical(
    file_path("tiledb://my", "array", fsep = "\\"),
    "tiledb://my/array"
  )
})

test_that("'uri_scheme()' retrieves URI scheme as expected", {
  expect_equal(uri_scheme("path_01/path_02"), NULL)
  expect_equal(uri_scheme("/path_01/path_02"), NULL)
  expect_equal(uri_scheme("file://path_01/path_02"), "file")
  expect_equal(uri_scheme("file:///path_01/path_02"), "file")
  expect_equal(uri_scheme("s3://my/bucket"), "s3")
  expect_equal(uri_scheme("tiledb://my/array"), "tiledb")
})

test_that("'uri_scheme_remove()' removes URI scheme as expected", {
  expect_equal(uri_scheme_remove("path_01/path_02"), "path_01/path_02")
  expect_equal(uri_scheme_remove("/path_01/path_02"), "/path_01/path_02")
  expect_equal(uri_scheme_remove("file://path_01/path_02"), "path_01/path_02")
  expect_equal(uri_scheme_remove("file:///path_01/path_02"), "/path_01/path_02")
  expect_equal(uri_scheme_remove("s3://my/bucket"), "my/bucket")
  expect_equal(uri_scheme_remove("tiledb://my/array"), "my/array")
})

test_that("'make_uri_relative()' computes relative URI paths as expected", {
  # NOTE: 'make_uri_relative' returns character vector that has also class `fs_path`;
  #        With testthat 2ed that passes but not with 3ed that checks the classes.
  trg <- "path_02"
  class(trg) <- c("fs_path", class(trg))

  expect_equal(make_uri_relative("path_01/path_02", "path_01"), trg)
  expect_equal(make_uri_relative("/path_01/path_02", "/path_01"), trg)
  expect_equal(make_uri_relative("file://path_01/path_02", "file://path_01"), trg)

  # Heterogenous schemes
  expect_equal(make_uri_relative("path_01/path_02", "file://path_01"), trg)
  expect_equal(make_uri_relative("file://path_01/path_02", "path_01"), trg)

  # Expected errors
  expect_error(
    make_uri_relative("file://path_02", "file://path_01/path_02"),
    "Unable to make relative path between URIs with no common parent"
  )

  expect_error(
    make_uri_relative("s3://path_01/path_02", "file://path_01"),
    "Unable to make relative path between URIs with different schemes"
  )
  expect_error(
    make_uri_relative("s3://path_01/path_02", "file://path_01"),
    "Unable to make relative path between URIs with different schemes"
  )
})


test_that("'check_uri()' gets scalar string", {

  expect_error(check_uri(c("a", "b")))

  })

test_that("'check_timestamp()' work OK", {

  expect_error(check_timestamp("a"))
  expect_error(check_timestamp(c(as.POSIXct(1),  as.POSIXct(12))))
  expect_error(check_timestamp(NA))

  expect_no_error(check_timestamp(as.POSIXct(1)))

})

