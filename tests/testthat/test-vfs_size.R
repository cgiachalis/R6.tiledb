test_that("Test 'vfs_size()'", {
gc()
  uri_arr <- file.path(withr::local_tempdir(), "test-arr")
  expect_error(vfs_size(uri_arr))

  create_empty_test_array(uri_arr)


  expect_no_error(s <- vfs_size(uri_arr))
  expect_type(s, "double")
  expect_s3_class(s, "vfs_size")


  # test byte format
  expect_snapshot(.byte_size_format(1023))
  expect_snapshot(.byte_size_format(1024^1))
  expect_snapshot(.byte_size_format(1024^2))
  expect_snapshot(.byte_size_format(1024^3))
  expect_snapshot(.byte_size_format(1024^4))
  expect_snapshot(.byte_size_format(1024^5))
  expect_snapshot(.byte_size_format(1024^6))
  expect_snapshot(.byte_size_format(1024^7))
})
