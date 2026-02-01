
test_that("Test 'vfs_dir_tree()'", {

  # TODO: test print/snapshot
  # skip_on_os(c("mac", "linux"))
  uri_arr <- file.path(withr::local_tempdir(), "test-arr")
  expect_error(vfs_dir_tree(uri_arr))

  create_empty_test_array(uri_arr)
  arrobj <- tdb_array(uri_arr)
  expect_no_error(capture_output(drt <- arrobj$dir_tree()))
  expect_contains(basename(drt), c("__commits", "__fragments", "__fragment_meta", "__labels",
                                     "__meta", "__schema",
                                     "__enumerations"))

  uri_grp <- file.path(withr::local_tempdir(), "test-grp")
  create_empty_test_group(uri_grp)
  grpobj <- tdb_group(uri_grp)
  expect_no_error(capture_output(drt <- grpobj$dir_tree()))
  expect_contains(basename(drt), c("__group", "__meta", "__tiledb_group.tdb"))
  # expect_snapshot(grpobj$dir_tree())

  })
