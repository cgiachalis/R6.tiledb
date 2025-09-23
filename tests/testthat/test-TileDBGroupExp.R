gc()

test_that("'TileDBGroupExp' class works as expected", {

  uri <- file.path(withr::local_tempdir(), "test-TileDBGroupExp")

  write_test_group2(uri)

  expect_no_error(grpObj <- TileDBGroupExp$new(uri = uri))

  dm <- grpObj$get_members_df()[, -1]
  colnames(dm) <- c("TYPE", "URI")
  dm <- sort_by(dm, ~URI)

  expect_equal(grpObj$walk_group(), dm, ignore_attr = TRUE)
  expect_false(grpObj$has_non_members())
  expect_equal(grpObj$non_members(), data.frame(TYPE = character(0),
                                                URI = character(0)))

  grpObj$reopen("WRITE")
  grpObj$remove("testarray2")

  # Only remove member, walking uri path should have two resources
  # a member and a non-member
  expect_equal(grpObj$walk_group(), dm, ignore_attr = TRUE)
  expect_true(grpObj$has_non_members())
  expect_equal( grpObj$non_members(), dm[2, ], ignore_attr = TRUE)

  # delete group layer with recursing deleting objects
  expect_invisible(obj <- grpObj$delete_group())
  expect_s3_class(obj, class = "TileDBGroupExp")
  rm(obj)
  expect_equal(grpObj$object_type, "INVALID")
  expect_error(grpObj$reopen(), label = "object does not exist.")

  # check we have not deleted resources under uri path
  dm2 <- tiledb::tiledb_object_walk(uri)
  expect_equal(dm2, dm, ignore_attr = TRUE)
  rm(grpObj)


  # New group to test recursive delete
  uri <- file.path(withr::local_tempdir(), "test-TileDBGroupExp")

  write_test_group2(uri)

  expect_no_error(grpObj <- tdb_group(uri = uri))

  expect_no_error(grpObj$delete_group(recursive = TRUE))
  dm2 <- tiledb::tiledb_object_walk(uri)
  expect_equal(dm2, data.frame(TYPE = character(0), URI = character(0)))
  rm(grpObj)
  gc()

})

