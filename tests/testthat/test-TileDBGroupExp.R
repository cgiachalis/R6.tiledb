
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

  # Test active field 'size'
  expect_error(arrObj$size <- "boo")
  expect_type(grpObj$size, "double")
  expect_s3_class(grpObj$size, "vfs_size")

  grpObj$reopen("WRITE")
  grpObj$remove("testarray2")

  # Only remove member, walking uri path should have two resources
  # a member and a non-member
  expect_equal(grpObj$walk_group(), dm, ignore_attr = TRUE)
  expect_true(grpObj$has_non_members())
  expect_equal( grpObj$non_members(), dm[2, ], ignore_attr = TRUE)

  # prune non members
  expect_no_error(pruned_uri <- grpObj$prune_non_members())
  expect_equal(pruned_uri, dm[2, "URI"])

  # delete group layer with recursive deleting objects
  expect_invisible(obj <- grpObj$delete_group())
  expect_s3_class(obj, class = "TileDBGroupExp")
  rm(obj)
  expect_equal(grpObj$object_type, "INVALID")
  expect_error(grpObj$reopen(), label = "object does not exist.")

  # check we have not deleted resources under uri path
  dm2 <- tiledb::tiledb_object_walk(uri)
  expect_equal(dm2, dm[1, ], ignore_attr = TRUE)
  rm(grpObj)


  # New group to test recursive delete
  uri <- file.path(withr::local_tempdir(), "test-TileDBGroupExp")

  write_test_group2(uri)

  expect_no_error(grpObj <- tdb_group(uri = uri))

  expect_no_error(grpObj$delete_group(recursive = TRUE))
  dm2 <- tiledb::tiledb_object_walk(uri)
  expect_equal(dm2, data.frame(TYPE = character(0), URI = character(0)))
  rm(grpObj)

})

test_that("Test 'tdb_group_create()'", {

  uri <- file.path(withr::local_tempdir(), "test-TileDBGroupExp")

  expect_no_error(grpobj <- tdb_group_create(uri))
  expect_s3_class(grpobj, class = "TileDBGroupExp")
  expect_equal(grpobj$mode, "WRITE")
  # in case we do sth funny
  expect_equal(grpobj$count_members(), 0)

  # Verify that group reference is opened at WRITE mode
  expect_equal(tiledb::tiledb_group_query_type(grpobj$object), "WRITE")
  rm(grpobj)
  })
