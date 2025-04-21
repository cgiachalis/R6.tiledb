test_that("'TileDBGroup' class tests on non-existent group", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri, internal_use = "permit")

  # Should not exist on disk until created
  expect_false(dir.exists(uri)) # Any folder at this uri? Expect FALSE
  expect_false(group$exists())

  # Check errors on non-existent group
  expect_error(group$get_member("a"),
               label = "TileDB resource should be open for read or write")
  expect_error(group$open(internal_use = "permit"),
               label = "Group does not exist.")
})

test_that("'TileDBGroup' class tests on existent but empty group", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri, internal_use = "permit")

  # Create a group object on disk
  group$create(internal_use = "permit")

  # Verify that group reference is opened at WRITE mode
  expect_equal(tiledb::tiledb_group_query_type(group$object), "WRITE")


  expect_error(group$create(internal_use = "permit"), "already exists")
  expect_true(dir.exists(uri))  # Any folder at this uri? Expect TRUE
  expect_true(file.exists(file.path(uri, "__group")))
  expect_true(group$exists())
  expect_equal(group$mode(), "WRITE") # once it is created, it is opened in WRITE mode
  # debug
  # fp = file.path(uri, "__group")
  # expect_match(tiledb::tiledb_object_type(uri), "GROUP")

  group$close()
  expect_equal(group$mode(), "CLOSED")

  uri2 <- file.path(withr::local_tempdir(), "test-group2")
  group2 <- TileDBGroup$new(uri2, internal_use = "permit")

  # Create a group object on disk
  group2$create(mode = "READ", internal_use = "permit")

  # Verify that group reference is opened at READ mode
  expect_equal(tiledb::tiledb_group_query_type(group2$object), "READ")

  group2$close()
  expect_equal(group2$mode(), "CLOSED")

  # Verify that group reference is CLOSED
  expect_false(tiledb::tiledb_group_is_open(group2$object))

  # New instance
  group2_new <- TileDBGroup$new(uri2, internal_use = "permit")
  expect_no_error(group2_new$open(mode = "READ", internal_use = "permit"))
  expect_equal(group2_new$mode(), "READ")

  # Verify that group is open in READ mode
  expect_equal(tiledb::tiledb_group_query_type(group2_new$object), "READ")
  group2_new$close()
  expect_equal(group2_new$mode(), "CLOSED")

  group2_new <- TileDBGroup$new(uri2, internal_use = "permit")
  expect_no_error(group2_new$open(mode = "WRITE", internal_use = "permit"))
  expect_equal(group2_new$mode(), "WRITE")
  expect_equal(tiledb::tiledb_group_query_type(group2_new$object), "WRITE")

  group2_new$close()

  group3_new <- TileDBGroup$new(uri2, internal_use = "permit")

  expect_equal( group3_new$mode(), "CLOSED")

  # Group object reference is initialised with READ mode if CLOSED
  expect_equal(tiledb::tiledb_group_is_open(group3_new$object), TRUE)

  # Checking again, this must be in READ mode
  expect_equal(group3_new$mode(), "READ")

  # Verify that group is open in READ mode
  expect_equal(tiledb::tiledb_group_query_type(group3_new$object), "READ")

  # Check is ready only field
  expect_error(group3_new$object <- "a")

  group3_new$close()


})

test_that("'TileDBGroup' class tests accessors on empty group", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri, internal_use = "permit")

  group$create(internal_use = "permit")

  group$open(mode = "READ", internal_use = "permit")

  # Check exporters
  lst <- group$to_list()
  expect_type(lst, "list")
  expect_length(lst, 0)

  mdf <- group$get_members_df()
  expect_s3_class(mdf, "data.frame")
  expect_equal(nrow(mdf), 0)

  # Print that group is empty
  expect_snapshot(group$print())

  group$close()
})

test_that("'TileDBGroup' class tests add/remove members", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri, internal_use = "permit")

  # Step 1: Create a group object
  group$create(internal_use = "permit")
  group$close()

  # Test opening on new instance that correctly initialises the object

  group_new <- TileDBGroup$new(uri, internal_use = "permit")

  expect_no_error(group_new$open(internal_use = "permit"))
  group_new$close()

  group_new <- TileDBGroup$new(uri, internal_use = "permit")

  expect_no_error(group_new$open(mode = "WRITE", internal_use = "permit"))
  group_new$close()

  # Step 2: Create array and subgroups that will be
  #           added later to test-group
  arr_uri <- file.path(uri, "arr_a1")
  create_empty_test_array(arr_uri)
  arr1 <- TileDBArray$new(arr_uri, internal_use = "permit")

  grp_uri <- file.path(uri, "grp_g1")
  tiledb::tiledb_group_create(grp_uri)
  grp1 <- TileDBGroup$new(grp_uri, internal_use = "permit")

  grp_uri2 <- file.path(uri, "grp2")
  tiledb::tiledb_group_create(grp_uri2)
  grp2 <- TileDBGroup$new(grp_uri2, internal_use = "permit")

  # Step 3: Check arr1 and grp1 exist (but not yet members)
  group$open(mode = "READ", internal_use = "permit")

  expect_true(arr1$exists())
  expect_true(grp1$exists())
  expect_true(grp2$exists())

  expect_equal(group$count_members(), 0)

  group$close()

  # Step 4: Add array and subgroup as members
  group$open(mode = "WRITE", internal_use = "permit")

  # add array
  group$set_member(arr1, name = "arr1")

  # test that error are raised when setting a member
  expect_error(group$set_member("no-member-boo"))
  expect_error(group$set_member(arr1, name = 100))
  expect_error(group$set_member(arr1, relative = 100))

  expect_equal(group$count_members(), 1)
  expect_equal(group$get_members_df(type = "ARRAY")$type, "ARRAY")

  # add group 1
  group$set_member(grp1, name = "grp1")
  expect_equal(group$count_members(), 2)

  df1 <- group$get_members_df()
  expect_setequal(df1$type, c("ARRAY", "GROUP"))
  expect_s3_class(df1, "data.frame")

  # add group 2
  group$set_member(grp2) # name defaults to uri basename
  expect_equal(group$count_members(), 3)

  # Test member 'grp1' exists in group
  expect_true(group$member_exists("grp1"))

  # Test member 'foo-grp1' don't exists in group
  expect_false(group$member_exists("foo-grp1"))

  group$close()

  # Step 5: Read back
  group$open(mode = "READ", internal_use = "permit")
  expect_equal(group$count_members(), 3)
  expect_setequal(group$names(), c("arr1", "grp1", "grp2"))

  expect_s3_class(group$get_member("arr1"), c("TileDBArray", "R6"))
  expect_s3_class(group$get_member("grp1"), c("TileDBGroup", "R6"))
  expect_s3_class(group$get_member("grp2"), c("TileDBGroup", "R6"))

  group$close()

  # New instantiation
  # Need to close group first
  group2 <- TileDBGroup$new(uri, internal_use = "permit")
  group2$open(internal_use = "permit")

  expect_setequal(group2$names(), c("arr1", "grp1", "grp2"))

  group2$close()

  lst <- group2$to_list() # to_list() will reopen group

  # Opening existing Group, it will not instantiate member objects
  expect_true(all(vapply_lgl(lst, function(.x) is.null(.x$object))))

  group2$close()

  group2$open("READ", internal_use = "permit")
  expect_error(group2$get_member("no-member"))

  # Test member constructor
  arr1 <- group2$get_member("arr1")
  expect_s3_class(arr1, "TileDBArray")

  # get_member() instantiates members when adding to cache
  lst <- group2$to_list()
  expect_true(!is.null(lst$arr1$object))
  # but grp1 is not there because we didn't fetch it via get_member
  expect_true(is.null(lst$grp1$object))

  group2$open("WRITE", internal_use = "permit")
  grp1 <- group2$get_member("grp1")
  expect_s3_class(grp1, "TileDBGroup")
  # mode should be identical to group2
  expect_equal(grp1$mode(), "WRITE")

  # Verify group query mode
  expect_equal(tiledb::tiledb_group_query_type(grp1$object), "WRITE")

  lst <- group2$to_list()
  # get_member() instantiates members when adding to cache (again for group)
  expect_true(!is.null(lst$grp1$object))

  arr1$close(); grp1$close()

  group2$close()

  # Step 5: Remove members
  group$open(mode = "WRITE", internal_use = "permit")

  group$remove("arr1")
  expect_equal(group$count_members(), 2)

  group$remove("grp1")
  expect_equal(group$count_members(), 1)

  group$remove("grp2")
  expect_equal(group$count_members(), 0)

  group$close()

  # Read back and check
  group$open(mode = "READ", internal_use = "permit")
  expect_equal(group$count_members(), 0)
  expect_equal(length(group$names()), 0)
  group$close()

  # Test that errors are raised
  expect_error(group$remove(c("a", "b")))
  expect_error(group$get_member(c("a", "b")))
  expect_error(group$get_member("not_a_member"))
  expect_error(group$set_member(c("a", "b")))
  expect_error(group$set_member(grp1, name = c("a", "b")))
  expect_error(group$set_member(grp1, relative = c("a", "b")))

  # Print that group is closed
  expect_snapshot(group$print())
})


test_that("'TileDBGroup' class tests print method", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri, internal_use = "permit")

  group$create(internal_use = "permit")
  group$close()

  arr_uri <- file.path(uri, "arr_a1")
  create_empty_test_array(arr_uri)
  arr1 <- TileDBArray$new(arr_uri, internal_use = "permit")

  grp_uri <- file.path(uri, "grp_g1")
  tiledb::tiledb_group_create(grp_uri)
  grp1 <- TileDBGroup$new(grp_uri, internal_use = "permit")

  grp_uri2 <- file.path(uri, "grp2")
  tiledb::tiledb_group_create(grp_uri2)
  grp2 <- TileDBGroup$new(grp_uri2, internal_use = "permit")

  # Add members
  group$open(mode = "WRITE", internal_use = "permit")
  group$set_member(arr1, name = "arr1")
  group$set_member(grp1, name = "grp1")
  group$set_member(grp2)
  group$close()

  # Full print
  group$open(mode = "READ", internal_use = "permit")

  expect_snapshot(group$print())

  # Remove one by one and print
  group$open(mode = "WRITE", internal_use = "permit")
  group$remove("arr1")
  expect_snapshot(group$print())

  group$remove("grp1")
  expect_snapshot(group$print())

  group$remove("grp2")
  expect_snapshot(group$print())

  group$close()


  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri, internal_use = "permit")

  expect_snapshot(group$print())

  group$close()

  })

test_that("'TileDBGroup' class tests relative paths", {

  uri <- file.path(withr::local_tempdir(), "test-group")

  group <- TileDBGroup$new(uri, internal_use = "permit")
  group$create(internal_use = "permit")

  # Error when attempting to add a relative member that's not a subpath
  g2 <- TileDBGroup$new(
    uri = file.path(withr::local_tempdir(), "not-relative-subpath"),
    internal_use = "permit")
  g2$create(internal_use = "permit")

  expect_error(group$set_member(g2, name = "g2", relative = TRUE),
    info = "Unable to make relative path between URIs with no common parent")

  # Check NA that are sanitised to NULL and then to default values for URI and relative
  expect_error(group$set_member(g2, name = NA, relative = NA),
               info = "Unable to make relative path between URIs with no common parent")

  expect_equal(group$count_members(), 0)

  expect_no_error(group$set_member(g2, name = "g2b", relative = FALSE))
  expect_equal(group$count_members(), 1)

  group$close()
})


test_that("'TileDBGroup' class tests metatadata", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri, internal_use = "permit")

  expect_error(group$set_metadata(list(a = 1)), info = "TileDB resource should be open for write")

  group$create(internal_use = "permit") # mode is WRITE now

  md <- list(a = "a", b = 100)
  group$set_metadata(md)

  # Read all metadata while the group is still open for write
  group$reopen("WRITE")
  expect_equal(group$get_metadata("a"), "a", ignore_attr = TRUE)
  expect_equal(group$get_metadata("b"), 100, ignore_attr = TRUE)

  readmd <- group$get_metadata()
  expect_equal(readmd, md, ignore_attr = TRUE)

  group$close()

  # Read all metadata while the group is open for read
  group$open(mode = "READ", internal_use = "permit")
  readmd <- group$get_metadata()
  expect_equal(readmd, md, ignore_attr = TRUE)

  group$close()
})

