

test_that("'TileDBGroup' class tests on non-existent group", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)

  # Should not exist on disk until created
  expect_false(dir.exists(uri)) # Any folder at this uri? Expect FALSE
  expect_false(group$exists())
  expect_equal(group$object_type, "INVALID")

  # Check errors on non-existent group
  expect_error(group$get_member("a"),
               label = "TileDB resource should be open for read or write")
  expect_error(group$open(),
               label = "Group does not exist.")

  expect_snapshot(group$print())

  rm(group)
})

test_that("'TileDBGroup' class tests on existent but empty group", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)

  # Create a group object on disk
  expect_invisible(group$create())

  # Verify that group reference is opened at WRITE mode
  expect_equal(tiledb::tiledb_group_query_type(group$object), "WRITE")
  expect_equal(group$object_type, "GROUP")

  expect_error(group$create(), "already exists")
  expect_true(dir.exists(uri))  # Any folder at this uri? Expect TRUE
  expect_true(file.exists(file.path(uri, "__group")))
  expect_true(group$exists())
  expect_equal(group$mode, "WRITE") # once it is created, it is opened in WRITE mode
  # debug
  # fp = file.path(uri, "__group")
  # expect_match(tiledb::tiledb_object_type(uri), "GROUP")

  expect_error(group$object <- "a", label = '"object" is a read-only field')
  expect_error(group$members <- "a", label = '"members" is a read-only field')


  group$close()
  expect_equal(group$mode, "CLOSED")

  rm(group)

  ## Open on WRITE from init and with tdb_group()

  expect_no_error(group_new <- tdb_group(uri, mode = "WRITE"))
  expect_equal(group_new$mode, "WRITE")

  # Verify that group reference is opened at WRITE mode
  expect_equal(tiledb::tiledb_group_query_type(group_new$object), "WRITE")

  ## Ensure timestamp is ignored when opening on WRITE from init
  expect_no_error(group_new <- tdb_group(uri, mode = "WRITE",
                                         tiledb_timestamp = as.Date("1990-01-01")))
  expect_equal(group_new$mode, "WRITE")

  # Verify that group reference is opened at WRITE mode
  expect_equal(tiledb::tiledb_group_query_type(group_new$object), "WRITE")

  # Εnsure to group timestamp_end was set both in ctx and group object
  cfg <- tiledb::config(group_new$ctx)
  expect_equal(unname(cfg["sm.group.timestamp_end"]), "18446744073709551615")
  cfg <- tiledb::tiledb_group_get_config(group_new$object)
  expect_equal(unname(cfg["sm.group.timestamp_end"]), "18446744073709551615")

  group_new$close()
  rm(group_new)
  # ---


  uri2 <- file.path(withr::local_tempdir(), "test-group2")
  group2 <- TileDBGroup$new(uri2)

  # Create a group object on disk
  group2$create(mode = "READ")

  # Verify that group reference is opened at READ mode
  expect_equal(tiledb::tiledb_group_query_type(group2$object), "READ")

  group2$close()
  expect_equal(group2$mode, "CLOSED")

  # Verify that group reference is CLOSED
  expect_false(tiledb::tiledb_group_is_open(group2$object))

  rm(group2)

  # New instance
  group2_new <- TileDBGroup$new(uri2)
  expect_no_error(group2_new$open(mode = "READ"))
  expect_equal(group2_new$mode, "READ")

  # Verify that group is open in READ mode
  expect_equal(tiledb::tiledb_group_query_type(group2_new$object), "READ")
  group2_new$close()
  expect_equal(group2_new$mode, "CLOSED")
  expect_false(tiledb::tiledb_group_is_open(group2_new$object))

  group2_new <- TileDBGroup$new(uri2)
  expect_equal(group2_new$mode, "CLOSED")

  expect_no_error(group2_new$open(mode = "WRITE"))
  expect_equal(group2_new$mode, "WRITE")
  expect_equal(tiledb::tiledb_group_query_type(group2_new$object), "WRITE")

  rm(group2_new)

  group3_new <- TileDBGroup$new(uri2)

  expect_equal( group3_new$mode, "CLOSED")

  # Group object reference is initialised with READ mode if CLOSED (active binding)
  expect_equal(tiledb::tiledb_group_is_open(group3_new$object), TRUE)

  # Checking again, this must be in READ mode
  expect_equal(group3_new$mode, "READ")

  # Verify that group is open in READ mode
  expect_equal(tiledb::tiledb_group_query_type(group3_new$object), "READ")

  # Check is ready only field
  expect_error(group3_new$object <- "a")

  rm(group3_new)
})


test_that("'TileDBGroup' class tests accessors on empty group", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)

  group$create()

  group$reopen(mode = "READ")

  # Check exporters
  lst <- group$members
  expect_type(lst, "list")
  expect_length(lst, 0)

  mdf <- group$get_members_df()
  expect_s3_class(mdf, "data.frame")
  expect_equal(nrow(mdf), 0)

  # Print that group is empty
  expect_snapshot(group$print())

  # Raw dump
  expect_snapshot(group$dump("Test TileDB"))
  # Raw dump (no header)
  expect_snapshot(group$dump(NULL))

  rm(group)
})

test_that("'TileDBGroup' class tests add/remove members", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)

  # Step 1: Create a group object
  group$create()
  group$close()

  # Test opening on new instance that correctly initialises the object

  group_new0 <- TileDBGroup$new(uri)
  expect_invisible(group_new0$open())
  rm(group_new0)

  group_new <- TileDBGroup$new(uri)
  expect_invisible(group_new$open(mode = "WRITE"))
  rm(group_new)

  # Step 2: Create array and subgroups that will be
  #           added later to test-group
  arr_uri <- file.path(uri, "arr_a1")
  create_empty_test_array(arr_uri)
  arr1 <- TileDBArray$new(arr_uri)

  grp_uri <- file.path(uri, "grp_g1")
  tiledb::tiledb_group_create(grp_uri)
  grp1 <- TileDBGroup$new(grp_uri)

  grp_uri2 <- file.path(uri, "grp2")
  tiledb::tiledb_group_create(grp_uri2)
  grp2 <- TileDBGroup$new(grp_uri2)

  # Step 3: Check arr1 and grp1 exist (but not yet members)
  group$open(mode = "READ")

  expect_true(arr1$exists())
  expect_true(grp1$exists())
  expect_true(grp2$exists())

  expect_equal(group$count_members(), 0)

  group$close()

  # Step 4: Add array and subgroup as members
  group$open(mode = "WRITE")

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

  # Verify we have 3 members on disk
  group$reopen()
  expect_identical(tiledb::tiledb_group_member_count(group$object), 3)


  # Test member 'grp1' exists in group
  expect_true(group$member_exists("grp1"))

  # Test member 'foo-grp1' don't exists in group
  expect_false(group$member_exists("foo-grp1"))

  group$close()
  rm(grp1)
  rm(grp2)
  rm(arr1)

  # Step 5: Read back
  group$open(mode = "READ")
  expect_equal(group$count_members(), 3)
  expect_setequal(group$names(), c("arr1", "grp1", "grp2"))

  expect_s3_class(group$get_member("arr1"), c("TileDBArray", "R6"))
  expect_s3_class(group$get_member("grp1"), c("TileDBGroup", "R6"))
  expect_s3_class(group$get_member("grp2"), c("TileDBGroup", "R6"))

  group$close()

  # New instantiation
  # Need to close group first
  group2 <- TileDBGroup$new(uri)
  group2$open()

  expect_setequal(group2$names(), c("arr1", "grp1", "grp2"))

  group2$close()

  lst <- group2$members # this will open group if it is in close mode

  # Opening existing Group, it will not instantiate member objects
  expect_true(all(vapply_lgl(lst, function(.x) is.null(.x$object))))

  group2$close()

  group2$open("READ")
  expect_error(group2$get_member("no-member"))

  # Test member constructor
  arr1 <- group2$get_member("arr1")
  expect_s3_class(arr1, "TileDBArray")

  # get_member() instantiates members when adding to cache
  lst <- group2$members
  expect_true(!is.null(lst$arr1$object))
  # but grp1 is not there because we didn't fetch it via get_member
  expect_true(is.null(lst$grp1$object))

  group2$reopen("WRITE")
  grp1 <- group2$get_member("grp1")
  expect_s3_class(grp1, "TileDBGroup")
  # mode should be identical to group2
  expect_equal(grp1$mode, "WRITE")

  # Verify group query mode
  expect_equal(tiledb::tiledb_group_query_type(grp1$object), "WRITE")

  lst <- group2$members
  # get_member() instantiates members when adding to cache (again for group)
  expect_true(!is.null(lst$grp1$object))

  # Step 5: Remove members
  group$open(mode = "WRITE")

  group$remove("arr1")
  expect_equal(group$count_members(), 2)

  group$remove("grp1")
  expect_equal(group$count_members(), 1)

  group$remove("grp2")
  expect_equal(group$count_members(), 0)

  group$close()

  # Read back and check
  group$open(mode = "READ")
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

  rm(group)
  rm(grp1)
  rm(group2)
  rm(arr1)

})


test_that("'TileDBGroup' class tests delete members", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)

  # Step 1: Create a group object
  group$create()
  group$close()

  # Step 1: Create arrays and subgroups that will be
  #           added later to test-group
  arr_uri <- file.path(uri, "arr_a1")
  create_empty_test_array(arr_uri)
  arr1 <- TileDBArray$new(arr_uri)

  arr_uri2 <- file.path(uri, "arr_a2")
  create_empty_test_array(arr_uri2)
  arr2 <- TileDBArray$new(arr_uri2)

  grp_uri <- file.path(uri, "grp_g1")
  tiledb::tiledb_group_create(grp_uri)
  grp1 <- TileDBGroup$new(grp_uri)

  grp_uri2 <- file.path(uri, "grp2")
  tiledb::tiledb_group_create(grp_uri2)
  grp2 <- TileDBGroup$new(grp_uri2)

  # Step 2: Add array and subgroup as members
  group$open(mode = "WRITE")

  # add array 1
  group$set_member(arr1, name = "arr1")

  # add array 2
  group$set_member(arr2, name = "arr2")

  # add group 1
  group$set_member(grp1, name = "grp1")

  # add group 2
  group$set_member(grp2) # name defaults to uri basename
  expect_equal(group$count_members(), 4)

  # close to write members on disk
  group$close()

  rm(arr1)
  rm(arr2)
  rm(grp1)
  rm(grp2)

  # Step 5: Delete members

  # open new instance with no cached members
  group2 <- TileDBGroup$new(uri)
  group2$reopen(mode = "WRITE")

  # Delete one member so we can open the previous instance to check if
  # it will pick the deletion
  group2$delete("arr2")
  group2$close()

  # Now the initial instance 'group' on reopening should have 3 members
  # as from 'group2' we deleted 'arr2' member
  group$open()
  expect_equal(group$count_members(), 3)
  expect_false(group$member_exists("arr2"))
  group$close()
  rm(group)

  group2$reopen("WRITE")
  expect_error(group2$delete("Bob"), label = "No member named `bob` found.")

  group2$delete("arr1")
  expect_equal(group2$count_members(), 2)

  group2$delete("grp1")
  expect_equal(group2$count_members(), 1)

  group2$reopen()
  expect_equal(tiledb::tiledb_group_member_count(group2$object), 1)

  # Verify on disk we have only 'grp2' GROUP
  result <- tiledb::tiledb_object_ls(group2$uri)
  expect_equal(result$TYPE, "GROUP")
  expect_equal(basename(result$URI), "grp2")

  rm(group2)

})

test_that("'TileDBGroup' class tests print method", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)

  group$create()
  group$close()

  arr_uri <- file.path(uri, "arr_a1")
  create_empty_test_array(arr_uri)
  arr1 <- TileDBArray$new(arr_uri)

  grp_uri <- file.path(uri, "grp_g1")
  tiledb::tiledb_group_create(grp_uri)
  grp1 <- TileDBGroup$new(grp_uri)

  grp_uri2 <- file.path(uri, "grp2")
  tiledb::tiledb_group_create(grp_uri2)
  grp2 <- TileDBGroup$new(grp_uri2)

  # Add members
  group$open(mode = "WRITE")
  group$set_member(arr1, name = "arr1")
  group$set_member(grp1, name = "grp1")
  group$set_member(grp2)
  group$close()

  # Full print
  group$open(mode = "READ")

  # Raw dump
  expect_snapshot(group$dump("Test Dump TileDB with members"))
  expect_snapshot(group$print())

  # Remove one by one and print
  group$reopen(mode = "WRITE")
  group$remove("arr1")
  expect_snapshot(group$print())

  group$remove("grp1")
  expect_snapshot(group$print())

  group$remove("grp2")
  expect_snapshot(group$print())

  rm(group)

  # Test snapshot print for empty group
  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)

  expect_snapshot(group$print())

  rm(group)
  rm(arr1)
  rm(grp1)
  rm(grp2)

  })

test_that("'TileDBGroup' class tests relative paths", {

  uri <- file.path(withr::local_tempdir(), "test-group")

  group <- TileDBGroup$new(uri)
  group$create()

  # Error when attempting to add a relative member that's not a subpath
  g2 <- TileDBGroup$new(
    uri = file.path(withr::local_tempdir(), "not-relative-subpath"))
  g2$create()

  expect_error(group$set_member(g2, name = "g2", relative = TRUE),
    info = "Unable to make relative path between URIs with no common parent")

  # Check NA that are sanitised to NULL and then to default values for URI and relative
  expect_error(group$set_member(g2, name = NA, relative = NA),
               info = "Unable to make relative path between URIs with no common parent")

  expect_equal(group$count_members(), 0)

  expect_no_error(group$set_member(g2, name = "g2b", relative = FALSE))
  expect_equal(group$count_members(), 1)

  rm(group)
  rm(g2)

})

# Do not remove.

test_that("'TileDBGroup' test active binding tiledb_timestamp", {

  uri <- file.path(withr::local_tempdir(), "test-group")

  group <- TileDBGroup$new(uri)
  group$create(mode = "READ")

  expect_no_error(group$tiledb_timestamp <- NULL)
  expect_s3_class(group$tiledb_timestamp, "tiledb_timestamp")

  expect_no_error(group$tiledb_timestamp <- 10)
  expect_equal(group$tiledb_timestamp, set_tiledb_timestamp(end_time = 10))

  expect_no_error(group$tiledb_timestamp <- c(0, 10))
  expect_equal(group$tiledb_timestamp, set_tiledb_timestamp(0, end_time = 10))

  expect_no_error(group$tiledb_timestamp <- "1990-01-01")
  expect_equal(group$tiledb_timestamp, set_tiledb_timestamp(end_time =  "1990-01-01"))

  expect_no_error(group$tiledb_timestamp <- as.POSIXct(10, tz = "UTC"))
  expect_equal(group$tiledb_timestamp, set_tiledb_timestamp(end_time = as.POSIXct(10)))

  # WRITE mode ----
  group$reopen("WRITE")
  expect_equal(group$tiledb_timestamp, set_tiledb_timestamp(), label = "reset on reopening in write mode")

  group$tiledb_timestamp <- as.POSIXct(10)
  expect_equal(group$tiledb_timestamp, set_tiledb_timestamp(), label = "change tstamps has no effect in write mode")

  # End WRITE mode ---

  group$reopen()

  expect_equal(group$tiledb_timestamp, set_tiledb_timestamp(), label = "reset on reopening in read mode")

  group$close()
  group$tiledb_timestamp <- as.POSIXct(10)
  expect_true(group$is_open())
  expect_error(group$open(), label = "Assigning a timestamp trigger reopening")

  ts <- set_tiledb_timestamp(start_time = as.Date("1990-01-01"), end_time = as.Date("2000-01-01"))
  expect_no_error(group$tiledb_timestamp <- ts)
  expect_equal(group$tiledb_timestamp, ts)

  expect_error(group$tiledb_timestamp <- "bob", label = "character string is not in a standard unambiguous format")
  expect_error(group$tiledb_timestamp <- c(1, 3, 3), label = "Invalid 'tiledb_timestamp' input")
  expect_error(group$tiledb_timestamp <- numeric(0), label = "Invalid 'tiledb_timestamp' input")

  rm(group)
})


test_that("'TileDBGroup' class tests time-traveling", {

  uri <- file.path(withr::local_tempdir(), "test-group-timetravel")
  res <- write_test_group(uri)

  group <- TileDBGroup$new(uri)

  group$open()

  expect_equal(group$count_members(), 2)
  expect_equal(group$names(), c("testarray2", "testarray1"))

  group$tiledb_timestamp <- 0
  expect_equal(group$count_members(), 0)

  group$reopen("WRITE")
  expect_equal(group$count_members(), 2, label = "WRITE mode reset time stamps")

  group$tiledb_timestamp <- 0
  expect_equal(group$count_members(), 2, label = "WRITE mode no effect of time stamps")

  group$reopen()
  group$tiledb_timestamp <- res$group_ts$t1
  expect_equal(group$count_members(), 1)
  expect_equal(group$names(), "testarray1")
  marr1 <- group$get_member("testarray1")

  # tiledb timestamp propagates to members
  expect_equal(marr1$tiledb_timestamp, group$tiledb_timestamp)

  expect_error(group$get_member("testarray2"),
               label = "arr2 member doesn't exist as it falls outside time range")

  # reset time stamps
  group$tiledb_timestamp <- NULL
  expect_equal(group$count_members(), 2)
  expect_equal(tiledb::tiledb_group_member_count(group$object), 2)

  # remove member arr1 that was added @ t1
  group$reopen("WRITE")
  group$remove("testarray1")
  group$reopen()
  group$tiledb_timestamp <- res$group_ts$t1
  expect_equal(group$count_members(), 1)
  expect_equal(group$names(), "testarray1")
  name <- tiledb::tiledb_group_member(group$object, 0)[3]
  expect_equal(name, "testarray1")

  # reset again, check we left with arr2
  group$tiledb_timestamp <- NULL
  expect_equal(group$count_members(), 1)
  expect_equal(group$names(), "testarray2")
  name <- tiledb::tiledb_group_member(group$object, 0)[3]
  expect_equal(name, "testarray2")

  # delete arr2 and time travel
  group$reopen("WRITE")
  group$delete("testarray2")
  expect_equal(group$count_members(), 0)
  group$reopen()
  expect_equal(tiledb::tiledb_group_member_count(group$object), 0)
  expect_equal(group$names(), character(0))

  group$tiledb_timestamp <- res$group_ts$t2
  # Time travel while we deleted arr2 and remove arr1;
  # both should be reflected as there were members
  expect_equal(group$count_members(), 2)
  expect_equal(tiledb::tiledb_group_member_count(group$object), 2)
  expect_equal(group$names(), c("testarray2", "testarray1"))

  # but arr2 doesn't exist
  expect_error(group$get_member("testarray2"))
  objw <- tiledb::tiledb_object_walk(uri)

  # on disk we have arr1 which back then was a member, check
  expect_equal(nrow(objw), 1)
  expect_equal(basename(objw$URI), "testarray1")

  # timestamp reset and check once again
  group$tiledb_timestamp <- NULL
  expect_equal(group$names(),  character(0))
  expect_equal(group$count_members(), 0)
  expect_equal(tiledb::tiledb_group_member_count(group$object), 0)

  # check disk again we a non member arr1
  objw <- tiledb::tiledb_object_walk(uri)
  # on disk we have arr1 which back then was a member
  expect_equal(nrow(objw), 1)
  expect_equal(basename(objw$URI), "testarray1")

  rm(group)
  rm(marr1)

})


test_that("'TileDBGroup' class tests metadata print method", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)

  group$create() # mode is WRITE now

  expect_snapshot(group$get_metadata())

  rm(group)
})


test_that("'TileDBGroup' class tests metadata", {

  uri <- file.path(withr::local_tempdir(), "test-group")
  group <- TileDBGroup$new(uri)

  expect_error(group$set_metadata(list(a = 1)), info = "TileDB resource should be open for write")

  group$create() # mode is WRITE now

  md <- list(a = "a", b = 100)
  group$set_metadata(md)
  expect_snapshot(group$get_metadata())

  # Read all metadata while the group is still open for write
  group$reopen("WRITE")
  expect_equal(group$get_metadata("a"), "a", ignore_attr = TRUE)
  expect_equal(group$get_metadata("b"), 100, ignore_attr = TRUE)

  readmd <- group$get_metadata()
  expect_equal(readmd, md, ignore_attr = TRUE)

  group$close()

  # Read all metadata while the group is open for read
  group$open(mode = "READ")
  readmd <- group$get_metadata()
  expect_equal(readmd, md, ignore_attr = TRUE)

  group$reopen("WRITE")
  new_md <- setNames(as.list(1:20), paste0(letters[1:20], "v"))
  group$set_metadata(new_md)
  expect_snapshot(group$get_metadata())

  rm(group)
})
