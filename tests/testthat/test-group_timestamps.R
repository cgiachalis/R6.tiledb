

test_that("Test 'group_timestamps()' works as expected", {

  ctx <- tiledb::tiledb_ctx(cached = FALSE)

  tz <- "Europe/London"
  Sys.setenv(TZ = tz)
  on.exit({Sys.setenv(TZ = "")})

  uri <- file.path(withr::local_tempdir(), "test-group")

  group <- TileDBGroup$new(uri)

  # errors are raised for TileDBGroup method
  expect_error(group_timestamps(group))
  group$create() # mode is WRITE now
  expect_error(group_timestamps("a"))
  expect_error(group_timestamps(group, tz = "invalid"))
  expect_error(group_timestamps(group$object, tz = "invalid"))
  expect_error(group_timestamps(group$ctx, tz = "invalid"))
  expect_error(group_timestamps(tiledb::config(group$ctx), tz = "invalid"))

  expect_s3_class(group_timestamps(group), "group_timestamps")

  group$reopen()

  end_time <- as.numeric(as.POSIXct("2020-08-20 21:00:00"))
  group$tiledb_timestamp <- set_tiledb_timestamp(as.Date("1990-01-01") , end_time)

  trg1 <- structure(list(
      timestamp_start = structure(631152000, class = c("POSIXct", "POSIXt"), tzone = ""),
      timestamp_end = structure(end_time, class = c("POSIXct", "POSIXt"),  tzone = "")
  ),
  class = "group_timestamps",
  mode = "read",
  tzone = tz,
  tdbsrc = "ctx")


  expect_equal(group_timestamps(group, tz), trg1)
  attr(trg1, "tdbsrc") <- "group_config"
  expect_equal(group_timestamps(group$object), trg1)

  attr(trg1, "mode") <- "N/A"
  attr(trg1, "tdbsrc") <- "config"
  expect_equal(group_timestamps(tiledb::config(group$ctx)), trg1)

  # revert back
  attr(trg1, "mode") <- "read"
  attr(trg1, "tdbsrc") <- "ctx"
  attr(trg1, "tzone") <- "UTC"
  expect_equal(group_timestamps(group, tz = "UTC"), trg1)

  attr(trg1, "mode") <- "N/A"
  expect_equal(group_timestamps(group$ctx, tz = "UTC"), trg1)

  group$close()

  attr(trg1, "mode") <- "closed"
  expect_equal(group_timestamps(group, tz = "UTC"), trg1)
  attr(trg1, "tdbsrc") <- "group_config"
  expect_equal(group_timestamps(group$object, tz = "UTC"), trg1)


  # Test print method
  expect_snapshot(group_timestamps(group,from = "ctx", tz = "UTC"))
  expect_snapshot(group_timestamps(group, from = "cfg", tz = "UTC"))
  group$open()
  expect_snapshot(group_timestamps(group$object, tz = "UTC"))
  expect_snapshot(group_timestamps(group$ctx, tz = "Europe/London"))
  expect_snapshot(group_timestamps(tiledb::config(group$ctx), tz = "Europe/London"))
})
