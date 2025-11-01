gc()

cfg <- tiledb::tiledb_config()

params <-  c("sm.group.timestamp_start" = "100",
             "sm.group.timestamp_end" = "10000")


test_that("'set_group_tstamps()' works as expected", {

  expect_error(set_group_timestamps(list(1)))
  expect_error(set_group_timestamps(cfg, start_time = "a"))
  expect_error(set_group_timestamps(cfg, end_time = "a"))
  expect_error(set_group_timestamps(cfg, start_time = as.POSIXct(100), end_time = as.POSIXct(1)))
  expect_error(set_group_timestamps(cfg, end_time = as.POSIXct(-1)))


  expect_no_error(cfg <- set_group_timestamps(cfg))
  expect_s4_class(cfg, "tiledb_config")

  expect_no_error(cfg <- set_group_timestamps(cfg, start_time = as.POSIXct(1), end_time = as.POSIXct(100)))
  expect_s4_class(cfg, "tiledb_config")

  expect_equal(cfg["sm.group.timestamp_start"], c(sm.group.timestamp_start = "1000"))
  expect_equal(cfg["sm.group.timestamp_end"], c(sm.group.timestamp_end = "100000"))

})

test_that("'unset_group_tstamps()' works as expected", {

  expect_no_error(cfg <- unset_group_timestamps(cfg))
  expect_s4_class(cfg, "tiledb_config")

  expect_equal(cfg["sm.group.timestamp_start"], c(sm.group.timestamp_start = "0"))
  expect_equal(cfg["sm.group.timestamp_end"], c(sm.group.timestamp_end =  "18446744073709551615"))

  rm(cfg)
  gc()
})

rm(params)
