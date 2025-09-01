
cfg <- tiledb::tiledb_config()

params <-  c("sm.consolidation.timestamp_start" = "100",
             "sm.consolidation.timestamp_end" = "10000")


test_that("'set_consolidation_tstamps()' works as expected", {

  expect_error(set_consolidation_tstamps(list(1)))
  expect_error(set_consolidation_tstamps(cfg, start_time = "a"))
  expect_error(set_consolidation_tstamps(cfg, end_time = "a"))
  expect_error(set_consolidation_tstamps(cfg, start_time = as.POSIXct(100), end_time = as.POSIXct(1)))
  expect_error(set_consolidation_tstamps(cfg, end_time = as.POSIXct(-1)))


  expect_no_error(cfg <- set_consolidation_tstamps(cfg))
  expect_s4_class(cfg, "tiledb_config")

  expect_no_error(cfg <- set_consolidation_tstamps(cfg, start_time = as.POSIXct(1), end_time = as.POSIXct(100)))
  expect_s4_class(cfg, "tiledb_config")

  expect_equal(cfg["sm.consolidation.timestamp_start"], c(sm.consolidation.timestamp_start = "1000"))
  expect_equal(cfg["sm.consolidation.timestamp_end"], c(sm.consolidation.timestamp_end = "100000"))

})

test_that("'unset_consolidation_tstamps()' works as expected", {

  expect_no_error(cfg <- unset_consolidation_tstamps(cfg))
  expect_s4_class(cfg, "tiledb_config")

  expect_equal(cfg["sm.consolidation.timestamp_start"], c(sm.consolidation.timestamp_start = "0"))
  expect_equal(cfg["sm.consolidation.timestamp_end"], c(sm.consolidation.timestamp_end =  "18446744073709551615"))

})

rm(cfg)
rm(params)
