gc()

cfg <- tiledb::tiledb_config()

params <-  c("sm.consolidation.timestamp_start" = "100",
             "sm.consolidation.timestamp_end" = "10000")


test_that("'set_config_params()' works as expected", {

  expect_error(set_config_params(list(1), params))
  expect_error(set_config_params(cfg, unname(params)), label = "keyval should be a named vector")
  expect_error(set_config_params(cfg, c(a = 1, b = 2)), label = "keyval should be a character vector")

  expect_no_error(cfg <- set_config_params(cfg, params))

  expect_s4_class(cfg, "tiledb_config")
  expect_equal(cfg["sm.consolidation.timestamp_start"], c(sm.consolidation.timestamp_start = "100"))
  expect_equal(cfg["sm.consolidation.timestamp_end"], c(sm.consolidation.timestamp_end = "10000"))



})

test_that("'unset_config_params()' works as expected", {

  keys <- names(params)

  expect_error(unset_config_params(list(1), keys))
  expect_error(unset_config_params(cfg, c(1, 2)), label = "keys should be a character vector")

  expect_no_error(cfg <- unset_config_params(cfg, keys))

  expect_s4_class(cfg, "tiledb_config")
  expect_equal(cfg["sm.consolidation.timestamp_start"], c(sm.consolidation.timestamp_start = "0"))
  expect_equal(cfg["sm.consolidation.timestamp_end"], c(sm.consolidation.timestamp_end =  "18446744073709551615"))

  rm(cfg)
  gc()
})

rm(params)
