gc()

is_mirai_installed <- requireNamespace("mirai", quietly = TRUE)

if (is_mirai_installed) {
  on.exit({mirai::daemons(NULL, dispatcher = FALSE, autoexit = tools::SIGINT, .compute = "r6.tiledb")})
}


test_that("'TileDBArrayExp' class works as expected", {

  uri <- file.path(withr::local_tempdir(), "test-TileDBArrayExp")

  # write test array on disk
  write_test_array(uri)

  expect_no_error(arrObj <- TileDBArrayExp$new(uri = uri))

  # Active --------------------------------------------------------------

  # active bindings are ready-only fields
  expect_error(arrObj$is_sparse  <- "boo")
  expect_error(arrObj$schema_version  <- "boo")
  expect_error(arrObj$fragments_object <- "boo")

  # active bindings are OK
  expect_equal(arrObj$is_sparse, TRUE)
  expect_type(arrObj$schema_version, "integer")
  expect_s3_class(arrObj$fragments_object, c("TileDBFragments", "R6"))


  # Methods -------------------------------------------------------------

  ## enums
  expect_true(arrObj$any_enums())
  expect_equal(arrObj$enum_columns(), "Admit")
  expect_equal(arrObj$enum_levels("Admit"), c("Admitted", "Rejected"))
  expect_error(arrObj$enum_levels("Dept"), label = "`Dept` is not attribute.")
  expect_error(arrObj$enum_levels("Freq"), label = "`Freq` attribute is not factor (enum).")
  expect_equal(arrObj$has_enumeration(), c(Admit = TRUE, Freq = FALSE))

  ## frags
  expect_no_error(dump_str <- capture_output_lines(arrObj$frag_dump()))
  expect_equal(dump_str[[1]], "- Fragment num: 3")
  expect_identical(arrObj$frag_num(), 3)
  expect_equal(arrObj$frag_to_vacuum(), data.frame(Fragment = character(0),
                                                   start_timestamp = numeric(0),
                                                   end_timestamp = numeric(0),
                                                   URI = character(0)))
  expect_no_error(dfrags <- arrObj$frag_uris())
  expect_s3_class(dfrags, "data.frame")
  expect_equal(dim(dfrags), c(3,4))
  expect_equal(colnames(dfrags), c("Fragment", "start_timestamp", "end_timestamp", "URI"))

  ## consolidate and vacuum

  expect_error(arrObj$consolidate(mode = "fragments", cfg = "nope"), label = "cfg should be 'tiledb_config'")
  expect_true(arrObj$consolidate(mode = "fragments")) # default
  # testthat::skip()
  expect_identical(arrObj$frag_num(), 1)
  expect_no_error(dfrags <- arrObj$frag_to_vacuum())
  expect_s3_class(dfrags, "data.frame")
  expect_equal(dim(dfrags), c(3,4))
  expect_equal(colnames(dfrags), c("Fragment", "start_timestamp", "end_timestamp", "URI"))

  expect_error(arrObj$vacuum(mode = "fragments", cfg = "nope"), label = "cfg should be'tiledb_config'")
  expect_true(arrObj$vacuum(mode = "fragments")) # default
  expect_equal(arrObj$frag_to_vacuum(), data.frame(Fragment = character(0),
                                                   start_timestamp = numeric(0),
                                                   end_timestamp = numeric(0),
                                                   URI = character(0)))
  ## drop attribute
  expect_invisible(arrObj$drop_attribute("Freq"))
  expect_equal(arrObj$colnames(),  c("Dept", "Gender", "Admit"))
  expect_error(arrObj$drop_attribute("Freq"), label = "`Freq` is not an attribute.")

  expect_null(arrObj$schema_upgrade())
  rm(arrObj)
  gc()

})


test_that("Test '$consolidate', '$consolidate_async' and '$vacuum' methods", {

  uri <- file.path(withr::local_tempdir(), "test-TileDBArrayExp")

  write_test_array_tstamps(uri, 4)

  expect_invisible(arrobj <- tdb_array(uri))
  expect_s3_class(arrobj, "TileDBArrayExp")

  expect_equal(arrobj$frag_num(), 4) # sanity check

  ts <- function(x) {
    as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
  }

  # get config from tiledb cache
  origcfg <- tiledb::config(tiledb::tiledb_get_context())

  # consolidate
  trg_range <- c(ts(3), ts(4))
  expect_true(arrobj$consolidate(mode = "fragments", start_time = trg_range[1], end_time = trg_range[2]))

  # check we didn't alter consolidation config values from tiledb cache
  cfg <- tiledb::config(tiledb::tiledb_get_context())
  expect_equal(cfg["sm.consolidation.timestamp_start"], origcfg["sm.consolidation.timestamp_start"])
  expect_equal(cfg["sm.consolidation.timestamp_end"], origcfg["sm.consolidation.timestamp_end"])

  # Visual check, see frag #4
  # options("digits.secs" = 6)
  # arrobj$frag_uris()

  expect_equal(nrow(arrobj$frag_to_vacuum()), 2)
 finfo <- arrobj$fragments_object$fragment_info
 ts_range <- tiledb::tiledb_fragment_info_get_timestamp_range(finfo, arrobj$frag_num() - 1)
 ts_range <-  as.POSIXct(ts_range, tz = "UTC")
 expect_equal(ts_range, trg_range)

 # consolidate async

 if (is_mirai_installed) {

 # get config from tiledb cache
 origcfg <- tiledb::config(tiledb::tiledb_get_context())
 expect_error(arrobj$consolidate_async(cfg = "nope"), label = "cfg should be 'tiledb_config'")

 trg_range <- c(ts(2), ts(4))
 expect_true(arrobj$consolidate_async(start_time = trg_range[1], end_time = trg_range[2])[])

 # check we didn't alter consolidation config values from tiledb cache
 cfg <- tiledb::config(tiledb::tiledb_get_context())
 expect_equal(cfg["sm.consolidation.timestamp_start"], origcfg["sm.consolidation.timestamp_start"])
 expect_equal(cfg["sm.consolidation.timestamp_end"], origcfg["sm.consolidation.timestamp_end"])


 expect_equal(nrow(arrobj$frag_to_vacuum()), 4)
 finfo <- arrobj$fragments_object$fragment_info
 ts_range <- tiledb::tiledb_fragment_info_get_timestamp_range(finfo, arrobj$frag_num() - 1)
 ts_range <- as.POSIXct(ts_range, tz = "UTC")
 expect_equal(ts_range, trg_range)

 # vacuum
 expect_error(arrobj$vacuum(cfg = "nope"), label = "cfg should be 'tiledb_config'")

 origcfg <- tiledb::config(tiledb::tiledb_get_context())

 expect_true(arrobj$vacuum(mode = "commits"))

 # check we didn't alter vacuum mode config values from tiledb cache
 cfg <- tiledb::config(tiledb::tiledb_get_context())
 expect_equal( cfg["sm.vacuum.mode"],  origcfg["sm.vacuum.mode"])

 expect_true(arrobj$vacuum(mode = "fragments"))
 expect_equal(nrow(arrobj$frag_to_vacuum()), 0)

 }

 rm(arrobj)
 rm(cfg)
 rm(origcfg)
 rm(finfo)
 gc()

})

test_that("Test '$vacuum_async' method", {

  skip_if_not_installed("mirai")

  uri <- file.path(withr::local_tempdir(), "test-TileDBArrayExp")
  write_test_array_tstamps(uri, 2)
  arrobj <- tdb_array(uri)
  arrobj$consolidate()

  expect_error(arrobj$vacuum_async(cfg = "nope"), label = "cfg should be 'tiledb_config'")

  origcfg <- tiledb::config(tiledb::tiledb_get_context())
  expect_true(arrobj$vacuum_async(mode = "commits")[])
  # check we didn't alter vacuum mode config values from tiledb cache
  cfg <- tiledb::config(tiledb::tiledb_get_context())
  expect_equal( cfg["sm.vacuum.mode"],  origcfg["sm.vacuum.mode"])

  expect_true(arrobj$vacuum_async()[])
  expect_equal(nrow(arrobj$frag_to_vacuum()), 0)

  rm(arrobj)
  rm(cfg)
  rm(origcfg)
  gc()
})


test_that("Test '$consolidate_and_vacuum' method", {

  uri <- file.path(withr::local_tempdir(), "test-TileDBArrayExp")
  write_test_array_tstamps(uri, 3)
  arrobj <- tdb_array(uri)

  expect_error(arrobj$consolidate_and_vacuum(cfg = "nope"), label = "cfg should 'tiledb_config'")

  ts <- function(x) {
    as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
  }

  # get config from tiledb cache
  origcfg <- tiledb::config(tiledb::tiledb_get_context())

  # consolidate
  trg_range <- c(ts(1), ts(3))
  expect_true(arrobj$consolidate_and_vacuum(start_time = trg_range[1], end_time = trg_range[2])[])

  # check we didn't alter consolidation config values from tiledb cache
  cfg <- tiledb::config(tiledb::tiledb_get_context())
  expect_equal(cfg["sm.consolidation.timestamp_start"], origcfg["sm.consolidation.timestamp_start"])
  expect_equal(cfg["sm.consolidation.timestamp_end"], origcfg["sm.consolidation.timestamp_end"])

  expect_equal(nrow(arrobj$frag_to_vacuum()), 0)
  expect_equal(nrow(arrobj$frag_uris()), 1)
  finfo <- arrobj$fragments_object$fragment_info
  ts_range <- tiledb::tiledb_fragment_info_get_timestamp_range(finfo, arrobj$frag_num() - 1)
  ts_range <- as.POSIXct(ts_range, tz = "UTC")
  expect_equal(ts_range, trg_range)

  rm(arrobj)
  rm(cfg)
  rm(origcfg)
  rm(finfo)
  gc()

})

test_that("Test '$consolidate_and_vacuum_async' method", {

  skip_if_not_installed("mirai")

  uri <- file.path(withr::local_tempdir(), "test-TileDBArrayExp")
  write_test_array_tstamps(uri, 3)
  arrobj <- tdb_array(uri)
  expect_error(arrobj$consolidate_and_vacuum_async(cfg = "nope"), label = "cfg should 'tiledb_config'")

  ts <- function(x) {
    as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
  }

  # get config from tiledb cache
  origcfg <- tiledb::config(tiledb::tiledb_get_context())

  # consolidate
  trg_range <- c(ts(1), ts(3))
  expect_true(arrobj$consolidate_and_vacuum_async(start_time = trg_range[1], end_time = trg_range[2])[])

  # check we didn't alter consolidation config values from tiledb cache
  cfg <- tiledb::config(tiledb::tiledb_get_context())
  expect_equal(cfg["sm.consolidation.timestamp_start"], origcfg["sm.consolidation.timestamp_start"])
  expect_equal(cfg["sm.consolidation.timestamp_end"], origcfg["sm.consolidation.timestamp_end"])


  expect_equal(nrow(arrobj$frag_to_vacuum()), 0)
  expect_equal(nrow(arrobj$frag_uris()), 1)
  finfo <- arrobj$fragments_object$fragment_info
  ts_range <- tiledb::tiledb_fragment_info_get_timestamp_range(finfo, arrobj$frag_num() - 1)
  ts_range <- as.POSIXct(ts_range, tz = "UTC")
  expect_equal(ts_range, trg_range)

  rm(arrobj)
  rm(cfg)
  rm(origcfg)
  rm(finfo)
  gc()
})

test_that("Test '$reopen' method resets fragment info object", {

  uri <- file.path(withr::local_tempdir(), "test-TileDBArrayExp")
  write_test_array_tstamps(uri, 1)
  arrobj <- tdb_array(uri)
  expect_equal(arrobj$frag_num(), 1)
  arr <- arrobj$tiledb_array()
  arr[] <- data.frame(id = 1, val = as.POSIXct(10, tz = "UTC", origin = "1970-01-01"))

  # still one frag
  expect_equal(arrobj$frag_num(), 1)
  expect_no_error(arrobj$reopen())

  # now it works
  expect_equal(arrobj$frag_num(), 2)

  rm(arrobj)
  gc()
})
