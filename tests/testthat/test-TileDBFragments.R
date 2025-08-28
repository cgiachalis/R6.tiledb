
test_that("'TileDBFragments' class works as expected", {

  ctx <- tiledb::tiledb_ctx(cached = FALSE)
  uri <- file.path(withr::local_tempdir(), "test-TileDBFragments")

  expect_error(TileDBFragments$new())
  expect_error(TileDBFragments$new("uri/path", ctx = "boo"))

  # write test array on disk
  write_test_array_tstamps(uri)

  expect_no_error(fragObj <- TileDBFragments$new(uri = uri))


  # Active --------------------------------------------------------------

  # active bindings are ready-only fields
  expect_error(fragObj$uri  <- "boo")
  expect_error(fragObj$fragment_info <- "boo")

  # active bindings are OK
  expect_equal(fragObj$uri, uri)
  expect_s4_class(fragObj$fragment_info, "tiledb_fragment_info")


  # Methods -------------------------------------------------------------


  expect_equal(fragObj$class(), "TileDBFragments")

  expect_identical(fragObj$frag_num(), 3)

  expect_no_error(dfrags <- fragObj$frag_uris())
  expect_s3_class(dfrags, "data.frame")
  expect_equal(dim(dfrags), c(3,4))
  expect_equal(colnames(dfrags), c("Fragment", "start_timestamp", "end_timestamp", "URI"))

  expect_no_error(dump_str <- capture_output_lines(fragObj$dump()))
  expect_equal(dump_str[[1]], "- Fragment num: 3")

  expect_equal(fragObj$to_vacuum(), data.frame(Fragment = character(0),
                                               start_timestamp = numeric(0),
                                               end_timestamp = numeric(0),
                                               URI = character(0)))

  expect_equal(fragObj$to_vacuum_num(), 0)
  expect_invisible(fragObj$reload_finfo())

  expect_error(fragObj$delete_fragment("a"))
  expect_error(fragObj$delete_fragment(5))
  expect_true(fragObj$delete_fragment(3))
  expect_identical(fragObj$frag_num(), 2)

  ts <- function(x) {
    as.POSIXct(x / 1000, tz = "UTC", origin = "1970-01-01")
  }


  expect_error(fragObj$delete_fragment_range(1, ts(1)))
  expect_error(fragObj$delete_fragment_range(ts(1), 1))
  expect_error(fragObj$delete_fragment_range(ts(2), ts(1)))

  expect_true(fragObj$delete_fragment_range(ts(3), ts(3)))
  expect_identical(fragObj$frag_num(), 2) # no deletion

  expect_true(fragObj$delete_fragment_range(ts(2), ts(2)))
  expect_identical(fragObj$frag_num(), 1)

  expect_no_error(frag_uris <- fragObj$frag_uris(trunc_uri = FALSE)$URI)
  expect_length(frag_uris, 1)

  expect_error(fragObj$delete_fragment_list(list(1)))

  expect_true(fragObj$delete_fragment_list(frag_uris))
  expect_identical(fragObj$frag_num(), 0)
  expect_equal( fragObj$frag_uris(), data.frame(Fragment = character(0),
                                                start_timestamp = numeric(0),
                                                end_timestamp = numeric(0),
                                                URI = character(0)))

  expect_snapshot(val <- fragObj$delete_fragment(1))
  expect_false(val)

})
