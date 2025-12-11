
test_that("'TileDBFragments' class works as expected", {

  testthat::skip_on_os("mac")

  withr::local_timezone(tz = "UTC")
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
    as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
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


test_that("Test ifragments methods", {

  uri <- file.path(withr::local_tempdir(), "test-TileDBFragments2")

  # write test array on disk
  write_test_array_tstamps(uri)

  expect_no_error(fobj <- tdb_fragments(uri))
  expect_s3_class(fobj, "TileDBFragments")

  # get fragment metadata
  expect_no_error(ifrag <- fobj$get_ifragment(1))
  expect_s3_class(ifrag, "ifragment")

  trg <- structure(list(fid = 1, URI = "__fragments/__1000_1000_28109_22",
                        type = "sparse", nonemptydom = list(id = structure(c(1, 1
                        ), dimtype = "INT32")), size = "3.14 KiB", cell_num = 1,
                        timestamp_range = structure(c(1, 1), class = c("POSIXct",
                      "POSIXt")), version = 22L, consolidated_metadata = FALSE),
                   class = "ifragment")

  # fixed uri
  ifrag$URI <- "__fragments/__1000_1000_28109_22"
  expect_equal(ifrag, trg)

  expect_no_error(ifrag_list <- fobj$get_first_ifragments(2))
  expect_s3_class(ifrag_list, "ifragment_list")
  expect_named(ifrag_list, c("Frag1", "Frag2"))
  expect_true(all(vapply_char(ifrag_list, class) == "ifragment"))

  expect_no_error(ifrag_list2 <- fobj$get_last_ifragments(2))
  expect_s3_class(ifrag_list2, "ifragment_list")
  expect_named(ifrag_list2, c("Frag2", "Frag3"))
  expect_true(all(vapply_char(ifrag_list2, class) == "ifragment"))

  # error are raised
  expect_error(fobj$get_ifragment(4))
  expect_error(fobj$get_first_ifragments(4))
  expect_error(fobj$get_last_ifragments(4))

  ifrag_list$Frag1$URI <-  "__fragments/__1000_1000_28109_22"
  ifrag_list$Frag2$URI <-  "__fragments/__2000_2000_fd109_22"

  expect_snapshot(ifrag)
  expect_snapshot(ifrag_list)

  })
