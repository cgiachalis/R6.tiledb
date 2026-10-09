
test_that("TileDBArray class", {
  uri <- file.path(withr::local_tempdir(), "test-nonexistent-array")
  expect_no_error(arr <- TileDBArray$new(uri = uri))
  expect_equal(arr$class(), "TileDBArray")
  expect_error(arr$object <- "a", '"object" is a read-only field.')
  expect_s3_class(arr, c("TileDBArray", "TileDBObject", "R6"), exact = TRUE)

  })

test_that("TileDBArray handles non-existent arrays", {
  uri <- file.path(withr::local_tempdir(), "test-nonexistent-array")
  arr <- TileDBArray$new(uri = uri)

  expect_false(arr$exists())
  expect_equal(arr$mode, "CLOSED")
  expect_error(arr$object, "Array does not exist.")
})

test_that("TileDBArray mode lifecycle works", {
  # open/reopen/close
  uri <- file.path(withr::local_tempdir(), "test-lifecycle")
  create_ucb_array_fixture(uri)

  arr <- TileDBArray$new(uri = uri)
  expect_invisible(arr$open(mode = "READ"))
  expect_equal(arr$mode, "READ")
  expect_true(tiledb::tiledb_array_is_open_for_reading(arr$object))

  expect_invisible(arr$close())
  expect_equal(arr$mode, "CLOSED")

  # Verify that array is neither open in READ nor WRITE mode
  expect_false(tiledb::tiledb_array_is_open_for_reading(arr$object))
  expect_false(tiledb::tiledb_array_is_open_for_writing(arr$object))

  # Open from CLOSE to WRITE mode and verify that array is open in WRITE mode
  expect_no_error(arr$open("WRITE"))
  expect_equal(arr$mode, "WRITE")
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr$object))

  # Re-open at READ mode
  expect_equal(arr$reopen()$mode, "READ")
  expect_true(arr$is_open())

  arr$close()

  # Open new instance from the same array to WRITE mode
  arr <- TileDBArray$new(uri = uri)
  expect_invisible(arr$open(mode = "WRITE"))
  expect_equal(arr$mode, "WRITE")

  # Verify that array is open in WRITE mode
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr$object), TRUE)

})

test_that("TileDBArray schema and names are reported correctly", {
  uri <- file.path(withr::local_tempdir(), "test-schema")
  create_ucb_array_fixture(uri)

  arr <- TileDBArray$new(uri = uri)
  expect_no_error(sch <- arr$schema_info())
  expect_equal(
    sch,
    structure(
      list(
        names = c("Dept", "Gender", "Admit", "Freq"),
        types = c("ASCII", "ASCII", "INT32", "FLOAT64"),
        status = c("Dim", "Dim", "Attr", "Attr"),
        enum = c(FALSE, FALSE, TRUE, FALSE)
      ),
      class = "data.frame",
      row.names = c(NA, -4L)
    )
  )

  expect_identical(arr$dimnames(), c("Dept", "Gender"))
  expect_identical(arr$attrnames(), c("Admit", "Freq"))
  expect_setequal(arr$colnames(), c("Dept", "Gender", "Admit", "Freq"))

  expect_s4_class(arr$schema(), "tiledb_array_schema")

})

test_that("TileDBArray method '$tiledb_array()' works as expected", {

  uri <- file.path(withr::local_tempdir(), "test-tiledb_array")
  create_ucb_array_fixture(uri)

  arr <- TileDBArray$new(uri = uri)
  expect_s4_class(arr$tiledb_array(), "tiledb_array")

  # Verify that tiledb_array query mode defaults to "READ"
  expect_true(tiledb::tiledb_array_is_open_for_reading(arr$tiledb_array(keep_open = TRUE)))

  # Verify that tiledb_array query mode is "WRITE" using query_type arg
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr$tiledb_array(query_type = "WRITE", keep_open = TRUE)))

  # Verify that tiledb_array query mode defaults to "WRITE"
  arr$reopen("WRITE")
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr$tiledb_array(keep_open = TRUE)))

  # Query a dim work without errors
  expect_no_error(d <- arr$tiledb_array(selected_points = list(Dept = "A"),
                       return_as = "data.frame")[])
  expect_s3_class(d, "data.frame")

})

test_that("TileDBArray with new instances", {

  uri <- file.path(withr::local_tempdir(), "test-tiledb_array")
  create_ucb_array_fixture(uri)

  arr <- TileDBArray$new(uri = uri)
  arr$open("WRITE")

  arr_new <- TileDBArray$new(uri = uri)
  expect_invisible(arr_new$open())
  expect_equal(arr_new$mode, "READ")

  # Verify that array is open in READ mode (1/2)
  expect_true(tiledb::tiledb_array_is_open_for_reading(arr_new$object))
  arr_new$close()
  expect_equal(arr_new$mode, "CLOSED")
  expect_false(tiledb::tiledb_array_is_open(arr_new$object))

  # Verify first instance state is not modified
  expect_equal(arr$mode, "WRITE")
  expect_true(tiledb::tiledb_array_is_open_for_writing(arr$object))

  # Verify that object is kept open in READ mode (not using open method) (2/2)
  arrObj_new <- TileDBArray$new(uri = uri)
  expect_true(tiledb::tiledb_array_is_open_for_reading(arrObj_new$object))
  expect_equal(arrObj_new$mode, "READ")
  arrObj_new$close()

  # Verify that array is open in WRITE mode
  arrObj_new <- TileDBArray$new(uri = uri)
  expect_no_error(arrObj_new$open(mode = "WRITE"))
  expect_true(tiledb::tiledb_array_is_open_for_writing(arrObj_new$object))

})

test_that("TileDBArray metadata operations work as expected", {

  uri <- file.path(withr::local_tempdir(), "test-metadata")
  create_ucb_array_fixture(uri)

  arr <- TileDBArray$new(uri = uri)
  arr$open("WRITE")

  empty_md <- arr$get_metadata()
  expect_s3_class(empty_md, "tdb_metadata")
  expect_length(empty_md, 0L)

  expect_s3_class(arr$set_metadata(list(a = "Hi", b = "good", c = 10)), "TileDBArray")
  expect_equal(arr$get_metadata(keys = "a"), "Hi")

  # Mode is "WRITE" after setting metadata
  expect_equal(arr$mode, "WRITE")

  trg <- structure(list(a = "Hi", b = "good"), class = c("tdb_metadata",
                                                        "list"),
                   R6.class = "TileDBArray", object_type = "ARRAY")

  expect_equal(arr$get_metadata(keys = c("a", "b")), trg)

  # Character values should be scalar strings only, not vectors
  expect_error(arr$set_metadata(list(invalid = c("Boo", "foo"))))

  # Should be a named list with key-value metadata"
  expect_error(arr$set_metadata(list(1)))

  arr$set_metadata(list(d = "Boo", e = 3))

  trg <- structure(list(a = "Hi", d = "Boo"), class = c("tdb_metadata",
                                                         "list"),
                   R6.class = "TileDBArray", object_type = "ARRAY")

  # Omit NULL keys
  expect_equal(arr$get_metadata(c("a", "d", "non-meta")), trg)

  # Not missing or invalid key returns NULL
  expect_null(arr$get_metadata("non-meta"))
  expect_length(arr$get_metadata(), 5L)

  arr$close()

  # Read back metadata even when the array is in WRITE mode.
  arr$open(mode = "WRITE")
  expect_equal(arr$get_metadata(keys = "d"), "Boo")
  expect_equal(arr$get_metadata(keys = "a"), "Hi")
  expect_named(arr$get_metadata(),c("a", "b", "c", "d", "e"))

  # Using get_metadata() from CLOSED mode, leaves state to READ mode
  arr$close()
  key <- arr$get_metadata(keys = "d")
  expect_equal(arr$mode, "READ")

  expect_length(arr$get_metadata(c("a", "d")), n = 2)
})

test_that("TileDBArray timestamp active field assignment validates input", {

  uri <- file.path(withr::local_tempdir(), "test-timestamps")
  create_ucb_array_fixture(uri)

  arr <- TileDBArray$new(uri = uri)

  expect_no_error(arr$tiledb_timestamp <- NULL)
  expect_s3_class(arr$tiledb_timestamp, "tiledb_timestamp")

  expect_no_error(arr$tiledb_timestamp <- 10)
  expect_equal(arr$tiledb_timestamp, set_tiledb_timestamp(end_time = 10))

  expect_no_error(arr$tiledb_timestamp <- c(0, 10))
  expect_equal(arr$tiledb_timestamp, set_tiledb_timestamp(0, end_time = 10))

  expect_no_error(arr$tiledb_timestamp <- "1990-01-01")
  expect_equal(arr$tiledb_timestamp, set_tiledb_timestamp(end_time = "1990-01-01"))

  expect_no_error(arr$tiledb_timestamp <- as.POSIXct(10, tz = "UTC"))
  expect_equal(arr$tiledb_timestamp, set_tiledb_timestamp(end_time = as.POSIXct(10)))

  ts <- set_tiledb_timestamp(start_time = as.Date("1990-01-01"), end_time = as.Date("2000-01-01"))
  expect_no_error(arr$tiledb_timestamp <- ts)
  expect_equal(arr$tiledb_timestamp, ts)

  expect_error(arr$tiledb_timestamp <- "bob", label = "character string is not in a standard unambiguous format")
  expect_error(arr$tiledb_timestamp <- c(1, 3, 3), label = "Invalid 'tiledb_timestamp' input")
  expect_error(arr$tiledb_timestamp <- numeric(0), label = "Invalid 'tiledb_timestamp' input")
})


test_that("TileDBArray object's time-stamps when time-travel", {

  uri <- file.path(withr::local_tempdir(), "test-timetravel")
  tstamps <- create_time_travel_fixture(uri)

  # Test that init and store the tiledb array with reading or
  # writing @ time point
  expect_no_error(arrobj <- TileDBArray$new(uri, tiledb_timestamp = tstamps[1]))
  expect_equal(arrobj$mode, "CLOSED")
  expect_no_error(arrobj$open(mode = "READ"))
  expect_true(tiledb::tiledb_array_is_open_for_reading(arrobj$object))
  expect_equal(arrobj$tiledb_timestamp, set_tiledb_timestamp(end_time = tstamps[1]))

  # Ensure array object has the correct end timestamp
  expect_equal(arrobj$object@timestamp_start, as.POSIXct(double()))
  expect_equal(as.POSIXct(arrobj$object@timestamp_end, tz = "UTC"),  tstamps[1])
  arrobj$close()

  arrobj <- TileDBArray$new(uri, tiledb_timestamp = tstamps[2])
  expect_no_error(arrobj$open(mode = "WRITE"))
  # Opening on WRITE after we init with timestamp, the timestamp should be
  # set to default (not important though as we open a new handle with no tstamps)
  # expect_equal(arrobj$tiledb_timestamp, set_tiledb_timestamp(end_time = NA))

  expect_true(tiledb::tiledb_array_is_open_for_writing(arrobj$object))

  # Ensure array object has the default timestamps
  expect_equal(arrobj$object@timestamp_start, as.POSIXct(double()))
  expect_equal(arrobj$object@timestamp_end, as.POSIXct(double()))
  arrobj$close()

  expect_no_error(arrobj <- TileDBArray$new(uri, tiledb_timestamp = tstamps[1:2]))
  expect_equal(arrobj$mode, "CLOSED")
  expect_no_error(arrobj$open(mode = "READ"))
  expect_true(tiledb::tiledb_array_is_open_for_reading(arrobj$object))
  expect_equal(arrobj$tiledb_timestamp, set_tiledb_timestamp(start_time = tstamps[1], end_time = tstamps[2]))

  # Ensure array object has the corrent start,end timestamps
  expect_equal(as.POSIXct(arrobj$object@timestamp_start, tz = "UTC"),  tstamps[1])
  expect_equal(as.POSIXct(arrobj$object@timestamp_end, tz = "UTC"),  tstamps[2])

})


test_that("TileDBArray time-travel semantics work", {

  trg <- structure(list(id = c(1L, 1L, 1L), val = c(1, 2, 3)), query_status = "COMPLETE")
  trg_t1 <- structure(list(id = c(1L), val = c(1)), query_status = "COMPLETE")
  trg_t2 <- structure(list(id = c(1L), val = c(2)), query_status = "COMPLETE")
  trg_t4 <- structure(list(id = c(1L, 1L), val = c(2, 3)), query_status = "COMPLETE")
  trg_t3 <- structure(list(id = c(1L, 1L), val = c(1, 2)), query_status = "COMPLETE")

  trg_meta <- structure(
    list(key1 = "2025-08-18 16:12:50", key2 = "2025-08-18 16:12:55", key3 = "2025-08-18 16:13:01"),
    class = c("tdb_metadata", "list"),
    R6.class = "TileDBArrayExp",
    object_type = "ARRAY"
  )

  uri <- file.path(withr::local_tempdir(), "test-timetravel")
  tstamps <- create_time_travel_fixture(uri)

  # Time - travelling
  arrobj <- tdb_array(uri)

  expect_equal(arrobj$object[], trg)
  expect_equal(arrobj$get_metadata(), trg_meta)

  arrobj$tiledb_timestamp <- tstamps[1]
  expect_equal(arrobj$object[], trg_t1)
  expect_equal(arrobj$get_metadata(), trg_meta[1])

  arrobj$tiledb_timestamp <- tstamps[2]
  expect_equal(arrobj$object[], trg_t3)
  expect_equal(arrobj$get_metadata(), trg_meta[1:2])

  arrobj$tiledb_timestamp <- c(tstamps[2], tstamps[2])
  expect_equal(arrobj$object[], trg_t2)

  # time range not applicable to metadata, retrieve up to t2
  expect_equal(arrobj$get_metadata(), trg_meta[1:2])

  arrobj$tiledb_timestamp <- c(tstamps[2], tstamps[3])
  expect_equal(arrobj$object[], trg_t4)
  # time range not applicable to metadata, etrieve up to t3
  expect_equal(arrobj$get_metadata(), trg_meta)

  # reset
  arrobj$tiledb_timestamp <- NULL
  expect_equal(arrobj$object[], trg)
  expect_equal(arrobj$get_metadata(), trg_meta)

  # no effect on "WRITE" mode
  arrobj$reopen("WRITE")
  arrobj$tiledb_timestamp <- tstamps[1]
  expect_equal(arrobj$mode, "WRITE")
  expect_equal(arrobj$object[], trg)
  expect_equal(arrobj$get_metadata(), trg_meta)

  arrobj$close()
  arrobj$tiledb_timestamp <- tstamps[1]

  # active field 'tiledb_timestamp' triggers opening
  expect_error(arrobj$open("WRITE"), label = "TileDB Array is already opened.")
  arrobj$close()
})

test_that("TileDBArray print() snapshot for non-existent arrays", {
  uri <- file.path(withr::local_tempdir(), "test-nonexistent-array")
  arr <- TileDBArray$new(uri = uri)

  expect_no_error(suppressMessages(arr$print()))
  expect_snapshot(arr$print())
})


test_that("TileDBArray print() snapshot for non-empty array", {
  uri <- file.path(withr::local_tempdir(), "test-nonempty -array")
  create_ucb_array_fixture(uri)
  arr <- TileDBArray$new(uri = uri)
  expect_snapshot(arr$print())
})

test_that("TileDBArray metadata print method", {

  uri <- file.path(withr::local_tempdir(), "test-TileDBArray")
  create_ucb_array_fixture(uri)
  arr <- TileDBArray$new(uri = uri)

  # metadata
  expect_snapshot(arr$get_metadata())

  md <- list(a = "Hi", b = "good", c = 10)
  arr$reopen(mode = "WRITE" )
  arr$set_metadata(md)

  md <- list(d = "Boo", e = 3, f = paste(rep(letters[1:21], 3), collapse = ""))
  arr$set_metadata(md)
  expect_snapshot(arr$get_metadata())

  })
