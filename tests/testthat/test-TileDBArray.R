
test_that("'TileDBArray' class works as expected", {

  options(R6.tiledb.internal = NULL)

  uri <- file.path(withr::local_tempdir(), "test-TileDBArray")

  arrObj <- TileDBArray$new(uri = uri, internal_use = "permit")

  expect_false(arrObj$exists())
  expect_equal(arrObj$mode(), "CLOSED")

  expect_error(arrObj$object, "Array does not exist.")

  # Print that array does not exist
  expect_snapshot(arrObj$print())

  # Create an array
  idx_cols <- c("Dept", "Gender")
  df <- as.data.frame(UCBAdmissions)
  tiledb::fromDataFrame(df, uri, col_index = idx_cols)

  expect_invisible(arrObj$open())
  expect_equal(arrObj$mode(), "READ")

  # Print array
  expect_snapshot(arrObj$print())

  # Check schema info
  expect_no_error(sch_info <- arrObj$schema_info())

  trg_sch_info <- structure(list(names = c("Dept", "Gender", "Admit", "Freq"),
                                 types = c("ASCII", "ASCII", "INT32", "FLOAT64"),
                                 status = c("Dim", "Dim", "Attr", "Attr"),
                                 enum = c(FALSE, FALSE, TRUE, FALSE)),
                                 class = "data.frame", row.names = c(NA, -4L))

  expect_equal(sch_info, trg_sch_info)

  # Verify that array is open in READ mode
  expect_true(tiledb::tiledb_array_is_open_for_reading(arrObj$object), TRUE)

  arrObj$close()
  expect_equal(arrObj$mode(), "CLOSED")

  # Verify that array is neither open in READ nor WRITE mode
  expect_false(tiledb::tiledb_array_is_open_for_reading(arrObj$object), FALSE)
  expect_false(tiledb::tiledb_array_is_open_for_writing(arrObj$object), FALSE)

  # Open from CLOSE to WRITE mode
  expect_no_error(arrObj$open("WRITE"))
  expect_equal(arrObj$mode(), "WRITE")

  # Verify that array is open in WRITE mode
  expect_true(tiledb::tiledb_array_is_open_for_writing(arrObj$object), TRUE)

  expect_equal(arrObj$reopen()$mode(), "READ")
  expect_true(arrObj$is_open())
  arrObj$close()

  # Check is ready only field
  expect_error(arrObj$object <- "a")

  expect_true(arrObj$exists())

  expect_s4_class(arrObj$tiledb_array(), "tiledb_array")

  # Verify that tiledb_array query mode defaults to "READ"
  expect_true(tiledb::tiledb_array_is_open_for_reading(arrObj$tiledb_array(keep_open = TRUE)))

  # Verify that tiledb_array query mode is "WRITE" using query_type arg
  expect_true(tiledb::tiledb_array_is_open_for_writing(arrObj$tiledb_array(query_type = "WRITE", keep_open = TRUE)))

  # Verify that tiledb_array query mode defaults to "WRITE"
  arrObj$reopen("WRITE")
  expect_true(tiledb::tiledb_array_is_open_for_writing(arrObj$tiledb_array(keep_open = TRUE)))

  expect_identical(arrObj$dimnames(), idx_cols)

  attr_cols <- setdiff(colnames(df), idx_cols)
  expect_identical(arrObj$attrnames(), attr_cols)

  expect_setequal(arrObj$colnames(), colnames(df))

  # metadata

  empty_metadata <- arrObj$get_metadata()
  expect_s3_class(empty_metadata, "tiledb_metadata")
  expect_equal(length(empty_metadata), 0L)

  md <- list(a = "Hi", b = "good", c = 10)
  arrObj$reopen(mode = "WRITE" )
  arrObj$set_metadata(md)

  md <- list(d = "Boo", e = 3)
  arrObj$set_metadata(md)
  arrObj$close()

  md <- list(1)
  arrObj$open(mode = "WRITE")
  expect_error(arrObj$set_metadata(md))
  arrObj$close()

  expect_equal(arrObj$get_metadata(key = "d"), "Boo")
  expect_equal(arrObj$get_metadata(key = "a"), "Hi")
  expect_equal(length(arrObj$get_metadata()), 5)
  arrObj$close()

  # We need the ability to read back metadata even when the
  # array is opened for write.
  arrObj$open(mode = "WRITE")
  expect_equal(arrObj$get_metadata(key = "d"), "Boo")
  expect_equal(arrObj$get_metadata(key = "a"), "Hi")
  expect_equal(length(arrObj$get_metadata()), 5)
  arrObj$close()

  # new instances
  arrObj_new <- TileDBArray$new(uri = uri, internal_use = "permit")
  expect_invisible(arrObj_new$open())
  expect_equal(arrObj_new$mode(), "READ")

  # Verify that array is open in READ mode (1/2)
  expect_true(tiledb::tiledb_array_is_open_for_reading(arrObj_new$object), TRUE)
  arrObj_new$close()
  expect_equal(arrObj_new$mode(), "CLOSED")

  # Verify that object is kept open in READ mode (not using open method) (2/2)
  arrObj_new <- TileDBArray$new(uri = uri, internal_use = "permit")
  expect_true(tiledb::tiledb_array_is_open_for_reading(arrObj_new$object), TRUE)

  # Verify that array is open in WRITE mode
  arrObj_new <- TileDBArray$new(uri = uri, internal_use = "permit")
  expect_no_error(arrObj_new$open(mode = "WRITE"))
  expect_true(tiledb::tiledb_array_is_open_for_writing(arrObj_new$object), TRUE)

  arrObj_new$close()

})
