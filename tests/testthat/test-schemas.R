test_that("Test demo array creation", {
  uri <- file.path(withr::local_tempdir(), "test-arr")

  expect_no_error(demo_array_UCBAdmissions(uri))
  expect_equal(tiledb::tiledb_object_type(uri), "ARRAY")
})


test_that("Test demo schemas", {

  expect_no_error(sch <- .gen_ts1dim_schema())
  expect_s4_class(sch, "tiledb_array_schema")
  expect_true(tiledb::schema_check(sch))

  expect_no_error(sch <- .gen_ts2dim_schema())
  expect_s4_class(sch, "tiledb_array_schema")
  expect_true(tiledb::schema_check(sch))

})
