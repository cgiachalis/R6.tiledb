
test_that("'TileDBArrayExp' class works as expected", {

  options(R6.tiledb.internal = NULL)

  uri <- file.path(withr::local_tempdir(), "test-TileDBArrayExp")

  # Create an array
  idx_cols <- c("Dept", "Gender")
  df <- as.data.frame(UCBAdmissions)

  # Writes 3 parts
  tiledb::fromDataFrame(df[1:8, ], uri, col_index = idx_cols, sparse = TRUE)

  arr <- tiledb::tiledb_array(uri)
  arr[] <- df[9:16, ]
  arr[] <- df[17:24, ]

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

  expect_true(arrObj$consolidate(mode = "fragments")) # default

  expect_identical(arrObj$frag_num(), 1)
  expect_no_error(dfrags <- arrObj$frag_to_vacuum())
  expect_s3_class(dfrags, "data.frame")
  expect_equal(dim(dfrags), c(3,4))
  expect_equal(colnames(dfrags), c("Fragment", "start_timestamp", "end_timestamp", "URI"))

  expect_true(arrObj$vacuum(mode = "fragments")) # default
  expect_equal(arrObj$frag_to_vacuum(), data.frame(Fragment = character(0),
                                                   start_timestamp = numeric(0),
                                                   end_timestamp = numeric(0),
                                                   URI = character(0)))


  # expect_no_error(arrObj$reopen("WRITE"))
  # arrObj$set_metadata(list(name = "bob"))
  # arrObj$reopen("WRITE")
  # arrObj$set_metadata(list(surname = "foss"))
  # arrObj$reopen("READ")
  #
  # # fragment info object
  # fo <- arrObj$fragments_object
  # fo$reload_finfo()
  # finfo <- fo$fragment_info
  # tiledb::tiledb_fragment_info_get_unconsolidated_metadata_num(finfo)
  #
  # tiledb::tiledb_fragment_info_has_consolidated_metadata(finfo, 2)
  # expect_true(arrObj$consolidate(mode = "array_meta"))

  expect_invisible(arrObj$drop_attribute("Freq"))
  expect_equal(arrObj$colnames(),  c("Dept", "Gender", "Admit"))
  expect_error(arrObj$drop_attribute("Freq"), label = "`Freq` is not an attribute.")

  expect_null(arrObj$schema_upgrade())


})
