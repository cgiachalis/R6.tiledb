test_that("Test 'new_context()'", {

  # new_context does not cache context
  ctx <- new_context()

  ctx_cached <- tiledb::tiledb_ctx(cached = TRUE)

  expect_false(identical(ctx@ptr, ctx_cached@ptr))

})

