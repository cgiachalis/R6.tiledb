test_that("'TileDBURI' class detects local object URIs using no scheme", {
  uri <- "/data/object_name"
  tdb_uri <- TileDBURI$new(uri)

  expect_match(tdb_uri$uri, uri)
  expect_equal(tdb_uri$scheme, NULL)
  expect_false(tdb_uri$is_remote_uri())
  expect_false(tdb_uri$is_tiledb_cloud_uri())
  expect_false(tdb_uri$is_tiledb_cloud_creation_uri())
})

test_that("'TileDBURI' class detects local object URIs using file:// scheme", {
  uri <- "file://data/object_name"
  tdb_uri <- TileDBURI$new(uri)

  expect_match(tdb_uri$uri, uri)
  expect_match(tdb_uri$scheme, "file")
  expect_false(tdb_uri$is_remote_uri())
  expect_false(tdb_uri$is_tiledb_cloud_uri())
  expect_false(tdb_uri$is_tiledb_cloud_creation_uri())
})

test_that("'TileDBURI' class detects remote object URIs using s3:// scheme", {
  uri <- "s3://bucket/prefix"
  tdb_uri <- TileDBURI$new(uri)

  expect_false(tdb_uri$is_tiledb_cloud_uri())
  expect_null(tdb_uri$tiledb_cloud_uri)

  expect_false(tdb_uri$is_tiledb_cloud_creation_uri())
  expect_equal(tdb_uri$object_uri, uri)
})

test_that("'TileDBURI' class detects remote object URIs using tiledb:// scheme", {
  uri <- "tiledb://namespace/object_name"
  tdb_uri <- TileDBURI$new(uri)

  expect_true(tdb_uri$is_tiledb_cloud_uri())
  expect_equal(tdb_uri$tiledb_cloud_uri, uri)

  expect_false(tdb_uri$is_tiledb_cloud_creation_uri())
  expect_equal(tdb_uri$object_uri, uri)
})

test_that("'TileDBURI' class detects remote object URIs using tiledb cloud creation scheme", {
  uri <- "tiledb://namespace/s3://bucket/prefix/object_name"
  tdb_uri <- TileDBURI$new(uri)
  expect_equal(tdb_uri$uri, uri)

  expect_true(tdb_uri$is_tiledb_cloud_uri())
  expect_equal(tdb_uri$tiledb_cloud_uri, "tiledb://namespace/object_name")

  expect_true(tdb_uri$is_tiledb_cloud_creation_uri())
  expect_equal(tdb_uri$object_uri, "s3://bucket/prefix/object_name")
})


test_that("'TileDBURI' class active bindings are ready-only fields", {
  uri <- "s3://bucket/prefix"
  tdb_uri <- TileDBURI$new(uri)

  expect_error(tdb_uri$scheme <- "a")
  expect_error(tdb_uri$tiledb_cloud_uri <- "a")
  expect_error(tdb_uri$object_uri <- "a")

  })

test_that("'TileDBURI' class active binding 'URI' field can be modified", {
  uri <- "s3://bucket/prefix"
  uri2 <- "s3://bucket/prefix200"
  tdb_uri <- TileDBURI$new(uri)
  tdb_uri$uri <- uri2

  expect_equal(tdb_uri$object_uri, "s3://bucket/prefix200")

})
