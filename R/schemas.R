
# Set up TileDB filter
.tiledb_filter <- function(level = -1L, name = "ZSTD") {
  tiledb::tiledb_filter_set_option(
    object = tiledb::tiledb_filter(name),
    option = "COMPRESSION_LEVEL",
    value = level)
}

# Filter list includes only one filter
.tiledb_flist <- function(level = -1, name = "ZSTD") {

  tiledb::tiledb_filter_list(.tiledb_filter(level = level, name = name))
}

# Set up common Dims
.dim_ascii <- function(name, level = -1L, fname = "ZSTD") {

  tiledb::tiledb_dim(name = name,
                     domain = c(NULL, NULL),
                     tile = NULL,
                     type = "ASCII",
                     filter_list = .tiledb_flist(level = level, name = fname))
}

.dim_datetime_day <- function(name, level = -1L, type_index = 0, fname = "ZSTD") {

  if (type_index == 0) {
    dim_type <- "DATETIME_DAY" # R's Date
    dom <- c(0, 84006) # 1970-01-01::2200-01-01
  } else if (type_index == 1) {
    dim_type = "DATETIME_MS" # R's POSIXct
    dom <- c(0, 7258111200) * 1000
  } else {
    stop("'type_index' gets values 0 for 'DATETIME_DAY' or 1 for 'DATETIME_MS'",
         call. = FALSE)
  }
  tiledb::tiledb_dim(name = name,
                     domain = dom,
                     tile = 1000,
                     type = dim_type,
                     filter_list = .tiledb_flist(level = level, name = fname))
}

# Schemas -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


# * Generate TileDB schema for 2D time series
#   Dims : "date", "key"
#   Attrs:  character vector
#   type_index: 0 for date class, 1 for datetime (POSIXct)
.gen_ts2dim_schema <- function(attr = "close",
                               type_index = 0) {

  .filter_zstd <- .tiledb_flist(level = -1, name = "ZSTD")
  .filter_rle <- .tiledb_flist(level = -1, name = "RLE")

  dom <- tiledb::tiledb_domain(c(.dim_datetime_day("index",
                                                   type_index = type_index),
                                 .dim_ascii("symbol")))

  attrs <- sapply(attr, function(.x) {
    tiledb::tiledb_attr(
      name = .x,
      type = "FLOAT64",
      ncells = 1,
      nullable = TRUE,
      filter_list = .filter_zstd
    )
  })

  sch <- tiledb::tiledb_array_schema(
    domain = dom,
    attrs = attrs,
    cell_order = "COL_MAJOR",
    tile_order = "COL_MAJOR",
    capacity = 10000,
    sparse = TRUE,
    allows_dups = FALSE,
    coords_filter_list = .filter_zstd,
    offsets_filter_list = .filter_zstd,
    validity_filter_list = .filter_rle
  )

  sch
}

# * Generate TileDB schema for 1D time series
#   Dims : "index"
#   Attrs:  character vector
#   type_index: 0 for date class, 1 for date-time (POSIXct)
.gen_ts1dim_schema <- function(attr = "close", type_index = 0) {

  .filter_zstd <- .tiledb_flist(level = -1, name = "ZSTD")
  .filter_rle <- .tiledb_flist(level = -1, name = "RLE")

  dom <- tiledb::tiledb_domain(c(.dim_datetime_day("index", type_index = type_index)))

  attrs <- sapply(attr, function(.x) {
    tiledb::tiledb_attr(
      name = .x,
      type = "FLOAT64",
      ncells = 1,
      nullable = TRUE,
      filter_list = .filter_zstd)
  })

  sch <- tiledb::tiledb_array_schema(
    domain = dom,
    attrs = attrs,
    cell_order = "COL_MAJOR",
    tile_order = "COL_MAJOR",
    capacity = 10000,
    sparse = TRUE,
    allows_dups = FALSE,
    coords_filter_list = .filter_zstd,
    offsets_filter_list = .filter_zstd,
    validity_filter_list = .filter_rle
  )

  sch
}

