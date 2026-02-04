# Time Series Storage

## Introduction

This vignette showcases how to store time-series data and build a query
interface similar to
[xts](https://cran.r-project.org/web/packages/xts/index.html) package.

A new class `TimeSeries1d` has been created to work with tabular daily
time-series data; its data model and R6 class definition can be found in
the Appendix.

**Prerequisites**:

- `R` packages: `data.table`, `xts` and `arrow`
- Source `TimeSeries1d` class from the appendix

## Application

### Create Time-Series Array

``` r
# URI path to store array
uri <- tempfile()

# Generate TimeSeries1d
tsobj <- TimeSeries1d$new(uri)

# Create schema with two symbols
tsobj$create(attr_names = c("A", "B"))

# Schema
tsobj$schema_info()
#   names        types status  enum
# 1 index DATETIME_DAY    Dim FALSE
# 2     A      FLOAT64   Attr FALSE
# 3     B      FLOAT64   Attr FALSE
```

### Generate data

``` r
# times-series data
dat <- data.frame(index = seq.Date("2010-01-01", length.out = 10),
                  A = as.numeric(1:10),
                  B = as.numeric(11:20),
                  C = as.numeric(21:30))

# Add some NAs, leading and random
dat[1:2, "A"] <- NA
dat[5, "B"] <- NA

# Initial dataset
dat1 <- dat[, c("index", "A", "B")]

# New series to ingest
dat2 <- dat[, c("index", "C")]
```

### Write and Ingest Series

``` r
# Write data
tsobj$write(dat1)

# Query all
tsobj$tiledb_array(return_as = "data.table")[]
#          index     A     B
#         <Date> <num> <num>
#  1: 2010-01-01    NA    11
#  2: 2010-01-02    NA    12
#  3: 2010-01-03     3    13
#  4: 2010-01-04     4    14
#  5: 2010-01-05     5    NA
#  6: 2010-01-06     6    16
#  7: 2010-01-07     7    17
#  8: 2010-01-08     8    18
#  9: 2010-01-09     9    19
# 10: 2010-01-10    10    20

# Subset like xts
tsobj$subset(c("2010-01-01", "2010-01-06"), cols = "A", as_xts = TRUE)
#             A
# 2010-01-01 NA
# 2010-01-06  6
```

``` r
t0 <- Sys.time()
# Ingest new series
tsobj$ingest(dat2)

# Check symbols
tsobj$symbols()
# [1] "A" "B" "C"

# Subset like xts
tsobj$subset(c("2010-01-06/"), cols = c("A", "C"))
#         index     A     C
#        <Date> <num> <num>
# 1: 2010-01-06     6    26
# 2: 2010-01-07     7    27
# 3: 2010-01-08     8    28
# 4: 2010-01-09     9    29
# 5: 2010-01-10    10    30
```

Note that schema has been evolved due to addition of new series (new
attribute):

``` r
vfs_dir_tree(tsobj$uri)
# C:/Users/Constantine/AppData/Local/Temp/Rtmp6JkFsL/file33e4e961897
# ├── __commits
# │   ├── __1769941398404_1769941398404_4136f8ae903054d78272ab6b378ea126_22.wrt
# │   └── __1769941399444_1769941399444_662c00de4e03839addba20ab4934789b_22.wrt
# ├── __fragments
# │   ├── __1769941398404_1769941398404_4136f8ae903054d78272ab6b378ea126_22
# │   │   ├── a0.tdb
# │   │   ├── a0_validity.tdb
# │   │   ├── a1.tdb
# │   │   ├── a1_validity.tdb
# │   │   ├── d0.tdb
# │   │   └── __fragment_metadata.tdb
# │   └── __1769941399444_1769941399444_662c00de4e03839addba20ab4934789b_22
# │       ├── a0.tdb
# │       ├── a0_validity.tdb
# │       ├── a1.tdb
# │       ├── a1_validity.tdb
# │       ├── a2.tdb
# │       ├── a2_validity.tdb
# │       ├── d0.tdb
# │       └── __fragment_metadata.tdb
# ├── __fragment_meta
# ├── __labels
# ├── __meta
# └── __schema
#     ├── __1769941398329_1769941398329_2f2845374bfd7b24cba73a8fa319dce4
#     ├── __1769941399389_1769941399389_3bdf9ca23026edf29709cafca6e50284
#     └── __enumerations
# 
# ❯ directories (6) • total size (9.95 KiB)
```

### Built-in functions

``` r
# Index domain
tsobj$domain_index()
# [1] "1970-01-01" "2200-01-01"

# Index range by series
tsobj$index_range()
#    symbol    st_date    en_date
#    <char>     <Date>     <Date>
# 1:      A 2010-01-03 2010-01-10
# 2:      B 2010-01-01 2010-01-10
# 3:      C 2010-01-01 2010-01-10

# Rename A-> NEW_A
tsobj$rename_symbol("A", "NEW_A")

tsobj$symbols()
# [1] "B"     "C"     "NEW_A"
```

## Appendix

### Data Model

#### Schema

| Parameter  | Value     |
|------------|-----------|
| Array Type | Sparse    |
| Rank       | 1         |
| Cell order | Col-major |
| Tile order | Col-major |

#### Dimensions

| Dimension | TileDB Datatype |
|-----------|-----------------|
| `index`   | `DATETIME_DAY`  |

#### Attributes

| Attribute    | TileDB Datatype |
|--------------|-----------------|
| `<SYMBOL_1>` | `ASCII`         |
| ….           | ….              |
| `<SYMBOL_K>` | `ASCII`         |

------------------------------------------------------------------------

### `TimeSeries1d` Class

``` r
TimeSeries1d <- R6::R6Class(
  classname = "TimeSeries1d",
  inherit = TileDBArray,
  cloneable = FALSE,
  active = list(
    #' @field index Retrieve Array's unique index.
    #'
    index = function(value) {

      if (!missing(value)) {
        private$check_read_only("index")
      }

      if (is.null(private$.index)) {

        arr <- self$tiledb_array(attrs = NA_character_, return_as = "arrow")[]
        index_unique <- arrow::call_function("unique", arr[["index"]])
        xi <- index_unique$as_vector()

        if (!xts::isOrdered(xi, strictly = TRUE)) {
          idx <- order(xi, method = "radix")
          xi <- xi[idx]
        }

        attr(xi, "tclass") <- self$tclass
        attr(xi, "tzone") <- self$tzone

        private$.index <- xi

      }

      private$.index

    },

    #' @field tclass Retrieve index class, one of:
    #' [Date()] or [POSIXct()].
    #'
    tclass = function(value) {

      if (!missing(value)) {
        private$check_read_only("tclass")
      }

      # Retrieve time class from metadata. If null, then infer from data type.
      if (is.null(private$.tclass)) {
        dt <- self$get_metadata("tclass")

        if (is.null(dt)) {
          dt <- tiledb::datatype(self$dimensions()[["index"]])
          if (dt == "DATETIME_DAY") { # Either "DATETIME_DAY" or "DATETIME_SEC/MS"
            private$.tclass <- "Date"
          } else {
            private$.tclass <- c("POSIXct","POSIXt")
          }
        } else {
          private$.tclass <- switch(dt,
                                    Date = "Date",
                                    POSIXct = c("POSIXct","POSIXt")
          )
        }
      }
      private$.tclass
    },
    #' @field tzone Retrieve Array timezone. This represents
    #' the timezone where the time series have been created.
    #'
    #' If `NULL`, it will default to `UTC`.
    #'
    tzone = function(value){

      if (!missing(value)) {
        private$check_read_only("tzone")
      }

      if (is.null(private$.tzone)) {

        xval <- self$get_metadata("tzone")

        if (is.null(xval)) {
          private$.tzone <- "UTC"
        } else {
          private$.tzone <- xval
        }

      }
      private$.tzone
    }

  ),

  public = list(
    #' @description Create a schema only for time series `TileDB` array.
    #'
    #' @param schema_name Select a time series schema.
    #' @param attr_names A character vector with attributes names to be created
    #' by generic schema.
    #' @param meta A named list with key-value pairs of metadata.
    #'
    #' @return The object, invisibly.
    #'
    create = function(schema_name = c("generic_date",
                                      "generic_datetime"),
                      attr_names = "close",
                      meta = NULL) {

      schema_name <- match.arg(schema_name)
      mode <- "WRITE"

      private$log_debug0("create", "Define time-series schema '{}'", schema_name)

      sch <- switch(
        schema_name,
        generic_date = {
          .gen_ts1dim_schema(attr = attr_names, type_index = 0)
        },
        generic_datetime = {
          .gen_ts1dim_schema(attr = attr_names, type_index = 1)
        },
        cli::cli_abort("Unkown schema", call = NULL)
      )

      # Add schema only
      tiledb::tiledb_array_create(self$uri, schema = sch)

      private$.tiledb_array <- tiledb::tiledb_array(self$uri,
                                                    ctx = self$ctx,
                                                    query_type = mode,
                                                    query_layout = "UNORDERED",
                                                    keep_open = TRUE)

      private$.mode <- mode
      private$.object_type <- "ARRAY"

      if (!is.null(meta)) {
        self$set_metadata(meta)
      }

      invisible(self)
    },
    #' @description Write time-series data to array.
    #'
    #' @param tsdata An object of class `data.frame`.
    #'
    #' @return The object, invisibly.
    #'
    write = function(tsdata) {

      private$check_object_exists()
      private$check_open_for_write()
      private$log_debug("write", "Writing time-series data")

      arr <- self$object
      arr[] <- tsdata

      invisible(self)

    },

    #' @description Ingest new time-series data to array.
    #'
    #' @param tsdata A `data.frame`, [arrow::Table()] or [arrow::RecordBatch()] with new symbols
    #' including index column.
    #'
    #' @return The object, invisibly.
    #'
    ingest = function(tsdata) {

      private$check_object_exists()
      private$check_open_for_write()

      stopifnot(is.data.frame(tsdata) || is_arrow_record_batch(tsdata) || is_arrow_table(tsdata))

      if (is.data.frame(tsdata)) {
        tsdata <- arrow::as_arrow_table(tsdata)
      }

      cols <- tsdata$ColumnNames()
      new_columns <- cols[cols != "index"]

      if (isFALSE(all(!(new_columns %in% self$symbols())))) {
        cli::cli_abort("Found symbols that are already present in database.", call = NULL)
      }

      if (is.null(tsdata[["index"]])) {
        cli::cli_abort("'index' is missing from time-series data.", call = NULL)
      }

      start_index <- paste0(as.character(tsdata[["index"]][1]$as_vector()), "/")

      qry <- private$index_query(start_index, self$tclass[1], tz = self$tzone)

      arrowx <- self$tiledb_array(return_as = "arrow",
                                  selected_ranges = list(index = qry$index))[]

      i <- arrowx$num_columns
      res <- lapply(new_columns, function(.s) {
        arrowx <<- arrowx$AddColumn(i, arrow::field(.s, arrow::float64()), tsdata[[.s]])
        i <<- i + 1
      })

      # evolve schema with new columns
      self$add_symbols(new_columns)
      self$reopen("WRITE")

      # Next write new data

      # TODO: zero copy?
      # Not yet, see tiledb-r issue #847

      arr <- self$object
      arr[] <- arrowx

      invisible(self)
    },
    #' @description Get subset of time series array.
    #'
    #' @param i Extract time index rows. One of the following:
    #'
    #'   - An `ISO-8601` style range string that represents the rows to extract.
    #'   - A numeric vector with elements within index range (Not supported).
    #'   - A logical vector of the same length as index (Not supported).
    #'   - A time-based vector (Not supported).
    #'
    #' @param cols A character vector of column names (`TileDB` attributes).
    #' @param as_xts Should the output be returned as `xts`? Default is `FALSE`.
    #'
    #' @return An object of class `data.table`.
    #'
    subset = function(i = "1970-01-01/", cols = character(), as_xts = FALSE) {

      if (is.character(i)) {
        # i. date string range
        # ii.  time of day recurrent selection, e.g, ["T09:30:30/T16:10"] (not supported)
        tod <- "(^/T)|(^T.*?/T)|(^T.*/$)"

        if (length(i) == 1 && !identical(integer(), grep(tod, i[1]))) {

          cli::cli_abort("Time of day pattern is not supported")

        } else {

          qry <- private$index_query(i, self$tclass[1], tz = self$tzone)

          x <- self$tiledb_array(attrs = cols,
                                 return_as = "arrow",
                                 selected_ranges = list(index = qry$index))[]

        }

      } else {
        cli::cli_abort("Not supported")
      }

      # extract query status/stats
      qstatus <- attr(x, "query_status")
      qstats <- attr(x, "query_statistics")

      x <- data.table::as.data.table(x)

      # assign back query status/stats
      data.table::setattr(x, "query_status", value = qstatus)
      data.table::setattr(x, "query_statistics", value = qstats)

      # set tzone attr to index
      data.table::setattr(x$index, "tzone", value = self$tzone)

      if(as_xts) {
        xts::as.xts(x[])
      } else {
        x[]
      }

    },

    #' @description Get the index domain.
    #'
    #' @return A vector of length two with min, max values of index domain.
    #'
    domain_index = function() {

      dim_index <- self$dimensions()[["index"]]

      dom_index <- tiledb::domain(dim_index)

      tc <- self$tclass[1]
      if (tc == "Date") {
        i  <- as.Date(dom_index)
      } else if (tc == "POSIXct"){
        i <- as.numeric(dom_index) / 1000
        i <- as.POSIXct(i, tz = "UTC")
      } else {
        cli::cli_abort("Unsupported time-series index class")
      }

      i

    },

    #' @description Get the index range of a series.
    #'
    #' @param series A character vector of symbol names. Defaults to `NULL`
    #' to return all series.
    #'
    #' @return An object of class `data.table`.
    #'
    index_range = function(series = NULL) {

      if (is.null(series)) {
        series = character()
      }

      x <- self$tiledb_array(attrs = series,
                             return_as = "arrow")[]

      x <- data.table::as.data.table(x)
      x <- data.table::melt(x, id.vars = "index",
                            variable.name = "symbol",
                            variable.factor = FALSE,
                            value.name = "value",
                            na.rm = TRUE)
      index <- NULL

      x1 <- x[x[, .I[which.min(index)], by = "symbol"]$V1, c("symbol", "index")]
      x2 <- x[x[, .I[which.max(index)], by = "symbol"]$V1, c("symbol", "index")]

      en_date <- i.index <- NULL
      x1[x2, on = "symbol", en_date := i.index]

      data.table::setnames(x1, old = "index", new = "st_date")

      x1[]

    },

    #' @description Get the start index of a series.
    #'
    #' @param series A character vector of symbol names. Defaults to `NULL`
    #' to return all series.
    #'
    #' @return An object of class `data.table`.
    #'
    index_start = function(series = NULL) {
      self$first(n = 1, series = series)[, c("symbol", "index")]
    },
    #' @description Get the last index of a series.
    #'
    #' @param series A character vector of symbol names. Defaults to `NULL`
    #' to return all series.
    #'
    #' @return An object of class `data.table`.
    #'
    index_last = function(series = NULL) {

      self$last(n = 1, series = series)[, c("symbol", "index")]

    },
    #' @description Get the N first values of a series.
    #'
    #' @param n A number of observations to return.
    #' @param series A character vector of symbol names.
    #'
    #' @return An object of class `data.table`.
    #'
    first = function(n = 1, series = character()) {

      if (is.null(series)) {
        series = character()
      }

      dt <- self$tiledb_array(return_as = "arrow", attrs = series)[]

      dt <- data.table::as.data.table(dt)

      # set tzone attr to index
      data.table::setattr(dt$index, "tzone", value = self$tzone)

      dt <- data.table::melt(dt, id.vars = "index",
                             variable.name = "symbol",
                             variable.factor = FALSE,
                             value.name = "value",
                             na.rm = TRUE)

      data.table::setorderv(dt, c("index"), order = 1)
      dt[dt[, .I[seq_len(.N) <= n], by  = "symbol"]$V1, ]

    },
    #' @description Get the N last values of a series.
    #'
    #' @param n A number of observations to return.
    #' @param series A character vector of symbol names.
    #'
    #' @return An object of class `data.table`.
    #'
    last = function(n = 1, series = character()) {

      if (is.null(series)) {
        series = character()
      }

      dt <- self$tiledb_array(return_as = "arrow", attrs = series)[]

      dt <- data.table::as.data.table(dt)

      # set tzone attr to index
      data.table::setattr(dt$index, "tzone", value = self$tzone)

      dt <- data.table::melt(dt, id.vars = "index",
                             variable.name = "symbol",
                             variable.factor = FALSE,
                             value.name = "value",
                             na.rm = TRUE)

      data.table::setorderv(dt, c("index"), order = -1)
      data.table::setorderv(
        dt[dt[, .I[seq_len(.N) <= n], by  = "symbol"]$V1, ],
        c("index"), order = 1
      )[]
    },

    #- ---- ----- - -

    #' @description Retrieve data from Array using symbol names.
    #'
    #' @param syms A character vector with symbol names.
    #'
    #' @return An object of class `data.table`.
    #'
    query_symbols = function(syms){

      dt <- self$tiledb_array(return_as = "arrow", attrs = syms)[]
      dt <- data.table::as.data.table(dt)

      dt

    },
    #' @description Remove symbols from a Chronos Array.
    #'
    #' @param syms A character vector with symbol names for deletion.
    #'
    #' @return The object, invisibly.
    #'
    remove_symbols = function(syms) {

      lapply(syms, function(.s) {
        ase <- tiledb::tiledb_array_schema_evolution()
        ase <- tiledb::tiledb_array_schema_evolution_drop_attribute(ase, .s)
        ase <- tiledb::tiledb_array_schema_evolution_array_evolve(ase, self$uri)
      })

      invisible(self)
    },

    #' @description Rename a symbol.
    #'
    #' @param x A character with symbol name to be renamed.
    #' @param newname A character with new symbol name.
    #'
    #' @return The object, invisibly.
    #'
    rename_symbol = function(x, newname) {

      private$check_scalar_character(newname)

      if (!self$symbol_exists(x)) {
        cli::cli_abort(
          "{.arg {deparse(substitute(x))}} series not found.",
          call = NULL
        )
      }

      obs <- self$query_symbols(x)
      colnames(obs) <- c("index", newname)

      self$ingest(obs)
      self$remove_symbols(x)
      invisible(self)
    },

    #' @description Add symbols to Chronos Array.
    #'
    #' It is used by ingest method to evolve schema with new columns
    #' of numeric type.
    #'
    #' @param syms A character vector with symbol names to be added as columns.
    #'
    #' @return The object, invisibly.
    #'
    add_symbols = function(syms) {

      if (any(!nzchar(trimws(syms)))) {
        cli::cli_abort("Symbol names should not be an empty string")
      }

      lapply(syms, function(.s) {
        attr <- tiledb::tiledb_attr(
          name = .s,
          type = "FLOAT64",
          ncells = 1,
          nullable = TRUE,
          filter_list = .tiledb_flist(level = -1, name = "ZSTD"))

        ase <- tiledb::tiledb_array_schema_evolution()

        ase <- tiledb::tiledb_array_schema_evolution_add_attribute(ase, attr)

        tiledb::tiledb_array_schema_evolution_array_evolve(ase, self$uri)
      })

      invisible(self)
    },

    #' @description Get all symbols from a Chronos Array.
    #'
    #' @return A character vector with symbol names.
    #'
    symbols = function() {

      self$attrnames()
    },

    #' @description Check a symbol exists in a Chronos Array.
    #'
    #' @param syms A string with symbol name.
    #'
    #' @return A boolean.
    #'
    symbol_exists = function(syms) {

      private$check_scalar_character(syms)

      syms %in% self$symbols()

    },
    #' @description Count unique symbols in a Chronos Array.
    #'
    #'
    #' @return A numeric with unique number of symbols
    #'  in the Array.
    #'
    count_symbols = function() {

      length(self$attrnames())
    }
  ), # End public
  private = list(
    # TODO: what if it is really big? option to decide at init?
    .index = NULL,
    .tclass = NULL,
    .tzone = NULL,

    # Built index ranges
    #
    # i ISO8601
    # tc tclass
    # tz time-zone
    index_query = function(i, tc, tz) {

      # Sub-setting by date style strings

      if (length(i) == 1) {

        dt <- xts::.parseISO8601(i, tz = tz)

        if (is.na(dt$last.time)) {
          dt$last.time <- as.POSIXct(Sys.Date(), tz = tz)
        }

        if (tc == "Date") {
          dt <- sapply(dt, as.Date, tz = tz)

          out <- list(index = cbind(dt[[1]], dt[[2]]))

        } else {

          out <- list(index = cbind(dt[1], dt[2]))
        }

      } else{
        # Ranges  e.g c("2016", "2017") or c("2016-09-21","2016-09-26/")
        # Handle a vector of ISO8601 and return a matrix with ranges

        if (tc == "Date") {

          dt <- lapply(i, function(.x) {
            .tmp <- xts::.parseISO8601(.x, tz = tz)

            if (is.na(.tmp$last.time))
              .tmp$last.time <- as.POSIXct(Sys.Date())

            sapply(.tmp, as.Date)
          })

          dt <- do.call(rbind, dt)

        } else {

          dt <- sapply(i, function(.x) {
            .tmp <- xts::.parseISO8601(.x, tz = tz)

            if (is.na(.tmp$last.time)) {
              .tmp$last.time <- as.POSIXct(Sys.Date())
            }

            .tmp

          }, simplify = FALSE)

          dt <- do.call(rbind, dt)

        }

        out <- list(index = dt)
      } # multiple ranges as matrix

      out
    }

  )
)
```
