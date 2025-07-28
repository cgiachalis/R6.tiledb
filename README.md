
# R6.tiledb

<!-- badges: start -->
[![repo-status](https://img.shields.io/badge/repo%20status-stable-brightgreen.svg)](#)
[![CRAN status](https://www.r-pkg.org/badges/version/R6.tiledb)](https://CRAN.R-project.org/package=R6.tiledb)
[![coverage](https://img.shields.io/badge/coverage-86.7%25-yellowgreen.svg)](#)
<!-- badges: end -->

`{R6.tiledb}` is an extension of [TileDB-R](https://cran.r-project.org/web/packages/tiledb/index.html) client built on top of [R6](https://cloud.r-project.org/web/packages/R6/index.html) object-oriented system. It includes base classes that represent `TileDB` arrays and groups with minimum integrated functionality that can be extended with focus on building domain specific applications. Moreover, it offers high level `R6` classes and convenient functional wrappers with additional array methods.

### Motivation

The impetus is to make data engineering a little easier and narrow the gab with analytics integration:

-   [TileDB Embedded](https://github.com/TileDB-Inc/TileDB) is a high performant storage engine that you can efficiently store any data and [R](https://www.r-project.org/) is a software with unparalleled analytics ecosystem

-   Combined these tools we can build powerful and versatile applications with tight integration of analytics

-   Using `R6` classes allow us to form complex interactions between arrays and groups ideal for developing any data architecture

As an example of domain specific application with above characteristics is the `TileDB` [SOMA-R](https://github.com/single-cell-data/TileDB-SOMA/tree/main/apis/r) from which `{R6.tiledb}` package extracts its initial base classes: `TileDBArray` and `TileDBGroup` and modifies them accordingly.

## Usage

To get started if you already have an existing array is to use [tdb_array()]() :

``` r

library(R6.tiledb)

  # Create new array on disk ----

  # temp uri path
  uri <- tempfile()
  

  idx_cols <- c("Dept", "Gender")
  d1 <- as.data.frame(UCBAdmissions)

  # Create array and ingest data
  tiledb::fromDataFrame(d1, uri, col_index = idx_cols, sparse = TRUE)

  
  # Create an instance that represents a TileDB Array ----
  arrobj <- tdb_array(uri)
  
  arrobj
#> R6Class: <TileDBArrayExp>
#> → URI Basename: file3ea47b75133b
#>   • Dimensions: "Dept" and "Gender"
#>   • Attributes: "Admit" and "Freq"
  
  arrobj$frag_num()
#> [1] 1

  
  # Query Dept dimension
  arr <- arrobj$tiledb_array(selected_points = list(Dept = "A"), return_as = "data.frame")
  arr[]
#>   Dept Gender    Admit Freq
#> 1    A Female Admitted   89
#> 2    A Female Rejected   19
#> 3    A   Male Rejected  313
#> 4    A   Male Admitted  512
```

## Installation

You can install the development version of `R6.tiledb` from GitHub:

``` r
remotes::install_github("cgiachalis/R6.tiledb")
```

## References

-   Dirk Eddelbuettel and Aaron Wolen (2021), ***Using TileDB with R: An Introductory Tutorial*** [<a href="https://dirk.eddelbuettel.com/papers/useR2021_tiledb_tutorial.pdf">slides</a>]

-   Aaron Wolen and Dirk Eddelbuettel (2021), ***Infinitely Scalable Data Analysis: Using R with TileDB Tutorial*** [<a href="https://dirk.eddelbuettel.com/papers/bioc2021_tiledb_talk.pdf">slides</a>]

-   ***A deep dive into the TileDB data format & storage engine*** [<a href="https://www.youtube.com/watch?v=GHJ16KyqGKI&t=3387s">video</a>]

-   ***Introduction to Arrays*** [<a href="https://documentation.cloud.tiledb.com/academy/structure/arrays/introduction/">TileDB Academy</a>]

