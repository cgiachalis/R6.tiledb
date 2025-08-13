
# R6.tiledb

<!-- badges: start -->
[![repo-status](https://img.shields.io/badge/repo%20status-stable-brightgreen.svg)](#)
[![CRAN status](https://www.r-pkg.org/badges/version/R6.tiledb)](https://CRAN.R-project.org/package=R6.tiledb)
[![coverage](https://img.shields.io/badge/coverage-93.2%25-blue.svg)](#)
<!-- badges: end -->

`R6.tiledb` is an extension of [TileDB-R](https://cran.r-project.org/web/packages/tiledb/index.html) client built on top of [R6](https://cloud.r-project.org/web/packages/R6/index.html) object-oriented system. It includes base classes that represent `TileDB` arrays and groups with minimum integrated functionality so that can be extended with focus on building domain specific applications. Also, it provides convenient functional wrappers with additional methods that make it easier to work with arrays.

### Impetus for `R6.tiledb`

In short, to narrow the gab between data engineering and analytics integration:

-   [TileDB Embedded](https://github.com/TileDB-Inc/TileDB) is a high performant storage engine that you can efficiently store and query any data, making ideal to build unified data architectures

-   [R](https://www.r-project.org/) is a software with unparalleled analytics ecosystem

-   Combined, one can build powerful and versatile applications with tight integration of domain specific analytics and workflows

-   Using `R6` classes we can form complex but robust interactions between arrays and groups

In R ecosystem, the `TileDB` [SOMA-R](https://github.com/single-cell-data/TileDB-SOMA/tree/main/apis/r) project stands out as an example of domain specific application with above characteristics and this repo extracts its initial base classes: `TileDBArray` and `TileDBGroup` and modifies them for our specific needs.

## Usage

For existing arrays you can get started by using `tdb_array()`:

``` r

library(R6.tiledb)

  # Create new array on disk ----

  # temp uri path
  uri <- tempfile()
  
 demo_UCBAdmissions_array(uri)
  
  # Create an instance that represents a TileDB Array ----
  arrobj <- tdb_array(uri)
  
  arrobj
#> R6Class: <TileDBArrayExp>
#> → URI Basename: file3ea47b75133b
#>   • Dimensions: "Dept" and "Gender"
#>   • Attributes: "Admit" and "Freq"
  
  arrobj$frag_num()
#> [1] 3

> arrobj$any_enums()
[1] TRUE

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

## Resources

-   ***Introduction to Arrays*** [<a href="https://documentation.cloud.tiledb.com/academy/structure/arrays/introduction/">TileDB Academy</a>]

-   Dirk Eddelbuettel and Aaron Wolen (2021), ***Using TileDB with R: An Introductory Tutorial*** [<a href="https://dirk.eddelbuettel.com/papers/useR2021_tiledb_tutorial.pdf">slides</a>]

-   Aaron Wolen and Dirk Eddelbuettel (2021), ***Infinitely Scalable Data Analysis: Using R with TileDB Tutorial*** [<a href="https://dirk.eddelbuettel.com/papers/bioc2021_tiledb_talk.pdf">slides</a>]

-   ***A deep dive into the TileDB data format & storage engine*** [<a href="https://www.youtube.com/watch?v=GHJ16KyqGKI&t=3387s">video</a>]
