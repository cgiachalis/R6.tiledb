
# R6.tiledb

<!-- badges: start -->
[![repo-status](https://img.shields.io/badge/repo%20status-stable-brightgreen.svg)](#)
[![CRAN status](https://www.r-pkg.org/badges/version/R6.tiledb)](https://CRAN.R-project.org/package=R6.tiledb)
[![coverage](https://img.shields.io/badge/coverage-61.2%25-orange.svg)](#)
<!-- badges: end -->


`{R6.tiledb}` is an extension of [TileDB-R](https://cran.r-project.org/web/packages/tiledb/index.html) client built on top of [R6](https://cloud.r-project.org/web/packages/R6/index.html) object-oriented system. It includes base classes that represent `TileDB` arrays and groups with minimum integrated functionality that can be extended with focus on building domain specific applications. Moreover, it offers high level `R6` classes and convenient functional wrappers that integrate additional array methods.

### Motivation

The impetus for this project is to narrow the gap between data engineering and analytics:

-   [TileDB Embedded](https://github.com/TileDB-Inc/TileDB) is a high performant storage engine that you can efficiently store any data and [R](https://www.r-project.org/) is a software with unparalleled analytics ecosystem

-   Combined these tools you can build powerful and versatile applications with tight integration of analytics

-   Using `R6` classes we can define complex interactions between arrays and groups ideal for building any data architecture

As an example of domain specific application with above characteristics is `TileDB` [SOMA-R](https://github.com/single-cell-data/TileDB-SOMA/tree/main/apis/r) from which `{R6.tiledb}` package extracts its initial base classes: `TileDBArray` and `TileDBGroup` and modifies them accordingly.

## Installation

You can install the development version from GitHub:

``` r
remotes::install_github("cgiachalis/R6.tiledb")

```

## Usage

TODO

``` r

uri <- tempdir()

arrobj <- tdb_array(uri)
```

See the [Introduction](https://r6.r-lib.org/articles/Introduction.html) article for usage examples.

## References

-   Dirk Eddelbuettel and Aaron Wolen (2021), ***Using TileDB with R: An Introductory Tutorial*** [<a href="https://dirk.eddelbuettel.com/papers/useR2021_tiledb_tutorial.pdf">slides</a>]

-   Aaron Wolen and Dirk Eddelbuettel (2021), ***Infinitely Scalable Data Analysis: Using R with TileDB Tutorial*** [<a href="https://dirk.eddelbuettel.com/papers/bioc2021_tiledb_talk.pdf">slides</a>]

-   ***A deep dive into the TileDB data format & storage engine*** [<a href="https://www.youtube.com/watch?v=GHJ16KyqGKI&t=3387s">video</a>]

-   ***Introduction to Arrays*** [<a href="https://documentation.cloud.tiledb.com/academy/structure/arrays/introduction/">TileDB Academy</a>]



