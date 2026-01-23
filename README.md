# R6.tiledb

<!-- badges: start -->

[![repo-status](https://img.shields.io/badge/repo%20status-stable-brightgreen.svg)](#) [![CRAN status](https://www.r-pkg.org/badges/version/R6.tiledb)](https://CRAN.R-project.org/package=R6.tiledb) [![coverage](https://img.shields.io/badge/coverage-94.2%25-blue.svg)](#)

<!-- badges: end -->

An extension to [TileDB-R](https://cran.r-project.org/web/packages/tiledb/index.html) interface.

[TileDB Embedded](https://github.com/TileDB-Inc/TileDB) is a high performant storage engine for dense and sparse multi-dimensional arrays. `R6.tiledb` is built on top of [R6](https://cloud.r-project.org/web/packages/R6/index.html) object-oriented system and offers base classes that represent `TileDB` Arrays and Groups, as well as subclasses that encapsulate additional methods, functional wrappers and S3 methods.

The package can be helpful to create domain specific applications and unified data architectures by extending the base classes, or using its functional interface to work with existing TileDB resources.

## Usage

To use `R6.tiledb`, first create a TileDB resource:

``` r

  library(R6.tiledb)

  # URI path
  uri <- tempfile()
  
  # Demo array from 'UCBAdmissions' built-in dataset
  demo_UCBAdmissions_array(uri)
  
  # Create an instance that represents a TileDB Array
  arrobj <- tdb_array(uri)
  
  arrobj
#> R6Class: <TileDBArrayExp>
#> → URI Basename: file3ea47b75133b
#>   • Dimensions: "Dept" and "Gender"
#>   • Attributes: "Admit" and "Freq"
```

Using class methods :

``` r

 # Query the number of fragments 
 arrobj$frag_num()
 #> [1] 3

 # Does the array have factors (enums)
 arrobj$any_enums()
 #>[1] TRUE

  # Run query on 'Dept' dimension
  arr <- arrobj$tiledb_array(selected_points = list(Dept = "A"), return_as = "data.frame")
  arr[]
#>   Dept Gender    Admit Freq
#> 1    A Female Admitted   89
#> 2    A Female Rejected   19
#> 3    A   Male Rejected  313
#> 4    A   Male Admitted  512
```

For more examples, see [Getting Started]().

## Installation

Development version from GitHub:

``` r
# pak
pak::pkg_install("cgiachalis/R6.tiledb")

# remotes
remotes::install_github("cgiachalis/R6.tiledb")
```

## Acknowledgements

`R6.tiledb` has modified the R6 base classes: `TileDBArray` and `TileDBGroup` from `TileDB` [SOMA-R](https://github.com/single-cell-data/TileDB-SOMA/tree/main/apis/r) project -
but can not be considered drop-in replacement[^1].

[^1]: `TileDBArray` and `TileDBGroup` names are no longer used by SOMA-R

## Resources

-   ***Introduction to Arrays*** [<a href="https://documentation.cloud.tiledb.com/academy/structure/arrays/introduction/">TileDB Academy</a>]

-   Dirk Eddelbuettel and Aaron Wolen (2021), ***Using TileDB with R: An Introductory Tutorial*** [<a href="https://dirk.eddelbuettel.com/papers/useR2021_tiledb_tutorial.pdf">slides</a>]

-   Aaron Wolen and Dirk Eddelbuettel (2021), ***Infinitely Scalable Data Analysis: Using R with TileDB Tutorial*** [<a href="https://dirk.eddelbuettel.com/papers/bioc2021_tiledb_talk.pdf">slides</a>]

-   ***A deep dive into the TileDB data format & storage engine*** [<a href="https://www.youtube.com/watch?v=GHJ16KyqGKI&t=3387s">video</a>]

## Disclaimer

`R6.tiledb` is not an official TileDB Inc product.

