# R6.tiledb

An extension to
[TileDB-R](https://cran.r-project.org/web/packages/tiledb/index.html)
interface.

[TileDB Embedded](https://github.com/TileDB-Inc/TileDB) is a high
performant storage engine for dense and sparse multi-dimensional arrays.
`R6.tiledb` is built on top of
[R6](https://cloud.r-project.org/web/packages/R6/index.html)
object-oriented system and extends
[tiledb](https://github.com/TileDB-Inc/TileDB-R) R package with base
classes, which represent `TileDB` Arrays and Groups, as well as with
subclasses that encapsulate additional methods. It provides also
functional wrappers, convenient utilities and S3 methods.

The package can be helpful to create domain specific applications and
unified data architectures by extending the base classes, or using its
functional interface to work with existing TileDB resources.

## Usage

To use `R6.tiledb`, first create a TileDB resource:

``` r

 library(R6.tiledb)

 # URI path
 uri <- tempfile()
  
 # Demo array from 'UCBAdmissions' built-in dataset
 demo_UCBAdmissions_array(uri)
  
 # Create an instance that represents the array
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
 arr <- arrobj$tiledb_array(selected_points = list(Dept = "A"),
                            return_as = "data.frame")
                            
 # Materialise array as 'data.frame'                   
 arr[]
 #>   Dept Gender    Admit Freq
 #> 1    A Female Admitted   89
 #> 2    A Female Rejected   19
 #> 3    A   Male Rejected  313
 #> 4    A   Male Admitted  512
```

See [Getting
Started](https://cgiachalis.github.io/R6.tiledb/articles/getting_started.html)
documentation for more examples.

## Installation

Development version from GitHub:

``` r
# pak
pak::pkg_install("cgiachalis/R6.tiledb")

# remotes
remotes::install_github("cgiachalis/R6.tiledb")
```

## Acknowledgements

`TileDB`
[SOMA-R](https://github.com/single-cell-data/TileDB-SOMA/tree/main/apis/r)
project for extracting the R6 base classes from: `TileDBArray` and
`TileDBGroup` which this repo has modified extensively that can not be
considered drop-in replacements[¹](#fn1).

## Resources

- ***Introduction to Arrays*** \[[TileDB
  Academy](https://documentation.cloud.tiledb.com/academy/structure/arrays/introduction/)\]

- Dirk Eddelbuettel and Aaron Wolen (2021), ***Using TileDB with R: An
  Introductory Tutorial***
  \[[slides](https://dirk.eddelbuettel.com/papers/useR2021_tiledb_tutorial.pdf)\]

- Aaron Wolen and Dirk Eddelbuettel (2021), ***Infinitely Scalable Data
  Analysis: Using R with TileDB Tutorial***
  \[[slides](https://dirk.eddelbuettel.com/papers/bioc2021_tiledb_talk.pdf)\]

- ***A deep dive into the TileDB data format & storage engine***
  \[[video](https://www.youtube.com/watch?v=GHJ16KyqGKI&t=3387s)\]

## Disclaimer

Please note that this project is not an official TileDB-Inc product.

------------------------------------------------------------------------

1.  R6 class names: `TileDBArray` and `TileDBGroup` are no longer used
    by SOMA-R
