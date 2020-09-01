addressable
============

![R-CMD-check](https://github.com/ropensci/addressable/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/ropensci/addressable/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/addressable)

Email Address Validation

## Install


```r
remotes::install_github("ropensci/addressable@main")
```


```r
library("addressable")
```

## Address


```r
x <- Address$new("User+tag@example.com")
x$host$host_name
#> [1] "example.com"
x$local$local
#> [1] "user+tag"
x$valid()
#> [1] TRUE
x$fail()
#> NULL
```


```r
x <- Address$new("user1")
x$valid()
#> [1] FALSE
x$fail()
#> [1] "Invalid Domain Name"
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/addressable/issues).
* License: MIT
* Get citation information for `addressable` in R doing `citation(package = 'addressable')`
* Please note that this project is released with a [Contributor Code of Conduct][coc]. By participating in this project you agree to abide by its terms.

[coc]: https://github.com/ropensci/addressable/blob/maddressable/CODE_OF_CONDUCT.md
