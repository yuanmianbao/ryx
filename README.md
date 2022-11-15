
# ryx 

<!-- badges: start -->
<!-- badges: end -->

The goal of ryx  is to tell you the correlation among the variables you are interested in for a data set. 

## Installation

You can install the development version of ryx  from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yuanmianbao/ryx")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ryx )
myryx <- (mtcars, cyl)
print.ryx(myryx)
plot.ryx(myryx)
summary.ryx(myryx)
## basic example code
```

