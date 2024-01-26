# rPHG2

<!-- badges: start -->
[![Life Cycle Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`rPHG2` is a system to interact with and retrieve information from **version 2** 
of the Practical Haplotype Graph (PHGv2) - a general, graph-based, computational 
framework for genotype inference. This is accomplished by leveraging the 
[Breeding](https://brapi.org/) and 
[PHG](https://github.com/maize-genetics/phg_v2) APIs.

## Installation

You can install the development version of rphg2 from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maize-genetics/rPHG2")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rphg2)

phgCon <- PHGServerCon("localhost", 8080, "http")

phgCon |> readSamples()
```

