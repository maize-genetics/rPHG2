# rPHG2

<!-- badges: start -->
[![Life Cycle Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/maize-genetics/rPHG2/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/maize-genetics/rPHG2/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/maize-genetics/rPHG2/graph/badge.svg?token=BEEI8KZ96H)](https://codecov.io/gh/maize-genetics/rPHG2)
<!-- badges: end -->

`rPHG2` is a system to interact with and retrieve information from **version 2** 
of the Practical Haplotype Graph (PHGv2) - a general, graph-based, computational 
framework for genotype inference. This is accomplished by leveraging the 
[Breeding](https://brapi.org/) and 
[PHG](https://github.com/maize-genetics/phg_v2) APIs.

## Installation

You can install the development version of rphg2 from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("maize-genetics/rPHG2")
```


## Usage

``` r
# Initialize with PHGv2 JARs
initPhg("my/phg/jar/path")


# Create a connection
locCon <- list.files("my/hvcf/dir") |>
    PHGLocalCon()


# Build a graph
graph <- locCon |> buildHaplotypeGraph()


# Read data into R
phgDs <- graph |> readPhgDataSet()


# Identify areas of interest
gr <- GRanges(
    seqnames = c("1", "2"),
    ranges = IRanges(
        c(100, 800),
        c(400, 900)
    )
)

phgDs |> 
    filterSamples(c("B73", "Ky21", "Mo17")) |> 
    filterRefRanges(gr)
```


## Lifecycle
[![Life Cycle Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`rPHG2` is a relatively new R package and may be subject to changes in
workflow and function definitions. If you would like to see anything added
or run into problems, please feel free to contact the team using the 
[issues page](https://github.com/maize-genetics/rPHG2/issues).


## Learning rPHG2
If installed locally, you can read the "Introduction to rPHG2" vignette by
using:

``` r
vignette("rPHG2")
```

Alternatively, the package's [website](https://rphg2.maizegenetics.net) is
another great place to read up on function usage, code changes, and
workflows.


