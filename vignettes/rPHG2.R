## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  fig.align = "center",
  external = TRUE,
  echo = TRUE,
  warning = FALSE
)

library(rPHG2)
source("../tests/testthat/setup.R")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  PHGLocalCon()

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  system.file("extdata", "LineA.h.vcf", package = "rPHG2")

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
hVcfFiles <- system.file(
    "extdata", 
    c("LineA.h.vcf", "LineB.h.vcf"), 
    package = "rPHG2"
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
localCon <- hVcfFiles |> PHGLocalCon()

localCon

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
srvCon <- "phg.maizegdb.org" |> PHGServerCon()

srvCon

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  "www.my-unsecure-phg.org" |>
#      PHGServerCon(
#          port     = 5300,
#          protocol = "http"
#      )

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
graph <- localCon |> buildHaplotypeGraph()

graph

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
graph |> javaRefObj()

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
graph |> readSamples()

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
graph |> readRefRanges()

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
m <- graph |> readHapIds()

# Show only first 3 columns
m[, 1:3]

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
graph |> readHapIdMetaData()

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
graph |> readHapIdPosMetaData()

