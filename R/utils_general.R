## ----
# Convert camelCase to snake_case
#
# @param x A character vector of strings
camelToSnake <- function(x) {
    sc <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
    sc <- tolower(sc)
    return(sc)
}


## ----
#' @title
#' User-defined function for pre-evaluated names in vectors
#'
#' @description
#' Generates a named element. Can be used in conjunction with \code{c()} to
#' create a collection of named elements
#'
#'
#' @param lhs
#' Name for element in vector. Can be either static string or object that will
#' need to be evaluated.
#' @param rhs
#' Element in vector
#'
#' @return
#' A named vector
#'
#' @export
`%T%` <- function(lhs, rhs) {
    nRhs <- eval(rhs)
    names(nRhs) <- eval(lhs)
    return(nRhs)
}


