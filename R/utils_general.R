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
isValidColor <- function(color) {
    isHex <- grepl(
        pattern = "^#([A-Fa-f0-9]{3}|[A-Fa-f0-9]{4}|[A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$",
        x = color
    )
    isNamedColor <- color %in% colors()

    return(isHex || isNamedColor)
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
#' @examples
#' # Creating a named vector with a static name
#' vectorElement <- "value" %T% 5
#' print(vectorElement)
#'
#' # Creating a combined named vector with both static and dynamic names
#' nameForElement <- "dynamicName"
#' combinedVector <- c("staticName" %T% 123, nameForElement %T% 456)
#' print(combinedVector)
#'
#' # Note: `%T%` can be especially useful in data manipulation where dynamic
#' # naming of elements is required. Here's a more complex example:
#' keyMap <- data.frame(key = "key_01")
#' dynamicVector <- keyMap$key %T% 10
#' print(dynamicVector)
#'
#' @export
`%T%` <- function(lhs, rhs) {
    nRhs <- eval(rhs)
    nLhs <- eval(lhs)

    if (length(nLhs) == 0) {
        rlang::abort("Failed evaluation of 'lhs' (empty character)")
    } else {
        names(nRhs) <- nLhs
        return(nRhs)
    }
}


