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
# Check if the JVM is Initialized
#
# @description
# This function checks whether the Java Virtual Machine (JVM) has been
# initialized using the `rJava` package.
#
# The function attempts to retrieve the current Java version by calling a Java
# method. If the JVM is not initialized, an error is caught, and the function
# returns `FALSE`. If the JVM is initialized, it returns `TRUE`.
#
# @return
# A logical value indicating whether the JVM has been initialized. Returns
# `TRUE` if the JVM is initialized, otherwise `FALSE`.
isJvmInitialized <- function() {
    tryCatch({
        # Attempt to get the current Java version
        javaVersion <- rJava::.jcall(
            obj       = "java/lang/System",
            returnSig =  "S",
            method    = "getProperty", "java.version"
        )
        return(TRUE)
    }, error = function(e) {
        return(FALSE)
    })
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


