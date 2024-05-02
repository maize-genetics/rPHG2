## ----
# Convert camelCase to snake_case
#
# @param x A character vector of strings
camelToSnake <- function(x) {
    sc <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
    sc <- tolower(sc)
    return(sc)
}


