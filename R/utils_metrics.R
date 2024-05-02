## ----
findFirstNonCommentLine <- function(filePath) {
    connection <- file(filePath, open = "r")
    firstLine <- NULL
    try({
        while (length(line <- readLines(connection, n = 1, warn = FALSE)) > 0) {
            if (!startsWith(line, "#")) {
                firstLine <- line
                break
            }
        }
    })
    close(connection)
    return(firstLine)
}


## ----
validateHeaders <- function(line, validHeaders) {
    if (!is.null(line)) {
        splitLine <- unlist(strsplit(line, "\t"))
        return(all(splitLine %in% validHeaders))
    } else {
        return(FALSE)
    }
}


## ----
# Check if input gVCF file is "valid"
#
# @param gVcfPaths A gVCF file path
isValidGvcf <- function(gVcfPaths) {
    # Assume this constant is defined somewhere in your environment
    validHeaders <- PHG_METRICS$VALID_GVCF_HEADERS

    # Use vapply to apply the validation process to each file path
    validities <- vapply(gVcfPaths, function(gPath) {
        firstLine <- findFirstNonCommentLine(gPath)
        validateHeaders(firstLine, validHeaders)
    }, logical(1))  # logical(1) defines the type and length of the output for each iteration

    return(validities)
}



## ----
# Check if Anchor file is "valid"
#
# @param a An .anchorspro file
isValidAnchor <- function(a) {
    con <- file(a, open = "r")
    lineCheck <- readLines(con, n = 1, warn = FALSE)
    close(con)

    if (grepl("#anchorwave", lineCheck)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


