# /// Helpers ///////////////////////////////////////////////////////

## ----
# Find text lines for validation sans "comments"
#
# @filePath plain text file to check
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
# Validate spliced text lines with proper header IDs
#
# @param line A line of text containing headers and delimiters
# @param validHeaders A character vector containing vetted IDs
# @param delimiter What delimiter "character" should be used?
validateHeaders <- function(line, validHeaders, delimiter = "\t") {
    if (!is.null(line)) {
        splitLine <- unlist(strsplit(line, delimiter))
        return(all(splitLine %in% validHeaders))
    } else {
        return(FALSE)
    }
}



# /// Primary functions /////////////////////////////////////////////

## ----
# Check if Anchor file is "valid" (vectorized)
#
# @param anchorPaths .anchorspro file paths
isValidAnchor <- function(anchorPaths) {
    validities <- vapply(anchorPaths, function(aPath) {
        con <- file(aPath, open = "r")
        lineCheck <- readLines(con, n = 1, warn = FALSE)
        close(con)

        if (grepl("#anchorwave", lineCheck)) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }, logical(1))

    return(validities)
}


## ----
# Check if input gVCF file is "valid" (vectorized)
#
# @param gVcfPaths A gVCF file path
isValidGvcf <- function(gVcfPaths) {
    validHeaders <- PHG_METRICS$VALID_GVCF_HEADERS

    validities <- vapply(gVcfPaths, function(gPath) {
        firstLine <- findFirstNonCommentLine(gPath)
        validateHeaders(firstLine, validHeaders)
    }, logical(1))

    return(validities)
}


## ----
# Read metric files (assumes tab-delimited structure)
#
# @param metricFiles A list of files
readMetricFiles <- function(metricFiles) {
    inMemDfs <- lapply(
        X = metricFiles,
        FUN = function(x) {
            message(" * reading data for: ", basename(x))
            tmp <- tibble::as_tibble(
                read.table(
                    file         = x,
                    header       = TRUE,
                    sep          = "\t",
                    comment.char = "#"
                )
            )
            colnames(tmp) <- camelToSnake(colnames(tmp))
            return(tmp)
        }
    )

    names(inMemDfs) <- tools::file_path_sans_ext(basename(metricFiles))

    return(inMemDfs)
}





