## ----
# Check if input gVCF file is "valid"
#
# @param g A gVCF file path
isValidGvcf <- function(g) {
    # Open a connection to the file
    con <- file(g, open = "r")

    # Initialize variable to store the first data line
    fl <- NULL

    # Read lines one by one until the first non-comment line is found
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        # Check if the line is not a comment
        if (!startsWith(line, "#")) {
            fl <- line
            break
        }
    }

    # Close the file connection
    close(con)

    # Check if a non-comment line was found
    if (!is.null(fl)) {
        flV <- unlist(strsplit(fl, "\t"))
    } else {
        return(FALSE)
    }

    # Check column headers (not sure there is any other way)
    if (any(!flV %in% PHG_METRICS$VALID_GVCF_HEADERS)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}


## ----
# Check if Anchor file is "valid"
#
# @param a An .anchorspro file
isValidAnchor <- function(a) {
    # TODO
}


