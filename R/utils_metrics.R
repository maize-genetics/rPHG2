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
# Helper function to display dimensions of metrics
displayMetrics <- function(metrics, label, maxPrint) {
    cuGrey        <- "\033[38;5;246m"
    rsGrey        <- "\033[0m"
    pointerSymbol <- cli::col_green(cli::symbol$pointer)
    infoSymbol    <- cli::symbol$info

    if (length(metrics) > maxPrint) {
        shownMetrics <- metrics[seq_len(maxPrint)]
        extraCount <- length(metrics) - maxPrint
    } else {
        shownMetrics <- metrics
        extraCount <- 0
    }

    cat(paste0("* ", label, ":\n"))
    lapply(seq_along(shownMetrics), function(x) {
        obj  <- dim(shownMetrics[[x]])
        nObj <- names(shownMetrics[x])
        msg <- paste0(
            " ", pointerSymbol, " ", nObj,
            " (", cuGrey, paste(obj, collapse = " "), rsGrey, ")", "\n"
        )
        cat(msg)
    })

    if (extraCount > 0) {
        cat(
            paste0(
                cuGrey, " # ", infoSymbol, " ",
                extraCount, " more tables", rsGrey, "\n"
            )
        )
    }
}


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
# Deploy messages across functions
#
# @param mn
# A metric identifier (character)
# @param type
# Boiler plate message template
metMessenger <- function(
    mn,
    type = c(
        "success_01",
        "success_02",
        "warn_01",
        "warn_02",
        "warn_03",
        "warn_04"
    )
) {
    type <- match.arg(type)

    msgInfo <- cli::symbol$tick
    msgWarn <- cli::symbol$warning

    # Function to generate messages based on type
    msgTemp <- function(action, note = "", success = TRUE) {
        cf <- if (success) cli::col_green else cli::col_yellow
        symbol <- if (success) msgInfo else msgWarn
        message <- paste0(" ", symbol, " ", action, ": ", basename(mn))
        message <- cf(message)
        if (note != "") {
            message <- paste0(message, cli::style_bold(note))
        }
        paste0(message, "\n")
    }

    # Response generation using the template function
    resp <- switch(
        EXPR = type,
        "success_01" = msgTemp("reading data for"),
        "success_02" = msgTemp("importing data for"),
        "warn_01"    = msgTemp("skipping value", " (not valid metric data)", FALSE),
        "warn_02"    = msgTemp("skipping value", " (ID already found in object)", FALSE),
        "warn_03"    = msgTemp("skipping value", " (ID not found in object)", FALSE),
        "warn_04"    = msgTemp("skipping value", " (ID duplicated)", FALSE)
    )

    return(resp)
}


## ----
# Read metric files (assumes tab-delimited structure)
#
# @param metricFiles A list of files
readMetricFiles <- function(metricFiles) {
    inMemDfs <- lapply(
        X = metricFiles,
        FUN = function(x) {
            cat(metMessenger(basename(x), "success_01"))
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





