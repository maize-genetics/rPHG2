# /// Internal helpers //////////////////////////////////////////////

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
# Generate a Colored and Formatted Message Based on Success or Failure
#
# This function creates a custom message incorporating user-defined
# actions, notes, and a success indicator. It uses color coding to
# distinguish between successful and warning messages. If the operation
# is not successful, it defaults the action to "skipping value". Notes
# are formatted in bold when provided.
#
# @param mn
# A metric ID
# @param action
# A string specifying the action being reported on.
# If `success` is FALSE, this is automatically set to "skipping value".
# Default is NULL.
# @param note
# An optional string providing additional details about
# the action. This note is displayed in bold within the message if
# provided. Default is an empty string.
# @param success
# A logical indicating whether the action was
# successful (`TRUE`) or not (`FALSE`). This affects the message's
# color and symbol. Default is `TRUE`.
#
# @return
# A string containing the formatted message with a newline
# character appended. The message includes a symbol, the action,
# possible note, and is colored based on the success parameter.
msgTemp <- function(mn, action = NULL, note = "", success = TRUE) {
    # Pre define 'cli' symbols
    msgInfo <- cli::symbol$tick
    msgWarn <- cli::symbol$warning

    # Choose the color function based on success
    colorFunc <- if (success) cli::col_green else cli::col_yellow

    # Choose the symbol based on success
    symbol <- if (success) msgInfo else msgWarn

    # Default action if not successful
    if (!success) {
        action <- "skipping value"
    }

    # Construct the base message
    message <- paste0(" ", symbol, " ", action, ": ", basename(mn))
    message <- colorFunc(message)  # Apply the color function

    # Append note if provided
    if (note != "") {
        noteFormatted <- cli::style_bold(paste0(" (", note, ")"))
        message <- paste0(message, colorFunc(noteFormatted))
    }

    # Append newline character
    paste0(message, "\n")
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

colCustGrey <- function(text, shade = 50) {
    greyColor <- paste0("grey", shade)
    style <- cli::make_ansi_style(greyColor)
    style(text)
}

## ----
# Helper function to display dimensions of metrics
#
# @param metrics A list of 'data.frame' objects
# @param label Header for display
# @param maxPrint Max number of table to display
displayMetrics <- function(metrics, label, maxPrint) {
    pointerSymbol <- cli::col_green(cli::symbol$pointer)
    infoSymbol    <- cli::symbol$info

    if (length(metrics) > maxPrint) {
        shownMetrics <- metrics[seq_len(maxPrint)]
        extraCount <- length(metrics) - maxPrint
    } else {
        shownMetrics <- metrics
        extraCount <- 0
    }

    cat(paste0("-- ", label, ":\n"))
    lapply(seq_along(shownMetrics), function(x) {
        obj  <- dim(shownMetrics[[x]])
        nObj <- names(shownMetrics[x])
        msg <- paste0(
            " ", pointerSymbol, " ", nObj,
            " (", colCustGrey(paste(obj, collapse = " ")), ")", "\n"
        )
        cat(msg)
    })

    if (extraCount > 0) {
        cat(
            colCustGrey(
                paste0(
                    " # ", infoSymbol, " ",
                    extraCount, " more table(s)", "\n"
                )
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
# @param mn A metric identifier (character)
# @param type Boiler plate message template
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

    # Response generation using the template function
    resp <- switch(
        EXPR = type,
        "success_01" = msgTemp(mn, success = TRUE, action = "reading data for"),
        "success_02" = msgTemp(mn, success = TRUE, action = "importing data for"),
        "warn_01"    = msgTemp(mn, success = FALSE, note = "not valid metric data"),
        "warn_02"    = msgTemp(mn, success = FALSE, note = "ID already found in object"),
        "warn_03"    = msgTemp(mn, success = FALSE, note = "ID not found in object"),
        "warn_04"    = msgTemp(mn, success = FALSE, note = "ID duplicated")
    )

    return(resp)
}


## ----
# Parse Left and Right Hand Sides of a Formula
#
# This function parses a formula object and extracts the left-hand side (LHS)
# and right-hand side (RHS) variables. It also checks if the keyword "ALL" is
# used with other variables on either side of the formula and raises an error
# if so.
#
# @param formula A formula object to be parsed.
#
# @return A list with two elements:
# \describe{
#   \item{lhs}{A character vector of variables on the left-hand side of the formula.}
#   \item{rhs}{A character vector of variables on the right-hand side of the formula.}
# }
#
# @examples
# \dontrun{
# formula <- y1 + y2 ~ x1 + x2 + x3
# parsedVars <- parseFormula(formula)
# print(parsedVars)
#
# # This will raise an error
# try(parseFormula(ALL + y1 ~ x1 + ALL), silent = TRUE)
# }
parseFormula <- function(formula) {
    # Extract the terms object from the formula
    termsObj <- stats::terms(formula)

    # Extract the response variables (LHS)
    lhsVars <- all.vars(formula[[2]])

    # Extract the predictor variables (RHS)
    rhsVars <- all.vars(formula[[3]])

    keywords <- c("ALL", "CORE")

    errorMsg <- "The keywords 'ALL' and 'CORE' cannot be used with other variables"

    # Check if keywords are used with other variables in LHS
    if (any(keywords %in% lhsVars) && length(lhsVars) > 1) {
        rlang::abort(errorMsg)
    }

    # Check if keywords are used with other variables in RHS
    if (any(keywords %in% rhsVars) && length(rhsVars) > 1) {
        rlang::abort(errorMsg)
    }

    # Return a list with LHS and RHS variables
    return(list(lhs = lhsVars, rhs = rhsVars))
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
                utils::read.table(
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


