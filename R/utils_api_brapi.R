## ----
# Check if BrAPI `serverinfo` endpoint exists
#
# @description
# Checks if BrAPI compliant `serverinfo` endpoint can be reached. This
# presumption will imply that we can at least connect to this "mandatory"
# endpoint for the PHG Ktor server.
#
# @param url
# Host URL for PHG server
# @param endpoint
# What endpoint to append to URL
#
# @return
# logical
brapiEndpointExists <- function(url, endpoint = BRAPI_ENDPOINTS$SERVER_INFO) {
    # Check specified BrAPI endpoint
    status <- tryCatch(
        expr  = httr::GET(file.path(url, endpoint))$status,
        error = function(cond) {
            return(NA)
        }
    )

    # NOTE: test currently negates `httResp` check for all status codes. Will
    #       keep in codebase for possible future debugging tests
    ifelse(
        test = !is.na(status) && status >= 200 && status <= 299,
        yes  = return(TRUE),
        no   = return(FALSE)
    )
}


## ----
# Get metadata field from a given endpoint
#
# @description
# Returns metadata field from a BrAPI endpoint as a list
#
# @param object
# @param endpoint
#
# @return
# list of nested metadata fields (e.g. pagination)
brapiMetadata <- function(object, endpoint) {
    endPoint <- file.path(brapiURL(object), endpoint)
    endPoint <- parseJSON(endPoint)

    return(endPoint[["metadata"]])
}


## ----
# Get HTTP response status codes from PHG server
#
# @description
# By default, this will ping the `serverinfo` BrAPI endpoint on the server.
# NOTE: `url` needs `brapi/v2` or `brapi/v1` suffix.
#
# @param url
# Host URL for PHG server
# @param endpoint
# What endpoint to append to URL? Can be `""` for non BrAPI tests.
#
# @return
# list containing status value and CLI formatted message statement
httpResp <- function(url, endpoint = BRAPI_ENDPOINTS$SERVER_INFO) {

    status <- httr::GET(file.path(url, endpoint))$status

    statusMsg <- switch(
        EXPR = floor(status / 100),
        `1`  = cli::col_yellow("Information"),
        `2`  = cli::col_green("OK"),
        `3`  = cli::col_blue("Redirection"),
        `4`  = cli::col_red("Client Error"),
        `5`  = cli::col_red("Server Error")
    )

    return(list(status = status, msg = cli::style_bold(statusMsg)))
}


## ----
# JSON to tibble converter
#
# @description
# Converts a requested JSON object to a tibble-based data.frame object.
#
# @param object
# A PHGServerCon object.
# @param endpoint
# A specified endpoint to request.
# @param returnCall
# What JSON section should be returned? Defaults to `data`.
#
# @return
# A tibble object.
jsonToTibble <- function(object, endpoint, returnCall = "data") {
    endPoint <- file.path(brapiURL(object), endpoint)
    endPoint <- parseJSON(endPoint)

    # This will most likely always be "data" in BrAPI spec...
    return(tibble::as_tibble(endPoint[["result"]][[returnCall]]))
}


## ----
# Parse JSON response to native R object
#
# @param url
# A BrAPI URL endpoint.
# @param verbose
# Do you want messages shown?
#
# @return
# list containing parse JSON fields
parseJSON <- function(url, verbose = FALSE) {
    res <- tryCatch(
        expr = {
            if (verbose) message("Attempting to read endpoint...")
            x <- httr::GET(url)
            x <- httr::content(x, as = "text", encoding = "UTF-8")
            x <- jsonlite::fromJSON(x)
            return(x)
        },
        error = function(cond) {
            return(NA)
        }
    )

    if (is.na(res)) {
        rlang::abort("Could not resolve BrAPI endpoint")
    }

    return(res)
}


