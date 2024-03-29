# rPHG2 setup and initialization for local and GitHub actions
#
# This script will perform the following actions:
#   1. Download and decompress the latest version of PHGv2 JARs
#   2. Initialize the JVM and add PHGv2 libraries to class path


# --- Functions -----------------------------------------------------

## ----
# Get latest PHGv2 release URL
#
# @param repo A GitHub repository
#
# @export A URL string of type `character`
getLatestReleaseUrl <- function(repo) {
    apiUrl <- sprintf("https://api.github.com/repos/%s/releases/latest", repo)

    response <- httr::GET(apiUrl)

    # Check if the request was successful
    if (httr::http_status(response)$category != "Success") {
        stop(
            sprintf(
                "Failed to fetch the latest release info for '%s'. HTTP status code: %s",
                repo,
                httr::http_status(response)$reason
            )
        )
    }

    responseContent <- httr::content(response, "text")
    parsed <- jsonlite::fromJSON(responseContent)

    # Check if the asset URL is available
    if (length(parsed$assets) == 0 || is.null(parsed$assets$browser_download_url)) {
        stop(sprintf("No assets found in the latest release for '%s'.", repo))
    }

    assetUrl <- parsed$assets$browser_download_url

    return(assetUrl)
}


## ----
# Download PHGv2 JARs
#
# @param dir
# @param repo
downloadJavaLibraries <- function(dir, repo = "maize-genetics/phg_v2") {

    # Attempt to get the latest release URL with error handling
    libraryUrl <- tryCatch({
        getLatestReleaseUrl(repo)
    }, error = function(e) {
        stop(sprintf("Error fetching latest release URL: %s", e$message))
    })

    destFile <- file.path(dir, "phg_java_libs.tar.gz")

    # Attempt to download the file with error handling
    tryCatch({
        utils::download.file(libraryUrl, destFile, mode = "wb")
    }, error = function(e) {
        stop(sprintf("Error downloading the Java library from '%s': %s", libraryUrl, e$message))
    })

    # Attempt to decompress the file with error handling
    tryCatch({
        utils::untar(destFile, exdir = dir)
    }, error = function(e) {
        stop(sprintf("Error decompressing the Java library archive: %s", e$message))
    })
}



# --- "Main" entry point --------------------------------------------

## Set up temporary directories ----
phgLibDir <- tempdir()
phgLibPath <- file.path(phgLibDir, "phg", "lib")


## Download and decompress ----
downloadJavaLibraries(phgLibDir)


## Initialize JVM and add PHGv2 JARs to classpath ----
rPHG2::initPhg(phgLibPath)


