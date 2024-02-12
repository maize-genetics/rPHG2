## ----
# Get samples from server connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
# @param conDemo Is this method of type 'DEMO'
samplesFromServer <- function(conObj, conMethod, conDemo) {
    finalUrl <- file.path(
        brapiURL(conObj),
        BRAPI_ENDPOINTS$SAMPLES
    )

    taxaDf <- parseJSON(finalUrl)$result$data

    return(taxaDf$sampleName)
}


## ----
# Get samples from JVM graph object
#
# @param jvmGraph A JVM graph object
samplesFromGraph <- function(jvmGraph) {
    jvmGraph$samples()$toArray() |>
        rJava::.jevalArray() |>
        vapply(\(it) {
            it$toString()
        }, "character")
}


