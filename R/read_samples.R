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


