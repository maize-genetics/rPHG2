## ----
# Get samples from `HaplotypeGraph` objects
#
# @param phgObj A PHG `HaplotypeGraph` object
samplesFromGraphObj <- function(phgObj) {
    jArray <- rJava::.jevalArray(obj = phgObj$taxaInGraph()$toArray())

    taxa <- unlist(lapply(jArray, function(x) x$getName()))

    return(taxa)
}


## ----
# Get samples from local connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
samplesFromLocal <- function(conObj, conMethod) {
    dbConn <- rJava::.jnew(TASSEL_API$DB_LOADING_UTILS)$
        connection(
            configFilePath(conObj),
            FALSE
        )

    sqlQuery <- paste(
        c("SELECT line_name, name FROM genotypes"),
        c("JOIN paths on paths.genoid=genotypes.genoid"),
        c("JOIN methods on methods.method_id=paths.method_id"),
        sprintf("WHERE methods.name = '%s'", conMethod),
        collapse = " "
    )

    rs <- dbConn$createStatement()$executeQuery(sqlQuery)

    taxa <- c()
    i <- 1
    while (rs$`next`()) {
        taxa[i] <- rs$getString("line_name")
        i <- i + 1
    }

    rs$close()
    dbConn$close()

    return(taxa)
}


## ----
# Get samples from server connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
# @param conDemo Is this method of type 'DEMO'
samplesFromServer <- function(conObj, conMethod, conDemo) {
    finalUrl <- file.path(
        brapiURL(conObj),
        BRAPI_ENDPOINTS$VARIANT_TABLES,
        conMethod,
        BRAPI_ENDPOINTS$SAMPLES
    )

    taxaDf <- parseJSON(finalUrl)$result$data

    if (conDemo) {
        return(taxaDf$sampleName[seq_len(BRAPI_PARAMS$DEMO_N_SAMPLES)])
    } else {
        return(taxaDf$sampleName)
    }
}

samplesFromV2Server <- function(conObj) {
    finalUrl <- file.path(
        brapiURL(conObj),
        BRAPI_ENDPOINTS$SAMPLES
    )

    taxaDf <- parseJSON(finalUrl)$result$data

    return(taxaDf$sampleName)
}


