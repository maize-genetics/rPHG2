## ----
# Create connection to RMethods class in PHGv2
createRMethodInterface <- function() {
    jrc <- PHG_JVM$R_METHODS
    interface <- rJava::.jnew(jrc)

    return(interface)
}


## ----
getLatestPhgVersion <- function() {

    apiUrl <- "https://api.github.com/repos/maize-genetics/phg_v2/releases/latest"
    response <- httr::GET(apiUrl)

    # Check if the request was successful
    if (httr::http_status(response)$category != "Success") {
        rlang::abort(
            sprintf(
                "Failed to fetch the latest release info for '%s'. HTTP status code: %s",
                repo,
                httr::http_status(response)$reason
            )
        )
    }

    responseContent <- httr::content(response, "text")
    parsed <- jsonlite::fromJSON(responseContent)


    return(parsed$tag_name)
}


## ----
# Constructor for instantiating a PHGv2 HaplotypeGraph object
#
# @param l A list of hVCF files
hapGraphConstructor <- function(l) {
    hvcfJList <- rJava::.jnew(PHG_JVM$ARRAY_LIST)
    lapply(l, function(i) hvcfJList$add(i))
    hvcfJList <- rJava::.jcast(hvcfJList, PHG_JVM$LIST)

    jvmGraph <- rJava::.jnew(PHG_JVM$HAP_GRAPH, hvcfJList)

    return(jvmGraph)
}


## ----
# Check if the JVM is Initialized
#
# @description
# This function checks whether the Java Virtual Machine (JVM) has been
# initialized using the `rJava` package.
#
# The function attempts to retrieve the current Java version by calling a Java
# method. If the JVM is not initialized, an error is caught, and the function
# returns `FALSE`. If the JVM is initialized, it returns `TRUE`.
#
# @return
# A logical value indicating whether the JVM has been initialized. Returns
# `TRUE` if the JVM is initialized, otherwise `FALSE`.
isJvmInitialized <- function() {
    tryCatch({
        # Attempt to get the current Java version
        javaVersion <- rJava::.jcall(
            obj       = "java/lang/System",
            returnSig =  "S",
            method    = "getProperty", "java.version"
        )
        return(TRUE)
    }, error = function(e) {
        return(FALSE)
    })
}


## ----
# Convert a PHG/Kotlin RList object into an R data frame
#
# @param kl A PHG/Kotlin RList object
kotlinListToRDataFrame <- function(kl) {
    if (!grepl("phgv2_r_list", kl$toString())) {
        rlang::abort("Object does not have a 'RList' signature")
    }

    rdf <- kl$getMatrixData() |>
        as.list() |>
        lapply(rJava::.jevalArray, simplify = TRUE)

    names(rdf) <- kl$getColNames()

    return(tibble::as_tibble(rdf))
}


## -----
# Convert a PHG/Kotlin (Int|Dbl|String)Matrix into an R matrix
#
# @param kmat A PHG/Kotlin (Int|Dbl|String)Matrix object
kotlin2DArrayToRMatrix <- function(kmat) {
    if (!grepl("MatrixWithNames", kmat$getClass()$toString())) {
        rlang::abort("Object does not have a 'MatrixWithNames' signature")
    }

    rmat <- kmat$getMatrixData() |> rJava::.jevalArray(simplify = TRUE)
    colnames(rmat) <- kmat$getColNames()
    rownames(rmat) <- kmat$getRowNames()

    return(rmat)
}


