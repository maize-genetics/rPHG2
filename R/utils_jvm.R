## ----
# Create connection to RMethods class in PHGv2
createRMethodInterface <- function() {
    jrc <- PHG_JVM$R_METHODS
    interface <- rJava::.jnew(jrc)

    return(interface)
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
# Convert hash map to list object in R
#
# @param j A Java HashMap object
hashMapToList <- function(j) {
    jvmHashMap <- list()
    entry_set  <- j$entrySet()
    iterator   <- rJava::.jcall(entry_set, "Ljava/util/Iterator;", "iterator")

    while (.jcall(iterator, "Z", "hasNext")) {
        entry <- rJava::.jcall(iterator, "Ljava/lang/Object;", "next")
        key   <- rJava::.jcall(entry, "Ljava/lang/Object;", "getKey")
        value <- rJava::.jcall(entry, "Ljava/lang/Object;", "getValue")
        jvmHashMap[[key$toString()]] <- value
    }

    return(jvmHashMap)
}


## ----
# Initialize JVM and add class path (for R&D purposes only)
#
# @param phgPath path to PHGv2 lib folder
# @param verbose Display all JARs added classpath? Defaults to FALSE.
initPhg <- function(phgPath, verbose = FALSE) {
    rJava::.jinit()
    rJava::.jaddClassPath(dir(phgPath, full.names = TRUE))

    message("PHG JARs added to class path")

    if (verbose) rJava::.jclassPath()
}


## ----
# Convert a PHG/Kotlin RList object into an R data frame
#
# @param kl A PHG/Kotlin RList object
kotlinListToRDataFrame <- function(kl) {
    if (!grepl("phgv2_r_list", kl$toString())) {
        stop("Object does not have an 'RList' signature")
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
kotlinMatToRMatrix <- function(kmat) {
    if (!grepl("MatrixWithNames", kmat$getClass()$toString())) {
        stop("Object does not have 'MatrixWithNames' signature")
    }

    rmat <- kmat$getMatrixData() |> rJava::.jevalArray(simplify = TRUE)
    colnames(rmat) <- kmat$getColNames()
    rownames(rmat) <- kmat$getRowNames()

    return(rmat)
}


