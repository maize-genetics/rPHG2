## ----
# Initialize JVM and add class path (for R&D purposes only)
#
# @param phgPath path to PHGv2 lib folder
initPhg <- function(phgPath) {
    rJava::.jinit()
    rJava::.jaddClassPath(dir(phgPath, full.names = TRUE))
    rJava::.jclassPath()
}


## ----
# Convert hash map to list object in R
#
# j A Java HashMap object
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
# Constructor for instantiating a JVM HaplotypeGraph object
#
# l A list of hVCF files
rjGraphConstructor <- function(l) {
    hvcfJList <- rJava::.jnew("java.util.ArrayList")
    lapply(l, function(i) hvcfJList$add(i))
    hvcfJList <- rJava::.jcast(hvcfJList, "java/util/List")

    jvmGraph <- .jnew(hgSrc, hvcfJList)

    return(jvmGraph)
}


