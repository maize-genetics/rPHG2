## ----
# Get haplotype IDs from JVM graph object
#
# @param jvmGraph A JVM graph object
hapIdsFromGraph <- function(jvmGraph) {
    rangeArray <- jvmGraph$ranges()
    rangeJList <- rJava::.jnew("java.util.ArrayList")
    lapply(rangeArray, function(i) rangeJList$add(i))
    rangeJList <- rJava::.jcast(rangeJList, "java/util/List")

    ids <- list()
    i <- 1
    for (r in rangeJList$toArray() |> rJava::.jevalArray()) {
        ids[[i]] <- jvmGraph$sampleGameteToHaplotypeId(r) |>
            hashMapToList() |>
            lapply(\(it) it$toString())
        i <- i + 1
    }

    m <- matrix(
        data  = ids |> unlist(),
        ncol  = length(ids[[1]]),
        byrow = TRUE
    )

    colnames(m) <- names(ids[[1]])

    return(m |> t())
}


