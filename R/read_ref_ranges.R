## ----
# Get ref ranges from JVM graph object
#
# @param jvmGraph A JVM graph object
rangesFromGraph <- function(jvmGraph) {
    rangeArray <- jvmGraph$ranges()
    rangeJList <- rJava::.jnew("java.util.ArrayList")
    lapply(rangeArray, function(i) rangeJList$add(i))
    rangeJList <- rJava::.jcast(rangeJList, "java/util/List")

    rangeJList$toArray() |>
        rJava::.jevalArray() |>
        lapply(\(it) {
            data.frame(
                "seqnames" = it$getContig(),
                "start"    = it$getStart(),
                "stop"     = it$getEnd()
            )
        }) |>
        do.call("rbind", args = _) |>
        GenomicRanges::makeGRangesFromDataFrame()
}


