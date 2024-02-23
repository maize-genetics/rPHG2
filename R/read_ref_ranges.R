## ----
# Get ref ranges from JVM graph object
#
# @param jvmGraph A JVM graph object
refRangesFromJvmGraph <- function(jvmGraph) {
    interface <- createRMethodInterface()

    jrr <- interface$getRefRangesFromGraph(jvmGraph)

    rrr <- jrr |>
        kotlinListToRDataFrame() |>
        GenomicRanges::makeGRangesFromDataFrame()

    return(rrr)
}


