## ----
# Get ref ranges from JVM graph object
#
# @param jvmGraph A JVM graph object
refRangesFromJvmGraph <- function(jvmGraph) {
    interface <- createRMethodInterface()

    jrr <- interface$getRefRangesFromGraph(jvmGraph)

    rrr <- kotlinListToRDataFrame(jrr)
    grr <- GenomicRanges::makeGRangesFromDataFrame(rrr)
    grr$rr_id <- rrr$rr_id

    return(grr)
}


