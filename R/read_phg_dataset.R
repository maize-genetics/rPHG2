## ----
# Get alt headers (hap ID metadata) from JVM graph object
#
# @param jvmGraph A JVM graph object
phgDataSetFromJvmGraph <- function(jvmGraph, nThreads) {
    samples          <- samplesFromJvmGraph(jvmGraph)
    refRanges        <- refRangesFromJvmGraph(jvmGraph)
    hapIds           <- hapIdsFromJvmGraph(jvmGraph, nThreads)
    hapIdMetaData    <- hapIdMetaDataFromJvmGraph(jvmGraph)
    hapIdPosMetaData <- hapIdPosMetaDataFromJvmGraph(jvmGraph)

    return(
        PHGDataSet(
            samples      = samples,
            refRanges    = refRanges,
            hapIds       = hapIds,
            hapIdMeta    = hapIdMetaData,
            hapIdMetaPos = hapIdPosMetaData
        )
    )
}


