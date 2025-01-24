## ----
# Get alt headers (hap ID metadata) from JVM graph object
#
# @param jvmGraph A JVM graph object
phgDataSetFromJvmGraph <- function(jvmGraph, nThreads) {
    jobjRef <- javaRefObj(jvmGraph)

    samples          <- samplesFromJvmGraph(jobjRef)
    refRanges        <- refRangesFromJvmGraph(jobjRef)
    hapIds           <- hapIdsFromJvmGraph(jobjRef, nThreads)
    hapIdMetaData    <- hapIdMetaDataFromJvmGraph(jobjRef)
    hapIdPosMetaData <- hapIdPosMetaDataFromJvmGraph(jobjRef)

    return(
        PHGDataSet(
            samples      = samples,
            refRanges    = refRanges,
            hapIds       = hapIds,
            hapIdMeta    = hapIdMetaData,
            hapIdMetaPos = hapIdPosMetaData,
            dbUri        = host(jvmGraph)
        )
    )
}


