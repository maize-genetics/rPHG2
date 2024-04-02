## ----
# Get positional data from alt headers (hap ID metadata) from JVM graph object
#
# @param jvmGraph A JVM graph object
hapIdPosMetaDataFromJvmGraph <- function(jvmGraph) {
    interface <- createRMethodInterface()

    ahPosDf <- interface$getAltHeaderPositionsFromGraph(jvmGraph) |> kotlinListToRDataFrame()

    return(ahPosDf)
}



