## ----
# Get alt headers (hap ID metadata) from JVM graph object
#
# @param jvmGraph A JVM graph object
hapIdMetaDataFromJvmGraph <- function(jvmGraph) {
    interface <- createRMethodInterface()

    ahDf <- interface$getAltHeadersFromGraph(jvmGraph) |> kotlinListToRDataFrame()

    return(ahDf)
}


