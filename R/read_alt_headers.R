## ----
# Get alt headers (hap ID metadata) from JVM graph object
#
# @param jvmGraph A JVM graph object
altHeadersFromJvmGraph <- function(jvmGraph) {
    interface <- createRMethodInterface()

    ahDf <- interface$getAltHeadersFromGraph(jvmGraph) |> kotlinListToRDataFrame()

    ahDf$positions <- ahDf$positions |>
        lapply(\(it) {
            GenomicRanges::makeGRangesFromDataFrame(
                df = kotlinListToRDataFrame(it),
                keep.extra.columns = TRUE,
                seqnames.field = "contig_start"
            )
        })

    return(ahDf)
}


