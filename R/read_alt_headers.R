## ----
# Get alt headers (hap ID metadata) from JVM graph object
#
# @param jvmGraph A JVM graph object
altHeadersFromJvmGraph <- function(jvmGraph) {
    interface <- rPHG2:::createRMethodInterface()

    ahDf <- interface$getAltHeadersFromGraph(jvmGraph) |>
        rPHG2:::kotlinListToRDataFrame()

    ahDf$positions <- ahDf$positions |>
        lapply(\(it) {
            GenomicRanges::makeGRangesFromDataFrame(
                df = rPHG2:::kotlinListToRDataFrame(it),
                keep.extra.columns = TRUE,
                seqnames.field = "contig_start"
            )
        })

    return(ahDf[order])
}

