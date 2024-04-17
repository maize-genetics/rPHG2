## ----
# Filter PHGDataSet object based on sample IDs
#
# @param object a PHGDataSet object
# @param sampleIds a character vector of sample IDs to filter on
filterSamplesFromPhgDataSet <- function(object, sampleIds) {
    # Get relevant data from original PHGDataSet
    hapIds       <- readHapIds(object)
    hapIdMeta    <- readHapIdMetaData(object)
    hapIdPosMeta <- readHapIdPosMetaData(object)
    samples      <- readSamples(object)

    # Filter for samples in ID vector
    fSamples <- samples[samples %in% sampleIds]
    if (length(fSamples) == 0) {
        stop("No samples identified with given query")
    }

    # Generate a regex pattern for row subsetting
    fHapIds <- hapIds[gsub("_G1", "", rownames(hapIds)) %in% fSamples, , drop = FALSE]

    return(
        PHGDataSet(
            samples      = fSamples,
            hapIds       = fHapIds,
            refRanges    = readRefRanges(object), # no need to filter this
            hapIdMeta    = hapIdMeta[hapIdMeta$hap_id %in% fHapIds, ],
            hapIdMetaPos = hapIdPosMeta[hapIdPosMeta$hap_id %in% fHapIds, ]
        )
    )
}


