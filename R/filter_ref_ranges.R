## ----
# Filter PHGDataSet object based on reference ranges
#
# @param object a PHGDataSet object
# @param gRanges a GRanges object for selected coordinates
filterRefRangesFromPhgDataSet <- function(object, gRanges) {
    # Get relevant data from original PHGDataSet
    hapIds       <- readHapIds(object)
    hapIdMeta    <- readHapIdMetaData(object)
    hapIdPosMeta <- readHapIdPosMetaData(object)
    refRanges    <- readRefRanges(object)

    # Filter by ref ranges using IRanges "any" range intersecting logic
    # NOTE: need to encapsulate in a suppressWarnings() function since
    #       the query might result in a length of 0 which will still return
    #       a new GRanges object but will complain
    fRefRanges <- suppressWarnings(
        IRanges::subsetByOverlaps(refRanges, gRanges)
    )
    if (length(fRefRanges) == 0) {
        rlang::abort("No reference ranges identified with given query")
    }

    # Filter hap ID matrix by ref range column
    fHapIds <- hapIds[, colnames(hapIds) %in% fRefRanges$rr_id, drop = FALSE]

    return(
        PHGDataSet(
            samples      = readSamples(object), # no need to filter this
            hapIds       = fHapIds,
            refRanges    = fRefRanges,
            hapIdMeta    = hapIdMeta[hapIdMeta$hap_id %in% fHapIds, ],
            hapIdMetaPos = hapIdPosMeta[hapIdPosMeta$hap_id %in% fHapIds, ],
            dbUri        = host(object)
        )
    )
}

