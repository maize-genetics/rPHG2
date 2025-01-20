# Read sequence data via AGC from a PHGDataSet object
#
# @param pds
# A \code{PHGDataSet} object containing the pangenome data.
# @param rrId
# Reference range ID to query sequences. Default is \code{NULL}.
# @param hapId
# Haplotype ID to query sequences. Default is \code{NULL}.
# @param pad
# Number of base pairs to pad upstream and . Default is \code{0}.
#
# @return A BioString object containing the retrieved sequence(s).
readSequenceFromPds <- function(pds, rrId = NULL, hapId = NULL, pad = 0) {
    if(is.null(rrId) && is.null(hapId)) {
        rlang::abort("'rrId' and 'hapId' cannot both be null")
    }

    if(!is.null(rrId) && !is.null(hapId)) {
        rrId <- NULL
    }

    if (!is.null(rrId)) {
        hapMat <- readHapIds(pds)
        ids <- hapMat[, colnames(hapMat) == rrId]
    } else {
        ids <- hapId
    }

    result <- vapply(
        ids[!is.na(ids)],
        function(id) genHapIdAgcQuery(pds, h = id, pad = pad),
        FUN.VALUE = character(1)
    )

    agcQuery <- paste(result, collapse = " ")

    return(
        agcCore(
            pds |> host() |> file.path("assemblies.agc"),
            "getctg", agcQuery
        ) |> rawFastaToBioString()
    )
}


