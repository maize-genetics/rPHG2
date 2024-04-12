## ----
#' @title A PHGDataSet Class
#'
#' @description
#' A \code{PHGDatSet} class is a general class for storing data found within
#' an hVCF file. The main components are:
#'
#'   \describe{
#'      \item{samples}{Sample IDs}
#'      \item{reference ranges}{start/stop ranges in reference genome}
#'      \item{haplotypes}{haplotype sequence IDs (MD5 hashes)}
#'      \item{metadata}{additional information related to haplotype IDs}
#'   }
#'
#' @slot samples
#' A character vector of sample identifiers.
#' @slot refRanges
#' A `GRanges` object from the GenomicRanges package representing reference
#' genomic ranges.
#' @slot hapIds
#' A matrix of haplotype identifiers.
#' @slot hapIdMeta
#' A `tibble` object containing metadata associated with haplotype identifiers.
#' @slot hapIdMetaPos
#' A `tibble` object containing positional metadata for haplotype identifiers.
#'
#'
#' @name PHGDataSet-class
#' @rdname PHGDataSet-class
#' @exportClass PHGDataSet
setClass(
    Class = "PHGDataSet",
    representation = representation(
        samples      = "character",
        refRanges    = "GRanges",
        hapIds       = "matrix",
        hapIdMeta    = "tbl_df",
        hapIdMetaPos = "tbl_df"
    ),
    prototype = list(
        samples      = character(),
        refRanges    = GenomicRanges::GRanges(),
        hapIds       = matrix(character()),
        hapIdMeta    = tibble::tibble(),
        hapIdMetaPos = tibble::tibble()
    )
)


## ----
#' @title PHGDataSet object constructor
#'
#' @description
#' \code{PHGDataSet} is the primary container for housing hVCF data.
#'
#' @param samples Sample IDs
#' @param refRanges What type of PHG connection is this?
#' @param hapIds Haplotype sequence IDs
#' @param hapIdMeta Metadata for haplotype IDs
#' @param hapIdMetaPos Positional metadata for haplotype IDs
#'
#' @return A \code{PHGDataSet} object.
#'
#' @export
PHGDataSet <- function(
        samples,
        hapIds,
        refRanges,
        hapIdMeta,
        hapIdMetaPos
) {
    methods::new(
        Class        = "PHGDataSet",
        samples      = samples,
        hapIds       = hapIds,
        refRanges    = refRanges,
        hapIdMeta    = hapIdMeta,
        hapIdMetaPos = hapIdMetaPos
    )
}



# /// Methods (show) ////////////////////////////////////////////////

## ----
#' @title Show methods for PHGDataSet objects
#'
#' @description
#' Prints out information regarding properties from the \code{PHGDataSet}
#' class to the console
#'
#' @param object A \code{\linkS4class{PHGDataSet}} object
#'
#' @docType methods
#' @rdname PHGDataSet-class
#' @aliases show,PHGDataSet-method
setMethod(
    f = "show",
    signature = "PHGDataSet",
    definition = function(object) {
        cat("A PHG data set!\n")
    }
)



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @rdname readSamples
#' @export
setMethod(
    f = "readSamples",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(object@samples)
    }
)


## ----
#' @rdname readRefRanges
#' @export
setMethod(
    f = "readRefRanges",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(object@refRanges)
    }
)


## ----
#' @rdname readHapIds
#' @export
setMethod(
    f = "readHapIds",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(object@hapIds)
    }
)


## ----
#' @rdname readHapIdMetaData
#' @export
setMethod(
    f = "readHapIdMetaData",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(object@hapIdMeta)
    }
)


## ----
#' @rdname readHapIdPosMetaData
#' @export
setMethod(
    f = "readHapIdPosMetaData",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(object@hapIdMetaPos)
    }
)


