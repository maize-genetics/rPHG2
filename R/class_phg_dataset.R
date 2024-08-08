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
        pointerSymbol <- cli::col_green(cli::symbol$pointer)

        msg <- c(
            paste0("A ", cli::style_bold("PHGDataSet"), " object"),
            paste0(" ", pointerSymbol, " # of ref ranges....: ", cli::style_bold(ncol(readHapIds(object)))),
            paste0(" ", pointerSymbol, " # of samples.......: ", cli::style_bold(length(readSamples(object)))),
            paste0("---"),
            paste0(" ", pointerSymbol, " # of hap IDs.......: ", cli::style_bold(nrow(readHapIdMetaData(object)))),
            paste0(" ", pointerSymbol, " # of asm regions...: ", cli::style_bold(nrow(readHapIdPosMetaData(object))))
        )

        cat(msg, sep = "\n")
    }
)



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @param gRanges
#' A \code{GRanges} coordinate object
#'
#' @rdname filterRefRanges
#' @export
setMethod(
    f = "filterRefRanges",
    signature = signature(object = "PHGDataSet"),
    definition = function(object, gRanges) {
        filterRefRangesFromPhgDataSet(object, gRanges)
    }
)


## ----
#' @param sampleIds
#' A \code{character} vector of sample IDs
#'
#' @rdname filterSamples
#' @export
setMethod(
    f = "filterSamples",
    signature = signature(object = "PHGDataSet"),
    definition = function(object, sampleIds) {
        filterSamplesFromPhgDataSet(object, sampleIds)
    }
)


## ----
#' @param numberOfChromosomes
#' A \code{character} vector of sample IDs
#'
#' @rdname numberOfChromosomes
#' @export
setMethod(
    f = "numberOfChromosomes",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(length(unique(GenomeInfoDb::seqnames(readRefRanges(object)))))
    }
)


## ----
#' @param object
#' A \code{\linkS4class{PHGDataSet}} object
#' @param byRefRange
#' If \code{TRUE}, a \code{tibble} object will created where each row is a
#' count of unique haplotype IDs for every reference range. If \code{FALSE}, a
#' global count of all haplotype IDs will be returned from the dataset.
#'
#' @rdname numberOfHaplotypes
#' @export
setMethod(
    f = "numberOfHaplotypes",
    signature = signature(object = "PHGDataSet"),
    definition = function(object, byRefRange = FALSE) {
        if (byRefRange) {
            hapIds <- readHapIds(object)
            refRanges <- as.data.frame(readRefRanges(object))

            uniqHaps <- data.frame(
                rr_id   = colnames(hapIds),
                n_haplo = apply(hapIds, 2, function(it) length(unique(stats::na.omit(it))))
            )

            return(tibble::as_tibble(
                merge(uniqHaps, refRanges, by = "rr_id")
            ))
        } else {
            return(nrow(readHapIdMetaData(object)))
        }
    }
)


## ----
#' @param numberOfRefRanges
#' A \code{character} vector of sample IDs
#'
#' @rdname numberOfRefRanges
#' @export
setMethod(
    f = "numberOfRefRanges",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(length(readRefRanges(object)))
    }
)


## ----
#' @param numberOfSamples
#' A \code{character} vector of sample IDs
#'
#' @rdname numberOfSamples
#' @export
setMethod(
    f = "numberOfSamples",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(length(readSamples(object)))
    }
)


## ----
#' @param gr
#' A \code{GenomicRanges} object for subsetting based on chromosome ID and
#' start/stop positions (bp). If \code{NULL}, all haplotype counts will be
#' plotted similar to a Manhattan plot. Defaults to \code{NULL}.
#'
#' @rdname plotHaploCounts
#' @export
setMethod(
    f = "plotHaploCounts",
    signature = signature(object = "PHGDataSet"),
    definition = function(object, gr = NULL) {
        nHaplo <- numberOfHaplotypes(object, byRefRange = TRUE)
        p <- NULL
        if (is.null(gr)) {
            p <- ggplot2::ggplot(nHaplo) +
                ggplot2::aes(x = !!rlang::sym("start"), y = !!rlang::sym("n_haplo")) +
                ggplot2::geom_point() +
                ggplot2::scale_y_continuous(
                    breaks = seq_len(max(nHaplo$n_haplo)),
                    limits = c(1, max(nHaplo$n_haplo))
                ) +
                ggplot2::scale_x_continuous(
                    labels = scales::label_number(
                        scale_cut = scales::cut_short_scale()
                    )
                ) +
                ggplot2::xlab("Position (bp)") +
                ggplot2::ylab("Number of unique haplotypes") +
                ggplot2::facet_wrap(~ seqnames) +
                ggplot2::theme_bw()
        } else {
            refRanges <- readRefRanges(object)
            if (!is(gr, "GRanges")) {
                rlang::abort("'gr' object is not of type 'GRanges'")
            }

            gr$sub_id <- paste0("QR ", GenomeInfoDb::seqnames(gr), ":", IRanges::ranges(gr))

            # Find overlaps
            overlaps <- suppressWarnings(GenomicRanges::findOverlaps(refRanges, gr))
            if (length(overlaps) == 0) {
                rlang::abort("No reference ranges identified with given query")
            }

            # Filter based on overlaps
            filtGr <- refRanges[S4Vectors::queryHits(overlaps)]

            # Add sub_id metadata
            filtGr$sub_id <- gr$sub_id[S4Vectors::subjectHits(overlaps)]
            filtGrDf <- as.data.frame(filtGr)
            filtGrDf <- merge(x = filtGrDf, y = nHaplo)

            p <- ggplot2::ggplot(filtGrDf) +
                ggplot2::aes(x = !!rlang::sym("rr_id"), y = !!rlang::sym("n_haplo")) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::scale_y_continuous(breaks = seq(0, max(nHaplo$n_haplo), by = 1)) +
                ggplot2::xlab("Reference range ID") +
                ggplot2::ylab("Number of unique haplotypes") +
                ggplot2::facet_grid(~ sub_id, scales = "free_x", space = "free") +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 90,
                        vjust = 0.5,
                        hjust = 1
                    )
                )
        }

        return(p)
    }
)


## ----
#' @param drop
#' Do you want unused unique count bins to be plotted? Defaults to \code{TRUE}.
#'
#' @rdname plotHaploDist
#' @export
setMethod(
    f = "plotHaploDist",
    signature = signature(object = "PHGDataSet"),
    definition = function(object, drop = FALSE) {
        nHaplo <- numberOfHaplotypes(object, byRefRange = TRUE)
        nHaplo$n_haplo <- factor(
            x = nHaplo$n_haplo,
            levels = seq_len(numberOfSamples(object))
        )

        p <- ggplot2::ggplot(nHaplo) +
            ggplot2::aes(x = !!rlang::sym("n_haplo")) +
            ggplot2::geom_bar() +
            ggplot2::scale_x_discrete(drop = drop) +
            ggplot2::labs(x = "Number of unique haplotypes", y = "Count") +
            ggplot2::ggtitle("Haplotype count distributions") +
            ggplot2::theme_bw()

        return(p)
    }
)


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


