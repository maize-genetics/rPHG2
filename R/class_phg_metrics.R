## ----
#' @title A PHGMetrics Class
#'
#' @description
#' A \code{PHGMetrics} class is a general class for storing metrics data
#' generated from PHGv2 operations.
#'
#' @slot anchorFiles
#' AnchorWave alignment metrics (\code{.anchorspro})
#' @slot gvcfMetrics
#' gVCF metrics
#' @slot metadata
#' A \code{data.frame} object of key-value data for files
#'
#' @name PHGMetrics-class
#' @rdname PHGMetrics-class
#' @exportClass PHGMetrics
setClass(
    Class = "PHGMetrics",
    representation = representation(
        anchorFiles = "list",
        gvcfMetrics = "list",
        metadata    = "tbl_df"
    ),
    prototype = list(
        anchorFiles = list(),
        gvcfMetrics = list(),
        metadata    = tibble::tibble()
    )
)


## ----
#' @title PHGMetrics object constructor
#'
#' @description
#' \code{PHGMetrics} is the primary container for housing PHGv2 metrics data
#'
#' @param dir A collection of directories
#' @param metadata key-value metadata for files
#'
#' @return A \code{PHGMetrics} object.
#'
#' @export
PHGMetrics <- function(dir = NULL, metadata = NULL) {
    dirFilt <- dir[dir.exists(dir)]

    if (length(dirFilt) == 0) {
        stop("No valid directories given")
    }

    posFiles <- list.files(
        path       = dirFilt,
        pattern    = "\\.tsv$|\\.anchorspro$",
        full.names = TRUE,
        recursive  = TRUE
    )
    posFiles <- normalizePath(posFiles)

    gvcfMet <- posFiles[endsWith(posFiles, "tsv")]
    anchMet <- posFiles[endsWith(posFiles, "anchorspro")]

    if (length(gvcfMet) != 0) {
        gvcfMet <- gvcfMet[isValidGvcf(gvcfMet)]
    }

    if (length(anchMet) != 0) {
        anchMet <- anchMet[isValidAnchor(anchMet)]
    }

    if (length(gvcfMet) == 0 && length(anchMet) == 0) {
        stop("No valid gVCF or anchor files detected")
    }

    if (length(gvcfMet) != 0) {
        gvcfDfs <- lapply(
            X = gvcfMet,
            FUN = function(x) {
                tmp <- tibble::as_tibble(
                    read.table(
                        file         = x,
                        header       = TRUE,
                        sep          = "\t",
                        comment.char = "#"
                    )
                )
                colnames(tmp) <- camelToSnake(colnames(tmp))
                return(tmp)
            }
        )
    } else {
        gvcfDfs <- list()
    }

    if (length(anchMet) != 0) {
        anchDfs <- lapply(
            X = anchMet,
            FUN = function(x) {
                tmp <- tibble::as_tibble(
                    read.table(
                        file         = x,
                        header       = TRUE,
                        sep          = "\t",
                        comment.char = "#"
                    )
                )
                colnames(tmp) <- camelToSnake(colnames(tmp))
                return(tmp)
            }
        )
    } else {
        anchDfs <- list()
    }

    if (is.null(dir)) {
        methods::new("PHGMetrics")
    } else {
        methods::new(
            Class       = "PHGMetrics",
            anchorFiles = anchDfs,
            gvcfMetrics = gvcfDfs,
            metadata    = if (is.null(metadata)) tibble::tibble()
        )
    }
}


