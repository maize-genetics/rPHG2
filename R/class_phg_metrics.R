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
PHGMetrics <- function(paths = NULL, metadata = NULL) {
    # V01 - check if directories or files exist
    dirFilt <- paths[dir.exists(paths)]
    filFilt <- paths[file.exists(paths)]

    # V02 - if no files or directories exist: exception
    if (length(dirFilt) == 0 && length(filFilt) == 0) {
        stop("No valid paths given")
    }

    # V03 - recursively pull metric files from directories (if found)
    # NOTE - probably don't need a pattern but good for only keeping
    #        possibly valid files in memory footprint in case it's a
    #        massive directory
    if (length(dirFilt) != 0) {
        metFilesFromDir <- list.files(
            path       = dirFilt,
            pattern    = "\\.tsv$|\\.anchorspro$",
            full.names = TRUE,
            recursive  = TRUE
        )
        metFilesFromDir <- normalizePath(metFilesFromDir)
    }

    # V04 - combine all files as one collection
    posFiles <- c(metFilesFromDir, filFilt)

    # V05 - split out files based on type
    gvcfMet <- posFiles[endsWith(posFiles, "tsv")]
    anchMet <- posFiles[endsWith(posFiles, "anchorspro")]

    # V06 - if .tsv or .anchorspro files are identified: check contents for
    #       further validity
    if (length(gvcfMet) != 0) {
        gvcfMet <- gvcfMet[isValidGvcf(gvcfMet)]
    }

    if (length(anchMet) != 0) {
        anchMet <- anchMet[isValidAnchor(anchMet)]
    }

    # V07 - if no .tsv or .anchorspro files are identified: exception
    if (length(gvcfMet) == 0 && length(anchMet) == 0) {
        stop("No valid gVCF or anchor files detected from paths")
    }

    # If files are fully vetted: read into memory and add to list
    if (length(gvcfMet) != 0) {
        gvcfDfs <- readMetricFiles(gvcfMet)
    } else {
        gvcfDfs <- list()
    }

    if (length(anchMet) != 0) {
        anchDfs <- readMetricFiles(anchMet)
    } else {
        anchDfs <- list()
    }

    if (is.null(metadata)) {
        metadata <- tibble::tibble(
            file = basename(c(gvcfMet, anchMet)),
            type = c(rep("gvcf", length(gvcfMet)), rep("anchor", length(anchMet))),
            id   = tools::file_path_sans_ext(basename(c(gvcfMet, anchMet)))
        )
    }

    if (is.null(dir)) {
        methods::new("PHGMetrics")
    } else {
        methods::new(
            Class       = "PHGMetrics",
            anchorFiles = anchDfs,
            gvcfMetrics = gvcfDfs,
            metadata    = metadata
        )
    }
}



# /// Methods (general) /////////////////////////////////////////////

#' @export
.DollarNames.PHGMetrics <- function(x, pattern = "") {
    grep(pattern, x@metadata$id, value = TRUE)
}

#' @export
setMethod("$", "PHGMetrics", function(x, name) {
    slot(x, "metadata")[["id"]][x@metadata[["id"]] == name]
})

#' @export
`%T%` <- function(lhs, rhs) {
    nRhs <- eval(rhs)
    names(nRhs) <- eval(lhs)
    return(nRhs)

}

#' @export
renameMetrics <- function(x, ...) {
    newNames <- c(...)
    oldNames <- names(newNames)

    resultGvcfIds <- newNames[names(x@gvcfMetrics)]
    resultAnchIds <- newNames[names(x@anchorFiles)]
    resultGvcfIds[is.na(resultGvcfIds)] <- names(x@gvcfMetrics)[is.na(resultGvcfIds)]
    resultAnchIds[is.na(resultAnchIds)] <- names(x@anchorFiles)[is.na(resultAnchIds)]

    names(x@gvcfMetrics) <- resultGvcfIds
    names(x@anchorFiles) <- resultAnchIds

    x@metadata[x@metadata$id %in% oldNames, "id"] <- newNames

    return(x)
}







