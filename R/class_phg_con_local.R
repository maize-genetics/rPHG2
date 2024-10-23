## ----
#' @title A PHGLocalCon Class
#'
#' @description
#' A \code{PHGLocalCon} class defines a \code{rPHG} class for storing
#' local config file data.
#'
#' @slot hVcfFiles A list of hVCF files
#'
#' @name PHGLocalCon-class
#' @rdname PHGLocalCon-class
#' @exportClass PHGLocalCon
setClass(
    Class    = "PHGLocalCon",
    contains = "PHGCon",
    representation = representation(
        hVcfFiles = "character"
    ),
    prototype = prototype(
        hVcfFiles = NA_character_
    )
)


## ----
#' @title PHGLocalCon validation
#'
#' @name PHGLocalCon-validity
#'
#' @description
#' Checks for correct data entry into \code{PHGLocalCon} class
#'
#' @param object A \code{\linkS4class{PHGLocalCon}} object
setValidity("PHGLocalCon", function(object) {
    errors <- character()

    if (!dir.exists(host(object)) && !is.na(host(object))) {
        errors <- c("TileDB URI path does not exist", errors)
    }

    if (length(errors) == 0) {
        return(TRUE)
    } else {
        return(errors)
    }
})


## ----
#' @title Helper function to construct a \code{PHGLocalCon} object
#'
#' @description
#' Creates a \code{\linkS4class{PHGLocalCon}} object to be used to read PHG
#' DB data for a given set of PHG-related methods.
#'
#' @param hVcfFiles
#' A path to a directory or file containing valid \href{https://github.com/maize-genetics/phg_v2/blob/main/docs/hvcf_specifications.md}{hVCF}
#' files (ending in either \code{.h.vcf} or \code{.h.vcf.gz}) as a
#' \code{character} vector
#'
#' @export
PHGLocalCon <- function(hVcfFiles) {
    # TODO - This is probably overkill right now, but will revisit once the TileDB
    #        C API is better integrated in PHGv2...

    # Initial type checking
    if (!is.vector(hVcfFiles) || !is.character(hVcfFiles)) {
        rlang::abort("Input is not a valid 'character' vector")
    }

    # Check if directory or files exist
    procFiles <- NULL
    hvcfExtPattern <- "\\.h\\.vcf(\\.gz)?$"
    if (any(dir.exists(hVcfFiles))) {
        # Get all files ending with .h.vcf or .h.vcf.gz in the directory
        procFiles <- list.files(
            path       = hVcfFiles,
            pattern    = hvcfExtPattern,
            full.names = TRUE
        )

        if (length(procFiles) == 0) {
            rlang::abort("No files ending with .h.vcf or .h.vcf.gz found in provided directory")
        }
    } else if (any(file.exists(hVcfFiles))) {
        if (any(grepl(hvcfExtPattern, hVcfFiles))) {
            procFiles <- hVcfFiles[grepl(hvcfExtPattern, hVcfFiles)]
        }
    } else {
        rlang::abort("The input path is neither a valid directory nor a valid file")
    }

    methods::new(
        Class     = "PHGLocalCon",
        phgType   = "local",
        host      = NA_character_, # making this NA for now
        hVcfFiles = normalizePath(procFiles)
    )
}



# /// Methods (show) ////////////////////////////////////////////////

## ----
#' @title Show methods for PHGLocalCon objects
#'
#' @description
#' Prints out information regarding properties from the \code{PHGLocalCon}
#' class to the console
#'
#' @param object A \code{\linkS4class{PHGLocalCon}} object
#'
#' @docType methods
#' @rdname PHGLocalCon-class
#' @aliases show,PHGLocalCon-method
setMethod(
    f = "show",
    signature = "PHGLocalCon",
    definition = function(object) {
        pointerSymbol <- cli::col_green(cli::symbol$pointer)
        present <- cli::col_green(cli::symbol$square_small_filled)
        absent  <- cli::col_grey(cli::symbol$square_small)

        #' TODO - implement with direct calls to TileDB
        dbUriStatus <- ifelse(is.na(object@host), absent, present)

        if (any(is.na(object@hVcfFiles))) {
            hVcfStatus <- absent
        } else {
            hVcfStatus <- present
        }

        msg <- c(
            paste0("A ", cli::style_bold("PHGLocalCon"), " connection object"),
            paste0(" ", pointerSymbol, " DB URI.......: ", dbUriStatus),
            paste0(" ", pointerSymbol, " hVCF Files...: ", hVcfStatus)
        )

        cat(msg, sep = "\n")
    }
)



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @rdname hVcfFiles
#' @export
setMethod(
    f = "hVcfFiles",
    signature = signature(object = "PHGLocalCon"),
    definition = function(object) {
        return(object@hVcfFiles)
    }
)


