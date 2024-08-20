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
#' A list of \href{https://github.com/maize-genetics/phg_v2/blob/main/docs/hvcf_specifications.md}{hVCF}
#' files as a \code{character} vector
#'
#' @export
PHGLocalCon <- function(hVcfFiles) {
    # This is probably overkill right now, but will revisit once the TileDB
    # C API is better integrated in PHGv2...
    methods::new(
        Class     = "PHGLocalCon",
        phgType   = "local",
        host      = NA_character_, # making this NA for now
        hVcfFiles = normalizePath(hVcfFiles)
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


