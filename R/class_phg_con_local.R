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
#' @param dbUri
#' A path to a PHG configuration file
#'
#' @export
PHGLocalCon <- function(hVcfFiles = NULL, dbUri = NULL) {
    if (is.null(hVcfFiles) && is.null(dbUri)) {
        stop("Please specify either the hVcfFiles or the dbUri parameter")
    }

    # Need to do this since `ifelse` can only take in one element
    if (is.null(hVcfFiles)) {
        hVcfFiles <- NA_character_
    } else {
        hVcfFiles <- normalizePath(hVcfFiles)
    }

    methods::new(
        Class     = "PHGLocalCon",
        phgType   = "local",
        host      = ifelse(!is.null(dbUri), dbUri, NA_character_),
        hVcfFiles = hVcfFiles
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
        present <- cli::col_green(cli::symbol$radio_on)
        absent  <- cli::col_grey(cli::symbol$radio_off)

        dbUriStatus <- ifelse(is.na(object@host), absent, present)

        if (any(is.na(object@hVcfFiles))) {
            hVcfStatus <- absent
        } else {
            hVcfStatus <- present
        }

        msg <- c(
            paste0("A ", cli::style_bold("PHGLocalCon"), " connection object"),
            paste0(" ", pointerSymbol, " DB URI....   : ", dbUriStatus),
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


