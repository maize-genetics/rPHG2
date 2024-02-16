## ----
#' @title A PHGLocalCon Class
#'
#' @description
#' A \code{PHGLocalCon} class defines a \code{rPHG} class for storing
#' local config file data.
#'
#' @slot dbUri URI path to PHGv2 database
#' @slot hVcfFiles A list of hVCF files
#'
#' @name PHGLocalCon-class
#' @rdname PHGLocalCon-class
#' @exportClass PHGLocalCon
setClass(
    Class    = "PHGLocalCon",
    contains = "PHGCon",
    representation = representation(
        dbUri     = "character",
        hVcfFiles = "character"
    ),
    prototype = prototype(
        dbUri     = NA_character_,
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

    if (!is.list(object@hVcfFiles)) {
        errors <- c("hVcfFiles are not of type list", errors)
    }

    if (!dir.exists(object@dbUri)) {
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
#' @param dbUri A path to a PHG configuration file
#'
#' @export
PHGLocalCon <- function(dbUri = NULL, hVcfFiles = NULL) {
    methods::new(
        Class     = "PHGLocalCon",
        phgType   = "local",
        dbUri     = if (!is.null(dbUri)) dbUri,
        hVcfFiles = if (!is.null(hVcfFiles)) hVcfFiles
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

        dbUriStatus <- ifelse(is.na(object@dbUri), absent, present)
        hVcfStatus  <- ifelse(is.na(object@hVcfFiles), absent, present)

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
#' @rdname configFilePath
#' @export
setMethod(
    f = "configFilePath",
    signature = signature(object = "PHGLocalCon"),
    definition = function(object) {
        return(object@configFilePath)
    }
)


## ----
#' @rdname dbName
#' @export
setMethod(
    f = "dbName",
    signature = signature(object = "PHGLocalCon"),
    definition = function(object) {
        return(object@dbName)
    }
)


## ----
#' @rdname dbType
#' @export
setMethod(
    f = "dbType",
    signature = signature(object = "PHGLocalCon"),
    definition = function(object) {
        return(object@dbType)
    }
)


## ----
#' @rdname showPHGMethods
#' @export
setMethod(
    f = "showPHGMethods",
    signature = signature(object = "PHGLocalCon"),
    definition = function(object, showAdvancedMethods) {
        return(
            methodTableFromLocal(
                configFilePath(object),
                showAdvancedMethods
            )
        )
    }
)


