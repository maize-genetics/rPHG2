## ----
#' @title A PHGLocalCon Class
#'
#' @description
#' A \code{PHGLocalCon} class defines a \code{rPHG} class for storing
#' local config file data.
#'
#' @slot dbName Name of database
#' @slot dbType Type of database
#' @slot configFilePath Path to configuration file
#'
#' @name PHGLocalCon-class
#' @rdname PHGLocalCon-class
#' @exportClass PHGLocalCon
setClass(
    Class    = "PHGLocalCon",
    contains = "PHGCon",
    representation = representation(
        dbName         = "character",
        dbType         = "character",
        configFilePath = "character"
    ),
    prototype = prototype(
        dbName         = NA_character_,
        dbType         = NA_character_,
        configFilePath = NA_character_
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

    if (!file.exists(object@configFilePath)) {
        msg <- "Path to config file does not exist"
        errors <- c(errors, msg)
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
#' @param file A path to a PHG configuration file
#'
#' @export
PHGLocalCon <- function(file) {
    configCatcher(file)

    configProperties <- parseConfigFile(file)

    methods::new(
        Class          = "PHGLocalCon",
        phgType        = "local",
        host           = configProperties$host,
        dbName         = basename(configProperties$DB),
        dbType         = configProperties$DBtype,
        configFilePath = normalizePath(file)
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
        msg <- c(
            paste0("A ", cli::style_bold("PHGLocalCon"), " connection object"),
            paste0(" ", pointerSymbol, " Host......: ", object@host),
            paste0(" ", pointerSymbol, " DB Name...: ", object@dbName),
            paste0(" ", pointerSymbol, " DB Type...: ", object@dbType)
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


