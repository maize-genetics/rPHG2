## ----
#' @title A PHGCon Class
#'
#' @description
#' A \code{PHGCon} class is a parent class for
#' local config file data.
#'
#' @slot phgType What type of PHG connection is this?
#' @slot host Location path of local TileDB or server database
#'
#' @name PHGCon-class
#' @rdname PHGCon-class
#' @exportClass PHGCon
setClass(
    Class = "PHGCon",
    representation = representation(
        phgType = "character",
        host    = "character"
    ),
    prototype = prototype(
        phgType = NA_character_,
        host    = NA_character_
    )
)


## ----
#' @title PHGCon validation
#'
#' @name PHGCon-validity
#'
#' @description
#' Checks for correct data entry into \code{PHGCon} class
#'
#' @param object A \code{\linkS4class{PHGCon}} object
setValidity("PHGCon", function(object) {
    validConTypes <- c("local", "server")
    errors <- character()

    if (!object@phgType %in% validConTypes) {
        msg <- "Given PHG connection type is not allowed"
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) {
        return(TRUE)
    } else {
        return(errors)
    }
})



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @rdname host
#' @export
setMethod(
    f = "host",
    signature = signature(object = "PHGCon"),
    definition = function(object) {
        return(object@host)
    }
)

## ----
#' @rdname phgType
#' @export
setMethod(
    f = "phgType",
    signature = signature(object = "PHGCon"),
    definition = function(object) {
        return(object@phgType)
    }
)


