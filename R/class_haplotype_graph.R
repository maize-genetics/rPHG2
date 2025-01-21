## ----
#' @title A HaplotypeGraph Class
#'
#' @description
#' Class \code{HaplotypeGraph} defines a \code{rPHG} Class for storing
#' a \code{HaplotypeGraph} object defined in the PHG API
#'
#' @slot nChrom
#' Number of chromosomes
#' @slot nRefRanges
#' Number of reference ranges
#' @slot nSamples
#' Number of samples
#' @slot jHapGraph
#' An \code{rJava} \code{jobjRef} object representing a \code{HaplotypeGraph}
#' class in the PHG API
#' @slot jMemAddress
#' An identifier string to the JVM memory space
#' @slot dbUri
#' A character representation of the PHGv2 DB instance
#'
#' @name HaplotypeGraph-class
#' @rdname HaplotypeGraph-class
#' @exportClass HaplotypeGraph
setClass(
    Class = "HaplotypeGraph",
    slots = c(
        nChrom      = "integer",
        nRefRanges  = "integer",
        nSamples    = "integer",
        jHapGraph   = "jobjRef",
        jMemAddress = "character",
        dbUri       = "character"
    ),
    prototype = list(
        nChrom      = NA_integer_,
        nRefRanges  = NA_integer_,
        nSamples    = NA_integer_,
        jHapGraph   = rJava::.jnull(),
        jMemAddress = NA_character_,
        dbUri       = NA_character_
    )
)


## ----
#' @title HaplotypeGraph validation
#'
#' @name HaplotypeGraph-validity
#'
#' @description Checks if \code{HaplotypeGraph} class objects are valid.
#'
#' @param object A \code{HaplotypeGraph} object.
#'
#' @importFrom curl has_internet
setValidity("HaplotypeGraph", function(object) {
    errors <- character()

    jObjRef <- javaRefObj(object)

    if (!any(names(jObjRef) == "getClass()")) {
        msg <- "Could not find `getClass()` getter from reference object"
        errors <- c(errors, msg)
    }

    jObjRefClass <- jObjRef$getClass()$getName()
    if (jObjRefClass != PHG_JVM$HAP_GRAPH) {
        msg <- "Reference object is not of type `HaplotypeGraph`"
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
})


## ----
#' @title Helper function to build HaplotypeGraph object
#'
#' @description
#' Creates a \code{\linkS4class{HaplotypeGraph}} object to be used to build and store
#' an \code{rJava} reference object pointing to a \code{HaplotypeGraph} object
#' from the PHG API.
#'
#' @param phgLocalCon A \code{\linkS4class{PHGLocalCon}} object.
#'
#' @importFrom methods is
#'
#' @export
buildHaplotypeGraph <- function(
    phgLocalCon
) {
    if (!isJvmInitialized()) {
        rlang::abort("JVM is not initialized - please run 'initPhg()' with PHGv2 library path")
    }

    if (!is(phgLocalCon, "PHGLocalCon")) {
        rlang::abort("phgLocalCon object is not of type PHGLocalCon")
    }

    jvmGraph <- hapGraphConstructor(hVcfFiles(phgLocalCon))

    pointer <- gsub(".*@", "", rJava::.jstrVal(jvmGraph))

    methods::new(
        Class       = "HaplotypeGraph",
        nChrom      = jvmGraph$getContigs()$size(),
        nRefRanges  = jvmGraph$numberOfRanges(),
        nSamples    = jvmGraph$numberOfSamples(),
        jHapGraph   = jvmGraph,
        jMemAddress = pointer,
        dbUri       = host(phgLocalCon)
    )
}



# /// Methods (show) ////////////////////////////////////////////////

## ----
#' @title Show methods for HaplotypeGraph objects
#'
#' @description
#' Prints out information regarding properties from the \code{HaplotypeGraph}
#' class to the console
#'
#' @param object A \code{\linkS4class{HaplotypeGraph}} object
#'
#' @docType methods
#' @rdname HaplotypeGraph-class
#' @aliases show,HaplotypeGraph-method
setMethod(
    f = "show",
    signature = "HaplotypeGraph",
    definition = function(object) {
        pointerSymbol <- cli::col_green(cli::symbol$pointer)

        msg <- c(
            paste0(
                "A ", cli::style_bold("HaplotypeGraph"), " object @ ",
                cli::style_bold(cli::col_blue(javaMemoryAddress(object)))
            ),
            paste0(" ", pointerSymbol, " # of ref ranges....: ", cli::style_bold(numberOfRefRanges(object))),
            paste0(" ", pointerSymbol, " # of samples.......: ", cli::style_bold(numberOfSamples(object))),
            paste0(" ", pointerSymbol, " # of chromosomes...: ", cli::style_bold(numberOfChromosomes(object)))
        )

        cat(msg, sep = "\n")
    }
)



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @rdname host
#' @export
setMethod(
    f = "host",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@dbUri)
    }
)


## ----
#' @rdname javaMemoryAddress
#' @export
setMethod(
    f = "javaMemoryAddress",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@jMemAddress)
    }
)


## ----
#' @rdname javaRefObj
#' @export
setMethod(
    f = "javaRefObj",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@jHapGraph)
    }
)


## ----
#' @rdname numberOfChromosomes
#' @export
setMethod(
    f = "numberOfChromosomes",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@nChrom)
    }
)


## ----
#' @rdname numberOfRefRanges
#' @export
setMethod(
    f = "numberOfRefRanges",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@nRefRanges)
    }
)


## ----
#' @rdname numberOfSamples
#' @export
setMethod(
    f = "numberOfSamples",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@nSamples)
    }
)


## ----
#' @param nThreads
#' Number of threads to use for JVM evaluation. Defaults to \code{2}.
#'
#' @rdname readHapIds
#' @export
setMethod(
    f = "readHapIds",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object, nThreads = 2) {
        return(hapIdsFromJvmGraph(javaRefObj(object), nThreads))
    }
)


## ----
#' @rdname readHapIdMetaData
#' @export
setMethod(
    f = "readHapIdMetaData",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(hapIdMetaDataFromJvmGraph(javaRefObj(object)))
    }
)


## ----
#' @rdname readHapIdPosMetaData
#' @export
setMethod(
    f = "readHapIdPosMetaData",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(hapIdPosMetaDataFromJvmGraph(javaRefObj(object)))
    }
)


## ----
#' @param nThreads
#' Number of threads to use for JVM evaluation. Defaults to \code{2}.
#'
#' @rdname readPhgDataSet
#' @export
setMethod(
    f = "readPhgDataSet",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object, nThreads = 2) {
        return(phgDataSetFromJvmGraph(object, nThreads))
    }
)


## ----
#' @rdname readRefRanges
#' @export
setMethod(
    f = "readRefRanges",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(refRangesFromJvmGraph(javaRefObj(object)))
    }
)


## ----
#' @rdname readSamples
#' @export
setMethod(
    f = "readSamples",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(samplesFromJvmGraph(javaRefObj(object)))
    }
)


