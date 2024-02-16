## ----
#' @title A HaplotypeGraph Class
#'
#' @description
#' Class \code{HaplotypeGraph} defines a \code{rPHG} Class for storing
#' a \code{HaplotypeGraph} object defined in the PHG API
#'
#' @slot methodID A \code{\linkS4class{PHGMethod}} object
#' @slot methodType The method type (e.g. PATHS, CONSENSUS, etc.)
#' @slot nChrom Number of chromosomes
#' @slot nNodes Number of nodes
#' @slot nRefRanges Number of reference ranges
#' @slot nTaxa Number of taxa
#' @slot jHapGraph An \code{rJava} \code{jobjRef} object representing a
#'    \code{HaplotypeGraph} class in the PHG API
#' @slot jMemAddress An identifier string to the JVM memory space
#'
#' @name HaplotypeGraph-class
#' @rdname HaplotypeGraph-class
#' @exportClass HaplotypeGraph
setClass(
    Class = "HaplotypeGraph",
    slots = c(
        methodID    = "character",
        methodType  = "character",
        nChrom      = "integer",
        nNodes      = "integer",
        nRefRanges  = "integer",
        nTaxa       = "integer",
        jHapGraph   = "jobjRef",
        jMemAddress = "character"
    ),
    prototype = list(
        methodID    = NA_character_,
        methodType  = NA_character_,
        nChrom      = NA_integer_,
        nNodes      = NA_integer_,
        nRefRanges  = NA_integer_,
        nTaxa       = NA_integer_,
        jHapGraph   = rJava::.jnull(),
        jMemAddress = NA_character_
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
    if (jObjRefClass != "net.maizegenetics.pangenome.api.HaplotypeGraph") {
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
#' @param phgMethodObj A \code{\linkS4class{PHGMethod}} object.
#' @param chrom A vector of chromosomes to include in graph. If NULL, defaults
#'   to all. To specify multiple chromosome, pass as a vector of strings (i.e.
#'   \code{c("1", "2", "3")}). Is currently only used for haplotypes.
#' @param includeSequence Whether to include sequences in haplotype nodes.
#'   Is currently only used for haplotypes. NOTE: this will greatly increase
#'   memory consumption!
#' @param includeVariants Whether to include variant contexts in haplotype
#'   nodes. Is currently only used for haplotypes. NOTE: this will greatly
#'   increase memory consumption!
#'
#' @export
buildHaplotypeGraph <- function(
    phgMethodObj,
    chrom = NULL,
    includeSequence = FALSE,
    includeVariants = FALSE
) {
    conMethod <- phgMethodId(phgMethodObj)
    conObj    <- phgConObj(phgMethodObj)
    conType   <- phgType(conObj)

    if (conType != "local") {
        stop(
            "Graphs can only be built using local PHG connection (`PHGLocalCon`) objects",
            call. = FALSE
        )
    }

    methMeta   <- showPHGMethods(conObj, showAdvancedMethods = TRUE)
    methodType <- methMeta[methMeta$method_name == conMethod, ]$type_name

    # NOTE - unresolved issues with ifelse, using conventional if/else instead
    if (methodType == "PATHS") {
        phgObj <- graphFromPaths(
            configFilePath(conObj),
            conMethod
        )
    } else {
        phgObj <- graphFromHaplotypes(
            configFilePath(conObj),
            conMethod,
            chrom,
            includeSequence,
            includeVariants
        )
    }

    pointer <- gsub(".*@", "", rJava::.jstrVal(phgObj))

    methods::new(
        Class       = "HaplotypeGraph",
        methodID    = conMethod,
        methodType  = methodType,
        nChrom      = phgObj$numberOfChromosomes(),
        nNodes      = phgObj$numberOfNodes(),
        nRefRanges  = phgObj$numberOfRanges(),
        nTaxa       = phgObj$totalNumberTaxa(),
        jHapGraph   = phgObj,
        jMemAddress = pointer
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
            paste0(" ", pointerSymbol, " Method.............: ", cli::style_bold(phgMethodId(object))),
            paste0(" ", pointerSymbol, " # of nodes.........: ", numberOfNodes(object)),
            paste0(" ", pointerSymbol, " # of ref ranges....: ", numberOfRefRanges(object)),
            paste0(" ", pointerSymbol, " # of taxa..........: ", numberOfTaxa(object)),
            paste0(" ", pointerSymbol, " # of chromosomes...: ", numberOfChromosomes(object))
        )

        cat(msg, sep = "\n")
    }
)



# /// Methods (general) /////////////////////////////////////////////

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
#' @rdname numberOfNodes
#' @export
setMethod(
    f = "numberOfNodes",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@nNodes)
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
#' @rdname numberOfTaxa
#' @export
setMethod(
    f = "numberOfTaxa",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@nTaxa)
    }
)


## ----
#' @rdname phgMethodId
#' @export
setMethod(
    f = "phgMethodId",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@methodID)
    }
)


## ----
#' @rdname phgMethodType
#' @export
setMethod(
    f = "phgMethodType",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(object@methodType)
    }
)


## ----
#' @rdname readHaplotypeIds
#' @export
setMethod(
    f = "readHaplotypeIds",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(hapIdsFromGraphObj(javaRefObj(object)))
    }
)


## ----
#' @rdname readPHGDataSet
#' @export
setMethod(
    f = "readPHGDataSet",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(phgDataSetFromGraphObj(javaRefObj(object), verbose = TRUE))
    }
)


## ----
#' @rdname readRefRanges
#' @export
setMethod(
    f = "readRefRanges",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(refRangesFromGraphObj(javaRefObj(object)))
    }
)


## ----
#' @rdname readSamples
#' @export
setMethod(
    f = "readSamples",
    signature = signature(object = "HaplotypeGraph"),
    definition = function(object) {
        return(samplesFromGraphObj(javaRefObj(object)))
    }
)


