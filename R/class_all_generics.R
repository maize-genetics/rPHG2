## ----
#' @title Return URL path
#'
#' @description
#' Returns the Uniform Resource Locator (URL) of a \code{BrapiCon} object.
#'
#' @param object an \code{rPHG} local or server connection object.
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname brapiURL
#' @export
setGeneric("brapiURL", function(object, ...) standardGeneric("brapiURL"))


## ----
#' @title Return BrAPI version ID
#'
#' @description
#' Returns the version ID for a BrAPI-compliant PHG server
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname brapiVersion
#' @export
setGeneric("brapiVersion", function(object, ...) standardGeneric("brapiVersion"))


## ----
#' @title Return host data
#'
#' @description
#' Returns the host information for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname host
#' @export
setGeneric("host", function(object, ...) standardGeneric("host"))


## ----
#' @title Return protocol value
#'
#' @description
#' Returns the protocol information for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname httProtocol
#' @export
setGeneric("httProtocol", function(object, ...) standardGeneric("httProtocol"))


## ----
#' @title Return hVCF files
#'
#' @description
#' Returns a list of hVCF files.
#'
#' @param object an \code{rPHG} local connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname hVcfFiles
#' @export
setGeneric("hVcfFiles", function(object, ...) standardGeneric("hVcfFiles"))


## ----
#' @title Return \code{rJava} reference object
#'
#' @description
#' Returns the \code{rJava} memory reference for a given \code{rPHG} object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname javaMemoryAddress
#' @export
setGeneric("javaMemoryAddress", function(object, ...) standardGeneric("javaMemoryAddress"))


## ----
#' @title Return \code{rJava} reference object
#'
#' @description
#' Returns the \code{rJava} memory reference for a given \code{rPHG} object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname javaRefObj
#' @export
setGeneric("javaRefObj", function(object, ...) standardGeneric("javaRefObj"))


## ----
#' @title Return number of chromosomes
#'
#' @description
#' Returns the number of chromosomes for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfChromosomes
#' @export
setGeneric("numberOfChromosomes", function(object, ...) standardGeneric("numberOfChromosomes"))


## ----
#' @title Return number of reference ranges
#'
#' @description
#' Returns the number of reference ranges for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfRefRanges
#' @export
setGeneric("numberOfRefRanges", function(object, ...) standardGeneric("numberOfRefRanges"))


## ----
#' @title Return number of taxa
#'
#' @description
#' Returns the number of taxa for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfTaxa
#' @export
setGeneric("numberOfTaxa", function(object, ...) standardGeneric("numberOfTaxa"))


## ----
#' @title Return type of PHG connection
#'
#' @description
#' Returns the PHG type for a given \code{rPHG} local or server connection object
#'
#' @param object an \code{rPHG} connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname phgType
#' @export
setGeneric("phgType", function(object, ...) standardGeneric("phgType"))


## ----
#' @title Return port value
#'
#' @description
#' Returns the port information for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname port
#' @export
setGeneric("port", function(object, ...) standardGeneric("port"))


## ----
#' @title Return haplotype IDs
#'
#' @description
#' Gets haplotype ID for given samples and reference ranges from a
#' \code{HaplotypeGraph}
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readHapIds
#' @export
setGeneric("readHapIds", function(object, ...) standardGeneric("readHapIds"))


## ----
#' @title Return haplotype ID metadata
#'
#' @description
#' Gets haplotype ID metadata for given samples and reference ranges from a
#' \code{HaplotypeGraph}
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readHapIdMetaData
#' @export
setGeneric("readHapIdMetaData", function(object, ...) standardGeneric("readHapIdMetaData"))


## ----
#' @title Return reference ranges
#'
#' @description
#' Get reference range data from a \code{HaplotypeGraph}
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readRefRanges
#' @export
setGeneric("readRefRanges", function(object, ...) standardGeneric("readRefRanges"))


## ----
#' @title Return samples IDs
#'
#' @description
#' Gets sample ID data from a \code{HaplotypeGraph}
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readSamples
#' @export
setGeneric("readSamples", function(object, ...) standardGeneric("readSamples"))


## ----
#' @title Return server information
#'
#' @description
#' Get available BrAPI calls from BrAPI compliant PHG server
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname serverInfo
#' @export
setGeneric("serverInfo", function(object, ...) standardGeneric("serverInfo"))


