## ----
#' @title Return URL path
#'
#' @description
#' Returns the Uniform Resource Locator (URL) of a \code{BrapiCon} object.
#'
#' @param object an \code{rPHG2} local or server connection object.
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
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname brapiVersion
#' @export
setGeneric("brapiVersion", function(object, ...) standardGeneric("brapiVersion"))


## ----
#' @title Filter data by reference range
#'
#' @description
#' Filters \code{rPHG2}-related datasets by reference range coordinates or ID.
#'
#' @param object an \code{rPHG2} dataset
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname filterRefRanges
#' @export
setGeneric("filterRefRanges", function(object, ...) standardGeneric("filterRefRanges"))


## ----
#' @title Filter data by sample IDs
#'
#' @description
#' Filters \code{rPHG2}-related datasets by sample ID.
#'
#' @param object an \code{rPHG2} dataset
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname filterSamples
#' @export
setGeneric("filterSamples", function(object, ...) standardGeneric("filterSamples"))


## ----
#' @title Return host data
#'
#' @description
#' Returns the host information for a given object
#'
#' @param object an \code{rPHG2} local or server connection object
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
#' @param object an \code{rPHG2} local or server connection object
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
#' @param object an \code{rPHG2} local connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname hVcfFiles
#' @export
setGeneric("hVcfFiles", function(object, ...) standardGeneric("hVcfFiles"))


## ----
#' @title Return \code{rJava} reference object
#'
#' @description
#' Returns the \code{rJava} memory reference for a given \code{rPHG2} object
#'
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname javaMemoryAddress
#' @export
setGeneric("javaMemoryAddress", function(object, ...) standardGeneric("javaMemoryAddress"))


## ----
#' @title Return \code{rJava} reference object
#'
#' @description
#' Returns the \code{rJava} memory reference for a given \code{rPHG2} object
#'
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname javaRefObj
#' @export
setGeneric("javaRefObj", function(object, ...) standardGeneric("javaRefObj"))


## ----
#' @title Return metrics table IDs
#'
#' @description
#' Returns a PHG metrics table by given ID
#'
#' @param object an \code{rPHG2} metrics object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname metricsIds
#' @export
setGeneric("metricsIds", function(object, ...) standardGeneric("metricsIds"))

#' @rdname metricsIds
#' @export
setGeneric("metricsIds<-", function(object, ..., value) standardGeneric("metricsIds<-"))


## ----
#' @title Return metric metadata
#'
#' @description
#' Returns metadata information from \code{rPHG2} metrics objects
#'
#' @param object an \code{rPHG2} metrics object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname metricsMetaData
#' @export
setGeneric("metricsMetaData", function(object, ...) standardGeneric("metricsMetaData"))


## ----
#' @title Return a metrics table
#'
#' @description
#' Returns a PHG metrics table by given ID
#'
#' @param object an \code{rPHG2} metrics object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname metricsTable
#' @export
setGeneric("metricsTable", function(object, ...) standardGeneric("metricsTable"))

#' @rdname metricsTable
#' @export
setGeneric("metricsTable<-", function(object, ..., value) standardGeneric("metricsTable<-"))


## ----
#' @title Return number of chromosomes
#'
#' @description
#' Returns the number of chromosomes for a given object
#'
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfChromosomes
#' @export
setGeneric("numberOfChromosomes", function(object, ...) standardGeneric("numberOfChromosomes"))


## ----
#' @title Return number of haplotypes
#'
#' @description
#' Returns the number of haplotypes either for the whole dataset or by each
#' individual reference range.
#'
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfHaplotypes
#' @export
setGeneric("numberOfHaplotypes", function(object, ...) standardGeneric("numberOfHaplotypes"))


## ----
#' @title Return number of reference ranges
#'
#' @description
#' Returns the number of reference ranges for a given object
#'
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfRefRanges
#' @export
setGeneric("numberOfRefRanges", function(object, ...) standardGeneric("numberOfRefRanges"))


## ----
#' @title Return number of samples
#'
#' @description
#' Returns the number of samples for a given object
#'
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfSamples
#' @export
setGeneric("numberOfSamples", function(object, ...) standardGeneric("numberOfSamples"))


## ----
#' @title Return type of PHG connection
#'
#' @description
#' Returns the PHG type for a given \code{rPHG2} local or server connection
#' object
#'
#' @param object an \code{rPHG2} connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname phgType
#' @export
setGeneric("phgType", function(object, ...) standardGeneric("phgType"))


## ----
#' @title Generate dot plots
#'
#' @description
#' Generates collinearity (dot) plots between 2 samples
#'
#' @param object an \code{rPHG2} metrics object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname plotDot
#' @export
setGeneric("plotDot", function(object, ...) standardGeneric("plotDot"))


## ----
#' @title Generate GVCF plots
#'
#' @description
#' Generates general GVCF metrics plots for genome-wide GVCF statistics
#'
#' @param object an \code{rPHG2} metrics object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname plotGvcf
#' @export
setGeneric("plotGvcf", function(object, ...) standardGeneric("plotGvcf"))


## ----
#' @title Plot haplotype counts
#'
#' @description
#' Plots the counts of unique haplotype IDs found in each reference range.
#'
#' @param object a \code{\linkS4class{PHGDataSet}} object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname plotHaploCounts
#' @export
setGeneric("plotHaploCounts", function(object, ...) standardGeneric("plotHaploCounts"))


## ----
#' @title Plot haplotype distribution
#'
#' @description
#' Plots a general distribution of the number of unique haplotypes in a
#' \code{\linkS4class{PHGDataSet}} object.
#'
#' @param object a \code{\linkS4class{PHGDataSet}} object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname plotHaploDist
#' @export
setGeneric("plotHaploDist", function(object, ...) standardGeneric("plotHaploDist"))


## ----
#' @title Return port value
#'
#' @description
#' Returns the port information for a given object
#'
#' @param object an \code{rPHG2} local or server connection object
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
#' @param object an \code{rPHG2} local or server connection object
# @param nThreads Number of threads to use when evaluating haplotype IDs
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
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readHapIdMetaData
#' @export
setGeneric("readHapIdMetaData", function(object, ...) standardGeneric("readHapIdMetaData"))


## ----
#' @title Return haplotype ID positional metadata
#'
#' @description
#' Gets haplotype ID positional metadata for given samples and reference ranges
#' from a \code{HaplotypeGraph}
#'
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readHapIdPosMetaData
#' @export
setGeneric("readHapIdPosMetaData", function(object, ...) standardGeneric("readHapIdPosMetaData"))


## ----
#' @title Return all hVCF data from a JVM graph
#'
#' @description
#' Returns all haplotype, sample, and reference range IDs and relative metadata
#' from a \code{\linkS4class{HaplotypeGraph}} object.
#'
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readPhgDataSet
#' @export
setGeneric("readPhgDataSet", function(object, ...) standardGeneric("readPhgDataSet"))


## ----
#' @title Return reference ranges
#'
#' @description
#' Get reference range data from a \code{HaplotypeGraph}
#'
#' @param object an \code{rPHG2} local or server connection object
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
#' @param object an \code{rPHG2} local or server connection object
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
#' @param object an \code{rPHG2} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname serverInfo
#' @export
setGeneric("serverInfo", function(object, ...) standardGeneric("serverInfo"))


## ----
#' @importFrom GenomeInfoDb seqnames
NULL


## ----
#' @importFrom GenomeInfoDb seqnames<-
NULL


