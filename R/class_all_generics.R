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
#' @title Return name of DB
#'
#' @description
#' Returns the name for a given PHG database
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname dbName
#' @export
setGeneric("dbName", function(object, ...) standardGeneric("dbName"))


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
#' @title Return a PHG connection object
#'
#' @description
#' Returns an \code{rPHG} connection object
#'
#' @param object an \code{rPHG} method object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname phgConObj
#' @export
setGeneric("phgConObj", function(object, ...) standardGeneric("phgConObj"))


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
#' @title Return reference ranges
#'
#' @description
#' Get reference range data for a given PHG method
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
#' Gets sample ID data for a given PHG method
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

