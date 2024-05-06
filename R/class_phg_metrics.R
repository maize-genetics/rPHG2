## ----
#' @title A PHGMetrics Class
#'
#' @description
#' A \code{PHGMetrics} class is a general class for storing metrics data
#' generated from PHGv2 operations.
#'
#' @slot metricAlign
#' AnchorWave alignment metrics (\code{.anchorspro})
#' @slot metricGvcf
#' gVCF metrics
#' @slot metadata
#' A \code{data.frame} object of key-value data for files
#'
#' @name PHGMetrics-class
#' @rdname PHGMetrics-class
#' @exportClass PHGMetrics
setClass(
    Class = "PHGMetrics",
    representation = representation(
        metricAlign = "list",
        metricGvcf = "list",
        metadata    = "tbl_df"
    ),
    prototype = list(
        metricAlign = list(),
        metricGvcf = list(),
        metadata    = tibble::tibble()
    )
)


## ----
#' @title PHGMetrics object constructor
#'
#' @description
#' \code{PHGMetrics} is the primary container for housing PHGv2 metrics data
#'
#' @param dir
#' A collection of directories
#' @param metadata
#' key-value metadata for files
#'
#' @return A \code{PHGMetrics} object.
#'
#' @export
PHGMetrics <- function(paths = NULL, metadata = NULL) {
    # V01 - check if directories or files exist
    dirFilt <- paths[dir.exists(paths)]
    filFilt <- paths[file.exists(paths)]

    # V02 - if no files or directories exist: exception
    if (length(dirFilt) == 0 && length(filFilt) == 0) {
        stop("No valid paths given")
    }

    # V03 - recursively pull metric files from directories (if found)
    # NOTE - probably don't need a pattern but good for only keeping
    #        possibly valid files in memory footprint in case it's a
    #        massive directory
    if (length(dirFilt) != 0) {
        metFilesFromDir <- list.files(
            path       = dirFilt,
            pattern    = "\\.tsv$|\\.anchorspro$",
            full.names = TRUE,
            recursive  = TRUE
        )
        metFilesFromDir <- normalizePath(metFilesFromDir)
    }

    # V04 - combine all files as one collection
    posFiles <- c(metFilesFromDir, filFilt)

    # V05 - split out files based on type
    gvcfMet <- posFiles[endsWith(posFiles, "tsv")]
    algnMet <- posFiles[endsWith(posFiles, "anchorspro")]

    # V06 - if .tsv or .anchorspro files are identified: check contents for
    #       further validity
    if (length(gvcfMet) != 0) {
        gvcfMet <- gvcfMet[isValidGvcf(gvcfMet)]
    }

    if (length(algnMet) != 0) {
        algnMet <- algnMet[isValidAnchor(algnMet)]
    }

    # V07 - if no .tsv or .anchorspro files are identified: exception
    if (length(gvcfMet) == 0 && length(algnMet) == 0) {
        stop("No valid gVCF or AnchorWave files detected from paths")
    }

    # If files are fully vetted: read into memory and add to list
    if (length(gvcfMet) != 0) {
        gvcfDfs <- readMetricFiles(gvcfMet)
    } else {
        gvcfDfs <- list()
    }

    if (length(algnMet) != 0) {
        algnDfs <- readMetricFiles(algnMet)
    } else {
        algnDfs <- list()
    }

    if (is.null(metadata)) {
        metadata <- tibble::tibble(
            file = basename(c(gvcfMet, algnMet)),
            type = c(rep("gvcf", length(gvcfMet)), rep("align", length(algnMet))),
            id   = tools::file_path_sans_ext(basename(c(gvcfMet, algnMet)))
        )
    }

    if (is.null(dir)) {
        methods::new("PHGMetrics")
    } else {
        methods::new(
            Class       = "PHGMetrics",
            metricAlign = algnDfs,
            metricGvcf  = gvcfDfs,
            metadata    = metadata
        )
    }
}



# /// Methods (show) ////////////////////////////////////////////////



# /// Methods (override) ////////////////////////////////////////////

## ----
#' @export
.DollarNames.PHGMetrics <- function(x, pattern = "") {
    grep(pattern, x@metadata$id, value = TRUE)
}


## ----
#' @export
setMethod("$", "PHGMetrics", function(x, name) {
    slot(x, "metadata")[["id"]][x@metadata[["id"]] == name]
})



# /// Methods (general) /////////////////////////////////////////////

# NEED:
# [x] metricsIds
# [x] metricsIds<-
# [x] metricsMetaData
# [x] metricsTable
# [ ] metricsTable<-


## ----
#' @rdname metricsIds
#' @export
setMethod(
    f = "metricsIds",
    signature = signature(object = "PHGMetrics"),
    definition = function(object) {
        return(
            c(
                names(object@metricAlign),
                names(object@metricGvcf)
            )
        )
    }
)


## ----
#' @param value
#' A named vector where name(s) are the old IDs and elements are the new IDs
#'
#' @rdname metricsIds
#' @export
setMethod(
    f = "metricsIds<-",
    signature = signature(object = "PHGMetrics"),
    definition = function(object, value) {
        if (is.null(names(value))) {
            stop("Provided 'value' is not a named 'vector'")
        }

        if (!is.character(value)) {
            stop("Elements in vector for 'value' must be of type 'character'")
        }

        if (any(!names(value) %in% metricsIds(object))) {
            stop("No provided IDs found in object")
        }

        newNames <- c(value)
        oldNames <- names(newNames)

        md <- metricsMetaData(object)
        oldGvcfIds <- md$id[md$type == "gvcf"]
        oldAlgnIds <- md$id[md$type == "align"]

        newGvcfIds <- newNames[oldGvcfIds]
        newAlgnIds <- newNames[oldAlgnIds]
        newGvcfIds[is.na(newGvcfIds)] <- oldGvcfIds[is.na(newGvcfIds)]
        newAlgnIds[is.na(newAlgnIds)] <- oldAlgnIds[is.na(newAlgnIds)]

        mdNew <- md
        mdNew[mdNew$id %in% oldNames, "id"] <- newNames

        slot(object, "metadata")  <- mdNew
        names(object@metricGvcf)  <- newGvcfIds
        names(object@metricAlign) <- newAlgnIds

        return(object)
    }
)


## ----
#' @rdname metricsMetaData
#' @export
setMethod(
    f = "metricsMetaData",
    signature = signature(object = "PHGMetrics"),
    definition = function(object) {
        return(object@metadata)
    }
)


## ----
#' @rdname metricsTable
#' @export
setMethod(
    f = "metricsTable",
    signature = signature(object = "PHGMetrics"),
    definition = function(object, name = NULL, type = NULL) {

        if (is.null(name) && is.null(type)) {
            return(
                c(
                    object@metricAlign,
                    object@metricGvcf
                )
            )
        }

        if (!is.null(name) && !is.null(type)) {
            type <- NULL
        }

        if (!name %in% metricsIds(object) && !is.null(name)) {
            stop("Provided 'name' not found in object")
        }

        if (!is.null(name) && is.null(type)) {
            return(
                c(
                    object@metricAlign,
                    object@metricGvcf
                )[names(c(
                    object@metricAlign,
                    object@metricGvcf
                )) == name][[1]]
            )
        }

        if (!is.null(type) && is.null(name)) {
            if (!type %in% PHG_METRICS$VALID_METRICS_IDS) {
                stop("Provided 'type' not a valid metrics ID")
            }

            metrics <- switch (type,
                "align" = object@metricAlign,
                "gvcf"  = object@metricGvcf,
            )

            if (length(metrics) == 1) {
                return(metrics[[1]])
            } else {
                return(metrics)
            }
        }
    }
)


## ----
#' @param value
#' A named list where names are the metric table IDs and values are valid
#' metrics tables. Currently, gVCF and AnchorWave alignment files are allowed.
#'
#' @rdname metricsTable
#' @export
setMethod(
    f = "metricsTable<-",
    signature = signature(object = "PHGMetrics"),
    definition = function(object, value) {
        # TODO - refactor this!
        if (!is.list(value)) {
            stop("Provided 'value' not of type 'list'")
        }

        if (is.data.frame(value)) {
            stop("Provided 'value' must be in a named 'list'")
        }

        if (is.null(names(value))) {
            stop("Provided 'list' object does not have names")
        }

        validAlgnDfs <- vector("list", length(value))
        validGvcfDfs <- vector("list", length(value))

        validFileAlgnIds <- vector("character")
        validFileGvcfIds <- vector("character")

        metNames <- names(value)

        metIndex <- 1
        for (v in value) {
            if (is(v, "character")) {
                if (file.exists(v)) {
                    if (isValidAnchor(v)) {
                        validAlgnDfs[[metIndex]] <- readMetricFiles(v)[[1]]
                        validFileAlgnIds <- c(validFileAlgnIds, basename(v))
                    } else if (isValidGvcf(v)) {
                        validGvcfDfs[[metIndex]] <- readMetricFiles(v)[[1]]
                        validFileGvcfIds <- c(validFileGvcfIds, basename(v))
                    } else {
                        metIndex <- metIndex + 1
                    }
                } else {
                    metIndex <- metIndex + 1
                }
            } else {
                if (is(v, "data.frame")) {
                    if (all(camelToSnake(colnames(v)) %in% camelToSnake(PHG_METRICS$VALID_ANCHOR_HEADERS))) {
                        validAlgnDfs[[metIndex]] <- v
                        validFileAlgnIds <- c(validFileAlgnIds, "df_var")
                    } else if (all(camelToSnake(colnames(v)) %in% camelToSnake(PHG_METRICS$VALID_GVCF_HEADERS))) {
                        validGvcfDfs[[metIndex]] <- v
                        validFileGvcfIds <- c(validFileGvcfIds, "df_var")
                    } else {
                        metIndex <- metIndex + 1
                    }
                } else {
                    metIndex <- metIndex + 1
                }
            }
        }

        names(validAlgnDfs) <- metNames
        names(validGvcfDfs) <- metNames

        validAlgnDfs <- validAlgnDfs[lengths(validAlgnDfs) != 0]
        validGvcfDfs <- validGvcfDfs[lengths(validGvcfDfs) != 0]

        if (length(validGvcfDfs) == 0 && length(validAlgnDfs) == 0) {
            stop("No valid metrics tables could be identified")
        } else {
            md <- metricsMetaData(object)
            if (length(validAlgnDfs) != 0) {
                mdAdd <- tibble::tibble(
                    file = validFileAlgnIds,
                    type = "align",
                    id   = names(validAlgnDfs)
                )
                object@metricAlign <- c(object@metricAlign, validAlgnDfs)
            }
            if (length(validGvcfDfs) != 0) {
                mdAdd <- tibble::tibble(
                    file = validFileGvcfIds,
                    type = "gvcf",
                    id   = names(validGvcfDfs)
                )
                object@metricGvcf <- c(object@metricGvcf, validGvcfDfs)
            }

            slot(object, "metadata") <- rbind(md, mdAdd)
        }

        return(object)
    }
)


