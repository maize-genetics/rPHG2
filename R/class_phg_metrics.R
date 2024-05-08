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

## ----
#' @title Show methods for PHGMetrics objects
#'
#' @description
#' Prints out information regarding properties from the \code{PHGMetrics}
#' class to the console
#'
#' @param object A \code{\linkS4class{PHGMetrics}} object
#'
#' @docType methods
#' @rdname PHGMetrics-class
#' @aliases show,PHGMetrics-method
setMethod(
    f = "show",
    signature = "PHGMetrics",
    definition = function(object) {
        maxPrint <- 2

        # Output header
        cat(
            paste0(
                "A ", cli::style_bold("PHGMetrics"), " object containing ",
                cli::style_bold(length(metricsTable(object))), " tables:\n"
            )
        )

        # Display metrics for gVCF and AnchorWave

        if (length(object@metricAlign) != 0) {
            displayMetrics(object@metricAlign, "AnchorWave data", maxPrint)
        }

        if (length(object@metricGvcf) != 0) {
            displayMetrics(object@metricGvcf, "gVCF data", maxPrint)
        }
    }
)



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
            rlang::abort("Provided 'value' is not a named 'vector'")
        }

        if (!is.character(value)) {
            rlang::abort("Elements in vector for 'value' must be of type 'character'")
        }

        if (any(is.na(names(value)))) {
            offenders <- value[is.na(names(value))]
            for (off in offenders) {
                cat(metMessenger(off, "warn_03"))
            }

            value <- value[value != offenders]

            if (length(value) == 0) {
                rlang::abort("No provided IDs found in object after NA check")
            }
        }

        if (any(duplicated(names(value)))) {
            offenders <- value[duplicated(names(value))]
            for (off in offenders) {
                cat(metMessenger(off, "warn_04"))
            }

            value <- value[value != offenders]

            if (length(value) == 0) {
                rlang::abort("No provided IDs found in object after duplicate check")
            }
        }

        if (any(duplicated(value))) {
            offenders <- value[duplicated(value)]
            for (off in offenders) {
                cat(metMessenger(off, "warn_04"))
            }

            value <- value[value != offenders]

            if (length(value) == 0) {
                rlang::abort("No provided IDs found in object after duplicate check")
            }
        }

        if (all(value %in% metricsIds(object))) {
            rlang::abort("No IDs changed (all new values already found in object)")
        }

        if (any(value %in% metricsIds(object))) {
            offenders <- value[value %in% metricsIds(object)]
            for (off in offenders) {
                cat(metMessenger(off, "warn_02"))
            }

            value <- value[value != offenders]

            if (length(value) == 0) {
                rlang::abort("No IDs remaining after duplicate check")
            }
        }

        if (any(!names(value) %in% metricsIds(object))) {
            rlang::abort("No provided IDs found in object")
        }

        newNames <- value
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
        if (!is.list(value)) {
            rlang::abort("Provided 'value' not of type 'list'")
        }

        if (is.data.frame(value)) {
            rlang::abort("Provided 'value' must be in a named 'list'")
        }

        if (is.null(names(value))) {
            rlang::abort("Provided 'list' object does not have names")
        }

        if (any(duplicated(names(value)))) {
            rlang::abort("Provided 'list' object has duplicated IDs (all IDs must be unique)")
        }

        validAlgnDfs <- list()
        validGvcfDfs <- list()

        validFileAlgnIds <- character()
        validFileGvcfIds <- character()

        for (metricName in names(value)) {
            metricValue <- value[[metricName]]

            if (metricName %in% metricsIds(object)) {
                cat(metMessenger(metricName, "warn_02"))
            } else if (is(metricValue, "character") && file.exists(metricValue)) {
                if (isValidAnchor(metricValue)) {
                    validAlgnDfs[[metricName]] <- readMetricFiles(metricValue)[[1]]
                    validFileAlgnIds <- c(validFileAlgnIds, basename(metricValue))
                } else if (isValidGvcf(metricValue)) {
                    validGvcfDfs[[metricName]] <- readMetricFiles(metricValue)[[1]]
                    validFileGvcfIds <- c(validFileGvcfIds, basename(metricValue))
                } else {
                    cat(metMessenger(metricName, "warn_01"))
                }
            } else if (is(metricValue, "data.frame")) {
                if (all(camelToSnake(colnames(metricValue)) %in% camelToSnake(PHG_METRICS$VALID_ANCHOR_HEADERS))) {
                    cat(metMessenger(metricName, "success_02"))
                    validAlgnDfs[[metricName]] <- metricValue
                    validFileAlgnIds <- c(validFileAlgnIds, metricName)
                } else if (all(camelToSnake(colnames(metricValue)) %in% camelToSnake(PHG_METRICS$VALID_GVCF_HEADERS))) {
                    cat(metMessenger(metricName, "success_02"))
                    validGvcfDfs[[metricName]] <- metricValue
                    validFileGvcfIds <- c(validFileGvcfIds, metricName)
                } else {
                    cat(metMessenger(metricName, "warn_01"))
                }
            } else {
                cat(metMessenger(metricName, "warn_01"))
            }
        }

        if (length(validAlgnDfs) == 0 && length(validGvcfDfs) == 0) {
            rlang::abort("No valid metrics tables could be identified")
        }

        md <- metricsMetaData(object)
        if (length(validAlgnDfs) != 0) {
            mdAdd <- tibble::tibble(
                file = validFileAlgnIds,
                type = "align",
                id = names(validAlgnDfs)
            )
            object@metricAlign <- c(object@metricAlign, validAlgnDfs)
        }
        if (length(validGvcfDfs) != 0) {
            mdAdd <- tibble::tibble(
                file = validFileGvcfIds,
                type = "gvcf",
                id = names(validGvcfDfs)
            )
            object@metricGvcf <- c(object@metricGvcf, validGvcfDfs)
        }

        # Combine the original and additional metadata
        mdNew <- rbind(md, mdAdd)
        slot(object, "metadata") <- mdNew

        return(object)
    }
)


