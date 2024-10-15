## ----
#' @title A JvmStats Class
#'
#' @description
#' An S4 class to represent statistics related to the Java Virtual Machine
#' (JVM).
#'
#' @slot nJars
#' An integer representing the number of JAR files currently loaded in the JVM.
#' @slot javaVersion
#' A character string representing the version of Java being used.
#' @slot phgVersion
#' A character string representing the version of PHG (Practical Haplotype
#' Graph).
#' @slot classPath
#' A character vector representing the JAR files found in the class path.
#' @slot maxMem
#' A numeric value representing the maximum amount of memory (in bytes) that
#' the JVM can use.
#' @slot totMem
#' A numeric value representing the total amount of memory (in bytes) currently
#' allocated to the JVM.
#' @slot freeMem
#' A numeric value representing the amount of free memory (in bytes) in the
#' JVM.
#' @slot allocMem
#' A numeric value representing the amount of memory (in bytes) currently
#' allocated by the JVM.
#' @slot memUnit
#' A character string representing the unit of memory (e.g., "bytes", "MB",
#' "GB").
#'
#' @return An object of class \code{JvmStats}.
#'
#' @name JvmStats-class
#' @rdname JvmStats-class
#' @exportClass JvmStats
setClass(
    Class = "JvmStats",
    slots = c(
        nJars       = "integer",
        javaVersion = "character",
        phgVersion  = "character",
        classPath   = "character",
        maxMem      = "numeric",
        totMem      = "numeric",
        freeMem     = "numeric",
        allocMem    = "numeric",
        memUnit     = "character"
    ),
    prototype = list(
        nJars       = NA_integer_,
        javaVersion = NA_character_,
        phgVersion  = NA_character_,
        maxMem      = NA_real_,
        totMem      = NA_real_,
        freeMem     = NA_real_,
        allocMem    = NA_real_,
        memUnit     = NA_character_
    )
)


## ----
#' @title Create an instance of the JvmStats class
#'
#' @description
#' Collects JVM statistics such as the number of JAR files loaded, Java
#' version, PHG version, and memory usage. Returns an instance of the
#' \code{JvmStats} class populated with these statistics.
#'
#' @return An object of class \code{JvmStats}, containing the following
#' fields:
#' \itemize{
#'   \item \code{nJars}: The number of JAR files in the JVM classpath.
#'   \item \code{javaVersion}: The version of Java being used.
#'   \item \code{phgVersion}: The version of PHG (Practical Haplotype
#'   Graph) being used.
#'   \item \code{maxMem}: The maximum memory (in GB) that the JVM can use.
#'   \item \code{totMem}: The total memory (in GB) allocated to the JVM.
#'   \item \code{freeMem}: The amount of free memory (in GB) in the JVM.
#'   \item \code{allocMem}: The amount of allocated memory (in GB) in the
#'   JVM.
#' }
#'
#' @details
#' This function requires that the JVM is initialized. If the JVM is not
#' initialized, it will throw an error. Use the \code{initPhg()} function
#' to initialize the JVM with the PHG library path before calling this
#' function.
#'
#' The function retrieves the number of JAR files in the classpath, the
#' version of Java and PHG being used, and memory statistics including
#' maximum memory, total memory, free memory, and allocated memory (all in
#' gigabytes).
#'
#' @examples
#' \dontrun{
#' # Ensure that the JVM is initialized with PHGv2 library path
#' initPhg("/path/to/phgv2")
#'
#' # Collect JVM statistics
#' stats <- jvmStats()
#' show(stats)
#' }
#'
#' @seealso \code{\link{JvmStats-class}}
#'
#' @export
jvmStats <- function() {
    if (!isJvmInitialized()) {
        rlang::abort("JVM is not initialized - please run 'initPhg()' with PHGv2 library path")
    }

    # Get number of JARs in class path
    classPath <- rJava::.jclassPath()
    nJars <- length(classPath[grepl("\\.jar$", classPath)])

    # Get Java version
    javaVersion <- rJava::.jcall("java/lang/System", "S", "getProperty", "java.version")

    # Attempt to get PHG version
    genUtils <- rJava::J(PHG_JVM$GEN_UTILS)
    if (!any(grepl("phgVersion()", names(genUtils)))) {
        phgVersion <- paste0(cli::symbol$leq, " 2.4.1.155")
    } else {
        phgVersion <- genUtils$phgVersion()
    }

    # Get memory profile
    runtime  <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
    gbConv   <- 1073741824 # 1024 ^ 3
    maxMem   <- round(rJava::.jcall(runtime, "J", "maxMemory") / gbConv, 3)
    totMem   <- round(rJava::.jcall(runtime, "J", "totalMemory") / gbConv, 3)
    freeMem  <- round(rJava::.jcall(runtime, "J", "freeMemory") / gbConv, 3)
    allocMem <- round(totMem - freeMem, 3)

    methods::new(
        Class       = "JvmStats",
        nJars       = nJars,
        javaVersion = javaVersion,
        phgVersion  = phgVersion,
        classPath   = classPath,
        maxMem      = maxMem,
        totMem      = totMem,
        freeMem     = freeMem,
        allocMem    = allocMem
    )
}



# /// Methods (show) ////////////////////////////////////////////////

## ----
#' @title Show method for JvmStats class
#'
#' @description
#' Displays JVM statistics in a formatted manner.
#'
#' @param object
#' An object of class \code{JvmStats}.
#'
#' @details
#' This method provides a structured and formatted output of the JVM statistics. It displays
#' the number of JARs, the Java version, the PHG version, and detailed memory statistics
#' (maximum memory, total memory, free memory, and allocated memory) in a human-readable format.
#' Memory values are shown in the respective units (e.g., bytes, MB, GB) defined in the object.
#'
#' @docType methods
#' @rdname JvmStats-class
#' @aliases show,JvmStats-method
setMethod(
    f = "show",
    signature = "JvmStats",
    definition = function(object) {
        pointerSymbol <- cli::col_green(cli::symbol$pointer)

        msg <- c(
            cli::style_bold("General Stats:"),
            paste0(" ", pointerSymbol, " # of JARs......: ", cli::style_bold(object@nJars)),
            paste0(" ", pointerSymbol, " Java version...: ", cli::style_bold(object@javaVersion)),
            paste0(" ", pointerSymbol, " PHG version....: ", cli::style_bold(object@phgVersion)),
            "",
            cli::style_bold("Memory Stats (GB):"),
            paste0(" ", pointerSymbol, " Max............: ", cli::style_bold(object@maxMem)),
            paste0(" ", pointerSymbol, " Total..........: ", cli::style_bold(object@totMem)),
            paste0(" ", pointerSymbol, " Free...........: ", cli::style_bold(object@freeMem)),
            paste0(" ", pointerSymbol, " Allocated......: ", cli::style_bold(object@allocMem))
        )

        cat(msg, sep = "\n")
    }
)



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @rdname javaVersion
#' @export
setMethod(
    f = "classPath",
    signature = signature(object = "JvmStats"),
    definition = function(object) {
        return(object@classPath)
    }
)


## ----
#' @rdname javaVersion
#' @export
setMethod(
    f = "javaVersion",
    signature = signature(object = "JvmStats"),
    definition = function(object) {
        return(object@javaVersion)
    }
)


## ----
#' @rdname jvmMemStats
#' @export
setMethod(
    f = "jvmMemStats",
    signature = signature(object = "JvmStats"),
    definition = function(object) {
        return(
            list(
                "max_memory"   = object@maxMem,
                "total_memory" = object@totMem,
                "free_memory"  = object@freeMem,
                "alloc_memory" = object@allocMem
            )
        )
    }
)


## ----
#' @rdname numberOfJars
#' @export
setMethod(
    f = "numberOfJars",
    signature = signature(object = "JvmStats"),
    definition = function(object) {
        return(object@nJars)
    }
)


## ----
#' @param granular
#' Should a \code{list} object be returned with major/minor/build/patch values?
#' Defaults to \code{FALSE}
#'
#' @rdname phgVersion
#' @export
setMethod(
    f = "phgVersion",
    signature = signature(object = "JvmStats"),
    definition = function(object, granular = FALSE) {
        phgVersion <- object@phgVersion

        if (phgVersion == "\u2264 2.4.1") {
            if (granular) {
                return(
                    list(
                        "major" = NA,
                        "minor" = NA,
                        "build" = NA,
                        "patch" = NA,
                        "note"  = "Build is older than v2.4.1"
                    )
                )
            } else {
                return("<= 2.4.1.155")
            }
        } else {
            if (granular) {
                vSplit <- unlist(strsplit(phgVersion, "\\."))
                return(
                    list(
                        "major" = vSplit[1],
                        "minor" = vSplit[2],
                        "patch" = vSplit[3],
                        "build" = vSplit[4]
                    )
                )
            } else {
                return(phgVersion)
            }
        }
    }
)









