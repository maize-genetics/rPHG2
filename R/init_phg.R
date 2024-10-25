## ----
#' Initialize JVM and add class path
#'
#' This function is needed for all PHGv2 API-related tasks (e.g., building
#' graph objects from hVCF files or DB connections).
#'
#' @param phgPath
#' Path to PHGv2 lib folder
#' @param check
#' Checks for latest PHGv2 release. This will need a working internet
#' connection. Defaults to \code{TRUE}.
#' @param verbose
#' Should check messages be printed to console? Defaults to \code{TRUE}
#'
#' @export
initPhg <- function(phgPath, check = TRUE, verbose = TRUE) {
    # Set up CLI message components
    #   Note: I **know** there are easier ways of accomplishing this but I
    #         need to do this 'ad-hoc' for JupyterHub notebooks!
    cliSuccess <- cli::col_green(cli::symbol$tick)
    cliInform  <- cli::col_blue(cli::symbol$info)
    cliNote    <- cli::col_red(cli::symbol$warning)
    cliWarn    <- cli::col_red(cli::symbol$cross)
    cliBullet  <- cli::symbol$bullet

    if (!is.character(phgPath) || !dir.exists(phgPath)) {
        rlang::abort("PHG library path ('phgPath') provided does not exist")
    }

    if (!"phg_v2.jar" %in% dir(phgPath)) {
        rlang::abort("Cannot find 'phg_v2.jar' in library path")
    }

    # Check if JVM is already initialized - if it is, show warning message and
    # move on
    loadMsg <- paste0(cliSuccess, " PHG JARs added to class path")
    if (isJvmInitialized() && "phg_v2.jar" %in% basename(rJava::.jclassPath())) {
        loadMsg <- paste0(cliWarn, " PHGv2 JARs already added to class path - skipping...")
    } else {
        tryCatch(
            {
                rJava::.jinit()
                rJava::.jaddClassPath(dir(phgPath, full.names = TRUE))
            },
            error = function(e) {
                rlang::abort("Java initialization or class path addition failed", parent = e)
            }
        )
    }

    # Get version stats
    jvmStats   <- jvmStats()

    # Get PHG version messages / set up load message
    currentPhgVersion <- phgVersion(jvmStats)
    cliCurrent        <- cli::style_bold(currentPhgVersion)
    phgMsg            <- paste0("  ", cliBullet, " PHG version....: ", cliCurrent)
    phgWarnMsg        <- NULL

    # Java version check for >= 17 / message setup
    jvmVersion <- javaVersion(jvmStats)
    jvmMajor   <- as.numeric(unlist(strsplit(jvmVersion, "\\."))[1])
    jvmMsg     <- paste0("  ", cliBullet, " Java version...: ", cli::style_bold(jvmVersion))
    if (jvmMajor < 17) {
        errMsg <- paste0(
            "Your Java version is out of date (", cli::style_bold(jvmVersion), "). Version ",
            cli::style_bold(cli::symbol$geq, " 17 "), "is needed."
        )
        rlang::abort(errMsg)
    }

    # Latest PHG version check
    #   Note: based on 'check' parameter since this is internet-dependent
    if (check) {
        latestPhgVersion  <- getLatestPhgVersion() # internet needed
        if (currentPhgVersion != latestPhgVersion) {
            cliLatest  <- paste0(cli::style_bold("Latest version: "), cli::style_bold(cli::col_green(latestPhgVersion)))
            phgWarnMsg <- sprintf(
                "  %s\n  %s %sCurrent version of PHGv2 (%s) is out of date (%s)\n    %s consider updating (%s)",
                cli::symbol$line,
                cliNote,
                cli::style_bold(cli::symbol$sup_1),
                cliCurrent,
                cliLatest,
                cli::col_blue(cli::symbol$arrow_right),
                cli::col_blue("https://github.com/maize-genetics/phg_v2/releases/latest")
            )
            loadMsg <- paste0(loadMsg, cli::style_bold(cli::symbol$sup_1))
        }

    }

    if (verbose) {
        msg <- c(loadMsg, jvmMsg, phgMsg, phgWarnMsg)
        cat(msg, sep = "\n")
    }
}


