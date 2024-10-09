## ----
#' Initialize JVM and add class path
#'
#' This function is needed for all PHGv2 API-related tasks (e.g., building
#' graph objects from hVCF files or DB connections).
#'
#' @param phgPath
#' Path to PHGv2 lib folder
#' @param check
#' Checks for latest PHGv2 release and correct Java version. Defaults to
#' \code{TRUE}.
#'
#' @export
initPhg <- function(phgPath, check = TRUE) {
    if (!is.character(phgPath) || !dir.exists(phgPath)) {
        rlang::abort("PHG library path ('phgPath') provided does not exist")
    }

    if (!"phg_v2.jar" %in% dir(phgPath)) {
        rlang::abort("Cannot find 'phg_v2.jar' in library path")
    }

    if (isJvmInitialized() && "phg_v2.jar" %in% basename(rJava::.jclassPath())) {
        rlang::abort("PHGv2 JARs already added to class path")
    }

    tryCatch(
        {
            rJava::.jinit()
            rJava::.jaddClassPath(dir(phgPath, full.names = TRUE))
        },
        error = function(e) {
            rlang::abort("Java initialization or class path addition failed", parent = e)
        }
    )

    if (check) {
        jvmStats          <- jvmStats()
        jvmVersion        <- javaVersion(jvmStats)
        currentPhgVersion <- phgVersion(jvmStats)
        latestPhgVersion  <- rPHG2:::getLatestPhgVersion()

        jvmMajor <- as.numeric(unlist(strsplit(jvmVersion, "\\."))[1])

        # Note: I **know** there are easier ways of accomplishing this but I
        #       need to do this 'ad-hoc' for JupyterHub notebooks!
        cliSuccess <- cli::col_green(cli::symbol$tick)
        cliInform  <- cli::col_blue(cli::symbol$info)
        cliWarn    <- cli::col_red(cli::symbol$warning)
        cliBullet  <- cli::symbol$bullet

        jvmMsg <- NULL
        if (jvmMajor < 17) {
            errMsg <- paste0("Your Java version is out of date (", cli::style_bold(jvmVersion), "). Version ", cli::style_bold(cli::symbol$geq, " 17 "), "is needed.")
            rlang::abort(errMsg)
        } else {
            jvmMsg <- paste0("  ", cliBullet, " Java version...: ", cli::style_bold(jvmVersion))
        }

        cliCurrent <- cli::style_bold(currentPhgVersion)
        loadMsg <- paste0(cliSuccess, " PHG JARs added to class path")
        if (currentPhgVersion != latestPhgVersion) {
            cliLatest  <- paste0(cli::style_bold("Latest version: "), cli::style_bold(cli::col_green(latestPhgVersion)))
            phgWarnMsg <- paste0(
                "  ", cli::symbol$line, "\n", "  ", cliWarn, " ", cli::style_bold(cli::symbol$sup_1), "Current version of PHGv2 (", cliCurrent, ") ",
                "is out of date (", cliLatest, ")\n",
                paste0(
                    "    ", cli::col_blue(cli::symbol$arrow_right),
                    " consider updating (",
                    cli::col_blue("https://github.com/maize-genetics/phg_v2/releases/latest"),
                    ")"
                )
            )
            phgVMsg <- paste0("  ", cliBullet, " PHG version....: ", cliCurrent)
            loadMsg <- paste0(loadMsg, cli::style_bold(cli::symbol$sup_1))
        } else {
            phgVMsg <- paste0("  ", cliBullet, " PHG version....: ", cliCurrent)
            phgWarnMsg <- NULL
        }

        msg <- c(loadMsg, jvmMsg, phgVMsg, phgWarnMsg)
        cat(msg, sep = "\n")
    }
}


