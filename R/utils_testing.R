## ----
# Create a mock Conda installation
#
# @description
# This function sets up a mock Conda installation directory
# structure, including a specified environment and its
# subdirectories.
#
# @param baseDir
# Base direcory
# @param condaEnvName
# The name of the environment. Defaults to "phgv2-conda".
#
# @return
# No return value. Directories are created as a side effect.
createMockCondaInstallation <- function(
    baseDir = "mock_conda",
    condaEnvName = "phgv2-conda",
    verbose = FALSE
) {
    dir.create(baseDir, showWarnings = FALSE)

    binDir <- file.path(baseDir, "bin")
    dir.create(binDir, showWarnings = FALSE)

    envsDir <- file.path(baseDir, "envs")
    dir.create(envsDir, showWarnings = FALSE)

    metaDir <- file.path(baseDir, "conda-meta")
    dir.create(metaDir, showWarnings = FALSE)

    envDir <- file.path(envsDir, condaEnvName)
    dir.create(envDir, showWarnings = FALSE)

    envBinDir <- file.path(envDir, "bin")
    envLibDir <- file.path(envDir, "lib")
    envIncDir <- file.path(envDir, "include")
    envShrDir <- file.path(envDir, "share")

    dirsToCreate <- c(envBinDir, envLibDir, envIncDir, envShrDir)
    lapply(dirsToCreate, dir.create, showWarnings = FALSE)

    if (verbose) {
        message("Mock conda created at: ", baseDir)
        message("Environment created: ", condaEnvName)
    }
}


## ----
# Download the AGC binary for the current operating system and architecture
#
# @description
# This function detects the OS and, if on macOS, checks whether
# the machine is arm64 (e.g., Apple Silicon) or x86_64 (Intel).
# It then downloads the matching AGC binary from GitHub for
# the specified version (e.g., "0.3.1") and extracts it into
# the specified directory.
#
# @param destDir
# A character string specifying where to save and unpack the AGC binary.
# Defaults to \code{tempdir()}.
# @param version
# A character string specifying the AGC release version (without the leading
# "v").
#
# @details
# This function will create a default directory called \code{agc_bin}. This
# cannot be changed!
#
# @return
# The path to the extracted AGC binary directory, returned invisibly.
downloadAgcBinary <- function(
    destDir,
    version = "3.2.1",
    machine = tolower(Sys.info()[["machine"]]),
    osName  = tolower(Sys.info()[["sysname"]]),
    verbose = TRUE
) {
    # Pre-check platform → suffix mapping
    fileMap <- list(
        "win_x86_64"    = "_x64_windows.zip",
        "linux_x86_64"  = "_x64_linux.tar.gz",
        "linux_aarch64" = "_arm_linux.tar.gz",
        "darwin_arm64"  = "_m1_mac.tar.gz",
        "darwin_x86_64" = "_x64_mac.tar.gz"
    )
    osCpu  <- paste(osName, machine, sep = "_")
    suffix <- fileMap[[osCpu]]
    if (is.null(suffix)) {
        rlang::abort("Unsupported chip architecture for this OS: ", osCpu)
    }

    # Normalize the destination directory
    destDir <- normalizePath(destDir, mustWork = TRUE)

    # Prepare file name: only major & minor version for the AGC release
    optVer  <- sub("\\.[^.]*$", "", version)  # e.g., "3.2.1" -> "3.2"
    fileName <- paste0("agc-", optVer, suffix)

    # Build the download URL
    baseUrl     <- paste0("https://github.com/refresh-bio/agc/releases/download/v", version, "/")
    downloadUrl <- paste0(baseUrl, fileName)
    destFile    <- file.path(destDir, fileName)

    # Download
    if (verbose) {
        message("Downloading: ", downloadUrl)
    }
    utils::download.file(downloadUrl, destFile, mode = "wb", quiet = !verbose)

    # Extract archive
    if (endsWith(fileName, ".zip")) {
        utils::unzip(destFile, exdir = destDir)
    } else {
        utils::untar(destFile, exdir = destDir)
    }

    # Clean up the compressed file
    unlink(destFile)

    # Rename extracted directory
    extractedDir <- sub("\\.(tar\\.gz|zip)$", "", fileName)
    oldPath      <- file.path(destDir, extractedDir)
    newPath      <- file.path(destDir, "agc_bin")
    if (dir.exists(newPath)) {
        unlink(newPath, recursive = TRUE)
    }
    file.rename(oldPath, newPath)

    if (verbose) {
        message("AGC binary downloaded and unpacked at: ", newPath)
    }
    invisible(newPath)
}


## ----
# Create an AGC archive from FASTA files
#
# @description
# Calls the AGC binary to compress multiple FASTA files
# into an AGC archive. The AGC path is read from
# `options("phgv2_agc_path")`.
#
# @param fastas
# Vector of FASTA file paths (.fa or .fasta).
# @param agcId
# Character string for the output AGC file name.
#
# @return
# Integer exit status from the `system2()` call.
makeAgc <- function(fastas, agcId) {
    # Get path to AGC binary from options
    agcBinPath <- getOption("phgv2_agc_path")
    if (is.null(agcBinPath)) {
        rlang::abort("AGC binary path not found.")
    }
    agcBinPath <- normalizePath(agcBinPath)

    # Verify that all inputs are FASTA files
    if (!all(grepl("\\.fa$|\\.fasta$", fastas))) {
        rlang::abort(
            "All input files must be .fa or .fasta"
        )
    }

    # Build arguments for system2
    agcArgs <- c(
        "create",
        paste(fastas, collapse = " "),
        ">",
        agcId
    )

    # Run the AGC command
    system2(agcBinPath, agcArgs)
}


