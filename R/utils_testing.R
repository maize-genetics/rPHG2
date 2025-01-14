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


