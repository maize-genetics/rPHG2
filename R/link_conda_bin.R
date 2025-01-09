## ----
#' Link a Conda environment binary to an R option
#'
#' @description
#' This function checks that a specified binary exists within a Conda
#' environment and then sets an R option pointing to it. By default,
#' the option name is constructed as \code{"phgv2_<bin>_path"}.
#'
#' @param condaPath
#' A character string for the Conda installation path.
#' @param envName
#' A character string for the environment name. Defaults to \code{"phgv2-conda"}.
#' @param bin
#' A character string for the binary to link. Defaults to \code{"agc"}.
#' @param verbose
#' A logical indicating whether to print a message after setting the option.
#' Defaults to \code{TRUE}.
#'
#' @details
#' This function calls \code{\link{validateBinary}} to ensure the binary
#' exists. Then it constructs an option name (\code{"phgv2_<bin>_path"})
#' and sets its value to the binary's path. If \code{verbose} is
#' \code{TRUE}, a status message is shown.
#'
#' @return
#' The function returns nothing. It is called for its side effects, namely
#' setting an R option and optionally printing a message.
#'
#' @examples
#' \dontrun{
#' linkCondaBin("~/miniconda3", "phgv2-conda", "agc")
#' }
#'
#' @export
linkCondaBin <- function(
        condaPath,
        envName = "phgv2-conda",
        bin = "agc",
        verbose = TRUE
) {
    condaPath <- normalizePath(condaPath)
    validateBinary(condaPath, envName, bin)

    optionName <- paste0("phgv2_", bin, "_path")

    binPath <- file.path(condaPath, "envs", envName, "bin", bin)

    do.call(options, setNames(list(binPath), optionName))

    if (verbose) {
        msg <- paste0("Binary: '", bin, "' in Conda environment '", envName, "' added to options(", optionName, ")")
        message(msg)
    }
}


