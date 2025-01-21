## ----
# Convert a FASTA character vector to a DNAStringSet object
#
# @description
# This function processes a character vector representing a FASTA file and
# converts it into a `DNAStringSet` object, where each sequence is named
# based on the corresponding FASTA header.
#
# @param faSeq
# A character vector representing a FASTA file. The headers should
# start with `>` and be followed by sequence lines.
#
# @return
# A `DNAStringSet` object containing the sequences, with names
# extracted from the headers.
#
# @examples
# library(Biostrings)
# faSeq <- c(
#   ">chr1 sampleName=A188", "ATCGATACGATCG", "ATCGATTTGATCG", "ATGGGTACCCCCG",
#   ">chr2 sampleName=A188", "ATCGATAGGGTCG", "ATCGTTTCGATCG", "AAAAATACGATCG"
# )
# dnaSet <- rawFastaToBioString(faSeq)
# dnaSet
#
# @export
rawFastaToBioString <- function(faSeq) {
    # Find the indices of header lines (lines starting with '>')
    headerIndices <- grep("^>", faSeq)

    # Extract the headers (removing the '>' symbol)
    sequenceNames <- sub("^>", "", faSeq[headerIndices])
    if (length(sequenceNames) == 0) {
        sequenceNames <- NULL
    }

    # Determine the ranges for sequences corresponding to each header
    startIndices <- headerIndices + 1
    endIndices <- c(headerIndices[-1] - 1, length(faSeq))

    # Extract sequences, ensuring empty sequences are handled
    contigSequences <- vapply(seq_along(startIndices), function(i) {
        if (startIndices[i] > endIndices[i]) {
            # No sequence lines present
            return("")
        } else {
            # Concatenate sequence lines
            return(paste(faSeq[startIndices[i]:endIndices[i]], collapse = ""))
        }
    }, character(1)) # Specify the output type and length as a character vector of length 1

    # Create a DNAStringSet object
    dnaStringSet <- Biostrings::DNAStringSet(contigSequences)

    # Assign the headers as the names of the sequences
    names(dnaStringSet) <- sequenceNames

    return(dnaStringSet)
}


## ----
# Run AGC commands via a system call
#
# @description
# This function builds a command-line call to an AGC binary and runs
# it. It returns any output from the command. If a path or command is
# invalid, an error or warning is raised.
#
# @param agcPath
# A character string with the path to the AGC file. If it is not found, an
# error is raised.
# @param command
# A character string with the AGC command to run. Valid values are
# \code{c("listset", "getctg", "getset")}.
# @param argV
# A character vector of extra arguments for AGC. Defaults to \code{NULL}.
#
# @details
# This function first checks if \code{agcPath} exists. If not, an error
# is raised. It then checks the \code{"agc_path"} option, which should
# point to the AGC binary. If unset, an error is raised. Finally, it
# calls \code{system2()} with the constructed command and returns
# the output.
#
# @return
# A character vector of the AGC command output. If an error
# occurs, a descriptive message is raised.
agcCore <- function(
        agcPath,
        command = c("listset", "getctg", "getset"),
        argV = NULL
) {
    if (!file.exists(agcPath)) {
        rlang::abort("Path to AGC file cannot be found")
    }

    rlang::arg_match(command)

    agcBinPath <- getOption("phgv2_agc_path")
    if (is.null(agcBinPath)) {
        rlang::abort("Cannot find binary path to AGC")
    }
    agcBinPath <- normalizePath(agcBinPath)

    if (command == "getctg" && is.null(argV)) {
        rlang::abort("'getctg' command needs coordinate locations")
    }

    argVString <- c(
        command,
        agcPath,
        argV
    )

    output <- system2(agcBinPath, args = argVString, stdout = TRUE)
    if (length(output) == 0) {
        return(NULL)
    } else {
        return(output)
    }
}


## ----
# Construct an AGC query from haplotype metadata
#
# @description
# This function reads metadata from the PDS object and returns an
# AGC query string of the form "contig@sample:start-end".
#
# @param pds
# A PDS object containing haplotype metadata.
# @param h
# A value specifying the haplotype ID.
# @param pad
# An integer for expanding the query range; default 0.
#
# @details
# Reads positional (hap_id positions) and metadata (hap_id info)
# to build a string suitable for AGC. The positions are adjusted
# by the 'pad' value.
#
# **NOTE**: we are not checking for end-boundary conditions (e.g., 'pad+')
# since the AGC program already has convenient checks for these cases
#
# @return
# A character string for an AGC query.
genHapIdAgcQuery <- function(pds, h, pad = 0) {
    hPos <- readHapIdPosMetaData(pds)
    hMet <- readHapIdMetaData(pds)

    resPos <- hPos[hPos$hap_id == h, ]
    resMet <- hMet[hMet$hap_id == h, ]

    # Check for negative bounds
    resPosStart <- max(0, resPos$start - 1 - pad)

    resAgc <- paste0(
        resPos$contig_start, "@", resMet$sample_name, ":",
        resPosStart, "-", resPos$end - 1 + pad
    )

    return(resAgc)
}


## ----
# Validate a local Conda environment
#
# @description
# This function checks if a local Conda environment at \code{condaPath}
# is valid. It verifies the presence of required subdirectories
# (\code{"conda-meta"}, \code{"envs"}, \code{"bin"}) and checks if a
# specified environment named \code{envName} exists.
#
# @param condaPath
# A character string specifying the path to the Conda installation. Must be
# valid.
# @param envName
# A character string for the Conda environment name. Defaults to
# \code{"phgv2-conda"}.
#
# @details
# If any required subdirectories are missing or the environment does not
# exist, an error is raised via \code{rlang::abort}.
#
# @return
# This function is called for its side effects. If successful, it returns
# nothing. Otherwise, an error is raised.
validateCondaEnv <- function(condaPath, envName = "phgv2-conda") {
    condaPath <- normalizePath(condaPath, mustWork = TRUE)

    # Subdir checks - more direct directory checks
    expectedDirs <- c("conda-meta", "envs", "bin")
    condaSubDirs <- list.dirs(condaPath, full.names = TRUE, recursive = FALSE)
    condaSubDirs <- basename(condaSubDirs)  # Get directory names only

    missingDirs <- setdiff(expectedDirs, condaSubDirs)
    if (length(missingDirs) > 0) {
        rlang::abort("Missing expected subdirectories for valid Conda installation")
    }

    # Conda environment name check
    envPath <- file.path(condaPath, "envs", envName)
    if (!dir.exists(envPath)) {
        rlang::abort(paste0("Conda environment '", envName, "' does not exist at: ", envPath))
    }
}


## ----
# Validate the existence of a binary in a Conda environment
#
# @description
# This function first validates a Conda environment by calling
# \code{\link{validateCondaEnv}}. Then it checks whether the specified
# binary \code{bin} exists in the environment.
#
# @param condaPath
# A character string specifying the path to the Conda installation.
# @param envName
# A character string for the Conda environment name. Defaults to
# \code{"phgv2-conda"}.
# @param bin
# A character string for the binary name to check. Defaults to \code{"agc"}.
#
# @details
# If the binary is not found, an error is raised via \code{rlang::abort}.
#
# @return
# This function is called for its side effects. It returns nothing if
# successful. Otherwise, an error is raised.
validateBinary <- function(condaPath, envName = "phgv2-conda", bin = "agc") {

    validateCondaEnv(condaPath, envName)

    if (tolower(Sys.info()[["sysname"]]) == "windows") {
        bin <- paste0(bin, ".exe")
    }

    if (!file.exists(file.path(condaPath, "envs", envName, "bin", bin))) {
        rlang::abort(paste0("Binary '", bin, "' does not exist in Conda environment: ", envName))
    }
}


