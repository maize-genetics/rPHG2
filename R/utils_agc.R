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

    # Extract the sequence lines for each contig
    contigSequences <- mapply(
        function(start, end) paste(faSeq[(start + 1):end], collapse = ""),
        start = headerIndices,
        end = c(headerIndices[-1] - 1, length(faSeq)) # Handles the last contig
    )

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

    agcBinPath <- getOption("agc_path")
    if (is.null(agcBinPath)) {
        rlang::abort("Cannot find binary path to AGC")
    }
    agcBinPath <- normalizePath(agcBinPath)

    argV <- c(
        command,
        agcPath,
        argV
    )

    tryCatch(
        {
            output <- system2(agcBinPath, args = argV, stdout = TRUE)
            return(output)
        },
        error = function(err) {
            rlang::abort(paste("An error occurred while running the AGC command:", err$message))
        },
        warning = function(warn) {
            rlang::warn(paste("A warning occurred while running the AGC command:", warn$message))
        }
    )
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
# @return
# A character string for an AGC query.
genHapIdAgcQuery <- function(pds, h, pad = 0) {
    hPos <- readHapIdPosMetaData(pds)
    hMet <- readHapIdMetaData(pds)

    resPos <- hPos[hPos$hap_id == h, ]
    resMet <- hMet[hMet$hap_id == h, ]

    resAgc <- paste0(
        resPos$contig_start, "@", resMet$sample_name, ":",
        resPos$start - 1 - pad, "-", resPos$end - 1 + pad
    )

    return(resAgc)
}


