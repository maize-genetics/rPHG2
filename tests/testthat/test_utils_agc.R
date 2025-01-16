test_that("rawFastaToBioString works as expected", {

    # Test: Single sequence in FASTA
    test_that("Single sequence FASTA is parsed correctly", {
        fasta <- c(">seq1", "ATGCGTACGTAGCTAGCTAG", "TACGATCG")
        result <- rawFastaToBioString(fasta)
        expected <- Biostrings::DNAStringSet(c("ATGCGTACGTAGCTAGCTAGTACGATCG"))
        names(expected) <- c("seq1")
        expect_equal(result, expected)
    })

    # Test: Multiple sequences in FASTA
    test_that("Multiple sequences FASTA is parsed correctly", {
        fasta <- c(
            ">seq1", "ATGCGTACGTAGCTAGCTAG",
            ">seq2", "CGTACGATC", "GTACTG",
            ">seq3", "TAGCTGAC"
        )
        result <- rawFastaToBioString(fasta)
        expected <- Biostrings::DNAStringSet(c(
            "ATGCGTACGTAGCTAGCTAG",
            "CGTACGATCGTACTG",
            "TAGCTGAC"
        ))
        names(expected) <- c("seq1", "seq2", "seq3")
        expect_equal(result, expected)
    })

    # Test: FASTA with empty sequence
    test_that("FASTA with empty sequence is handled", {
        fasta <- c(
            ">seq1", "ATGCGTACG",
            ">seq2",
            ">seq3", "TAGCTGAC"
        )
        result <- rawFastaToBioString(fasta)
        expected <- Biostrings::DNAStringSet(c(
            "ATGCGTACG",
            "",
            "TAGCTGAC"
        ))
        names(expected) <- c("seq1", "seq2", "seq3")
        expect_equal(result, expected)
    })

    # Test: FASTA with only headers
    test_that("FASTA with only headers returns empty sequences", {
        fasta <- c(">seq1", ">seq2", ">seq3")
        result <- rawFastaToBioString(fasta)
        expected <- Biostrings::DNAStringSet(c("", "", ""))
        names(expected) <- c("seq1", "seq2", "seq3")
        expect_equal(result, expected)
    })

    # Test: Empty FASTA input
    test_that("Empty FASTA input returns empty DNAStringSet", {
        fasta <- character(0)
        result <- rawFastaToBioString(fasta)
        expected <- Biostrings::DNAStringSet()
        expect_equal(result, expected)
    })

    # Test: FASTA with malformed headers
    test_that("FASTA with malformed headers is handled gracefully", {
        fasta <- c("seq1", "ATGCGTACG", ">seq2", "TAGCTGAC")
        # Will exclude first entry with malformation
        expect_true(length(rawFastaToBioString(fasta)) == 1,)
    })
})


test_that("agcCore works as expected", {
    # Test: Path to AGC file does not exist
    test_that("agcCore errors when AGC file path does not exist", {
        agcPath <- tempfile()
        expect_error(rPHG2:::agcCore(agcPath), "Path to AGC file cannot be found")
    })

    # Test: Invalid command argument
    test_that("agcCore errors on invalid command", {
        agcPath <- tempfile()
        file.create(agcPath) # Create a temporary mock file
        expect_error(agcCore(agcPath, command = "invalidCommand"), "must be one of")
    })

    # Test: Missing binary path in options
    test_that("agcCore errors when binary path is missing", {
        agcPath <- tempfile()
        file.create(agcPath)
        options(phgv2_agc_path = NULL) # Remove binary path
        expect_error(agcCore(agcPath), "Cannot find binary path to AGC")
    })

    # Test: Successful execution
    test_that("agcCore runs successfully with valid inputs", {
        options(phgv2_agc_path = agcVanillaPath)
        result <- agcCore(agcFilePath, command = "listset")
        expect_equal(result, c("LineA", "LineB"))
    })

    # Test: Errors
    test_that("agcCore throws correct flag exceptions", {
        options(phgv2_agc_path = agcVanillaPath)
        expect_error(agcCore(agcFilePath, command = "getctg"))
        expect_equal(agcCore(agcFilePath, command = "getctg", "chr20@B73"), NULL)
        expect_equal(agcCore(agcFilePath, command = "getset", "R2D2"), NULL)
    })

})


test_that("genHapIdAgcQuery works as expected", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)
    pds         <- readPhgDataSet(graph)
    hapIds      <- readHapIds(pds)
    hQuery      <- hapIds[1, 1]
    hSample     <- gsub("_G1", "", rownames(hapIds)[1])
    hRefRng     <- colnames(hapIds)[1]
    hContig     <- gsub(":.*$", "", hRefRng)
    hStart      <- as.numeric(gsub("^.:|-.*$", "", hRefRng))
    hEnd        <- as.numeric(gsub("^.*-", "", hRefRng))

    expRes <- paste0(hContig, "@", hSample, ":", hStart - 1, "-", hEnd - 1)
    obsRes <- rPHG2:::genHapIdAgcQuery(pds, hQuery)
    expect_equal(expRes, obsRes)

    pad <- 500
    expRes <- paste0(hContig, "@", hSample, ":", 0, "-", hEnd + pad - 1)
    obsRes <- rPHG2:::genHapIdAgcQuery(pds, hQuery, pad = pad)
    expect_equal(expRes, obsRes)
})


test_that("validateCondaEnv works as expected", {
    expect_equal(validateCondaEnv(agcCondaPath, envName = "phgv2-conda"), NULL)
    expect_error(validateCondaEnv(tempdir(), envName = "phgv2-conda"))
    expect_error(validateCondaEnv(agcCondaPath, envName = "phgv222-condaaa"))
})


test_that("validateBinary works as expected", {
    expect_equal(validateBinary(agcCondaPath, envName = "phgv2-conda"), NULL)
    expect_error(validateBinary(agcCondaPath, envName = "phgv2-condaaa"))
    expect_error(validateBinary(agcCondaPath, envName = "phgv2-conda", bin = "fail"), NULL)
})


