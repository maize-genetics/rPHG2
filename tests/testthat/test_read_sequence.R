test_that("readSequenceFromPds works as expected", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles, dbUri = phgLibDir)
    graph       <- buildHaplotypeGraph(locCon)
    pds         <- readPhgDataSet(graph)
    hapIds      <- readHapIds(pds)

    options("phgv2_agc_path" = agcVanillaPath)

    testRefRange <- colnames(hapIds)[1]
    obsRes <- readSequenceFromPds(pds, rrId = testRefRange)
    expect_true(is(obsRes, "DNAStringSet"))

    expSeqLA <- "GCGCGGGGACCGAGAAACCCGGCGG"
    obsSeqLA <- as.character(obsRes[[1]][seq_len(nchar(expSeqLA))])
    expSeqLB <- "GCGCGGGGACCGTGAAACCCGGCGG"
    obsSeqLB <- as.character(obsRes[[2]][seq_len(nchar(expSeqLB))])
    expect_equal(obsSeqLA, expSeqLA)
    expect_equal(obsSeqLB, expSeqLB)

    testHapId <- hapIds[1, 1]
    obsRes <- readSequenceFromPds(pds, hapId = testHapId)
    expect_true(is(obsRes, "DNAStringSet"))
    expect_true(length(obsRes) == 1)
    obsSeqLA <- as.character(obsRes[[1]][seq_len(nchar(expSeqLA))])
    expect_equal(obsSeqLA, expSeqLA)

    # Test for 'rrId' to NULL coercion
    obsRes <- readSequenceFromPds(pds, hapId = testHapId, rrId = testRefRange)
    expect_true(is(obsRes, "DNAStringSet"))
    expect_true(length(obsRes) == 1)
    obsSeqLA <- as.character(obsRes[[1]][seq_len(nchar(expSeqLA))])
    expect_equal(obsSeqLA, expSeqLA)

    # Test for padding (start - inf)
    obsRes <- rPHG2:::readSequenceFromPds(pds, hapId = testHapId, pad = 500)
    expect_true(length(obsRes[[1]]) == 1500)
    obsRes <- rPHG2:::readSequenceFromPds(pds, rrId = testRefRange, pad = 500)
    expect_true(length(obsRes[[1]]) == 1500)
    expect_true(length(obsRes[[2]]) == 1500)

    # Test for padding (inf - inf)
    testHapId <- hapIds[1, 2]
    obsRes <- rPHG2:::readSequenceFromPds(pds, hapId = testHapId, pad = 500)
    expect_true(length(obsRes[[1]]) == 5500) # 500 + 4500 + 500
    testRefRange <- colnames(hapIds)[2]
    obsRes <- rPHG2:::readSequenceFromPds(pds, rrId = testRefRange, pad = 500)
    expect_true(length(obsRes[[1]]) == 5500) # 500 + 4500 + 500
    expect_true(length(obsRes[[2]]) == 5500) # 500 + 4500 + 500

    # Test for padding (inf - end)
    testHapId <- hapIds[1, 38]
    obsRes <- rPHG2:::readSequenceFromPds(pds, hapId = testHapId, pad = 500)
    expect_true(length(obsRes[[1]]) == 1800) # 500 + 800 + 500
    testRefRange <- colnames(hapIds)[38]
    obsRes <- rPHG2:::readSequenceFromPds(pds, rrId = testRefRange, pad = 500)
    expect_true(length(obsRes[[1]]) == 1800) # 500 + 800 + 500
    expect_true(length(obsRes[[2]]) == 1800) # 500 + 800 + 500
})


