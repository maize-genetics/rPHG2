test_that("Ref range reading tests from JVM object", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)

    obsRefRanges <- readRefRanges(graph)
    obsRefRangesDf <- as.data.frame(obsRefRanges, char)

    expect_true(is(obsRefRanges, "GRanges"))
    expect_equal(nrow(obsRefRangesDf), 38)
    expect_equal(levels(obsRefRangesDf$seqnames), c("1", "2"))
    expect_equal(
        colnames(obsRefRangesDf),
        c("seqnames", "start", "end", "width", "strand")
    )
})


