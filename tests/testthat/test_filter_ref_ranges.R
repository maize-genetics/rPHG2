test_that("Sample filtering tests", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)
    pds         <- readPhgDataSet(graph)

    gr01 <- GenomicRanges::GRanges(
        seqnames = c("1"),
        ranges = IRanges::IRanges(
            start = 2,
            end   = 10000
        )
    )

    gr02 <- GenomicRanges::GRanges(
        seqnames = c("1", "2", "3"),
        ranges = IRanges::IRanges(
            start = c(2, 100, 12),
            end   = c(10000, 4500, 150000)
        )
    )

    gr03 <- GenomicRanges::GRanges(
        seqnames = "3",
        ranges = IRanges::IRanges(
            start = 12,
            end   = 150000
        )
    )

    obsFiltPds01 <- filterRefRanges(pds, gr01)
    obsFiltPds02 <- filterRefRanges(pds, gr02)

    expect_true(is(obsFiltPds01, "PHGDataSet"))
    expect_equal(length(readRefRanges(obsFiltPds01)), 4)
    expect_equal(dim(readHapIds(obsFiltPds01)), c(2, 4))
    expect_equal(length(readRefRanges(obsFiltPds02)), 6)
    expect_equal(dim(readHapIds(obsFiltPds02)), c(2, 6))
    expect_error(filterRefRanges(pds, gr03))

})


