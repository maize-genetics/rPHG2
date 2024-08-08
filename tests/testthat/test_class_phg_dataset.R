test_that("PHGDataSet class construction tests", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)


    graph <- buildHaplotypeGraph(locCon)
    pds   <- readPhgDataSet(graph)
    locConOutput <- utils::capture.output(pds)

    expect_equal(length(locConOutput), 6)

    expect_equal(numberOfChromosomes(pds), 2)
    expect_equal(numberOfSamples(pds), 2)
    expect_equal(numberOfRefRanges(pds), 38)
    expect_equal(numberOfHaplotypes(pds), 76)
    expect_true(is(numberOfHaplotypes(pds, byRefRange = TRUE), "data.frame"))
    expect_true(is(plotHaploDist(pds), "ggplot"))


    # plotHaploCounts tests for subsetting and errors

    ## General expectations
    grQuery <- GenomicRanges::GRanges(
        seqnames = c("1", "2"),
        ranges = IRanges::IRanges(start = c(50, 50), end = c(10000, 7500))
    )
    expect_true(is(plotHaploCounts(pds), "ggplot"))
    expect_true(is(plotHaploCounts(pds, gr = grQuery), "ggplot"))

    ## Subset tests
    grQueryWarn1 <- GenomicRanges::GRanges(
        seqnames = c("1", "3"),
        ranges = IRanges::IRanges(start = c(50, 50), end = c(10000, 7500))
    )
    grQueryWarn2 <- GenomicRanges::GRanges(
        seqnames = c("1", "2"),
        ranges = IRanges::IRanges(start = c(50, 5e8), end = c(10000, 7e8))
    )
    grQueryError <- GenomicRanges::GRanges(
        seqnames = c("3", "3"),
        ranges = IRanges::IRanges(start = c(50, 50), end = c(10000, 7500))
    )

    ## Subset test (positive control)
    testPlot <- plotHaploCounts(pds, gr = grQuery)
    ### Expect 2 chromosomes to be represented
    expect_equal(length(unique(testPlot$data$seqnames)), 2)
    ### Expect 2 'facets' to be deployed (e.g. 2 correct queries)
    expect_equal(length(unique(testPlot$data$sub_id)), 2)
    ### expect 8 observations to overlap with query
    expect_equal(length(testPlot$data$rr_id), 8)

    ## Subset test (warn 1)
    testPlot <- plotHaploCounts(pds, gr = grQueryWarn1)
    ### Expect 1 chromosome to be represented
    expect_equal(length(unique(testPlot$data$seqnames)), 1)
    ### Expect 1 'facet' to be deployed (e.g. 2 correct queries)
    expect_equal(length(unique(testPlot$data$sub_id)), 1)
    ### expect 4 observations to overlap with successful query
    expect_equal(length(testPlot$data$rr_id), 4)

    ## Subset test (warn 2)
    testPlot <- plotHaploCounts(pds, gr = grQueryWarn2)
    ### Expect 1 chromosome to be represented
    expect_equal(length(unique(testPlot$data$seqnames)), 1)
    ### Expect 1 'facet' to be deployed (e.g. 2 correct queries)
    expect_equal(length(unique(testPlot$data$sub_id)), 1)
    ### expect 4 observations to overlap with successful query
    expect_equal(length(testPlot$data$rr_id), 4)

    ## General errors
    expect_error(plotHaploCounts(pds, gr = mtcars))
    expect_error(plotHaploCounts(pds, gr = grQueryError))
})


