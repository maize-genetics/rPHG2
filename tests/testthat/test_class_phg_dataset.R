test_that("PHGDataSet class construction tests", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)


    graph <- buildHaplotypeGraph(locCon)
    pds   <- readPhgDataSet(graph)
    locConOutput <- utils::capture.output(pds)

    expect_equal(length(locConOutput), 6)
})


