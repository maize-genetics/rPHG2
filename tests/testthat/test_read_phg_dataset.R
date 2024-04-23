test_that("PHGDataSet reading tests from JVM object", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)

    obsPhgDataSet <- readPhgDataSet(graph)

    expect_true(is(obsPhgDataSet, "PHGDataSet"))
    expect_error(readPhgDataSet(mtcars))
})


