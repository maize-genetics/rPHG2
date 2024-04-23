test_that("Sample filtering tests", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)
    pds         <- readPhgDataSet(graph)


    obsFiltPds01 <- filterSamples(pds, "LineB")
    obsFiltPds02 <- filterSamples(pds, c("LineA", "LineX", "LineB"))

    expect_true(is(obsFiltPds01, "PHGDataSet"))
    expect_equal(readSamples(obsFiltPds01), "LineB")
    expect_equal(nrow(readHapIds(obsFiltPds01)), 1)
    expect_equal(readSamples(obsFiltPds02), c("LineA", "LineB"))
    expect_equal(nrow(readHapIds(obsFiltPds02)), 2)
    expect_error(filterSamples(pds, "LineX"))
})


