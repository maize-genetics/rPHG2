test_that("Hap ID matrix reading tests from JVM", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)

    obsHapIds <- readHapIds(graph, 2)

    expect_true(is(obsHapIds, "matrix"))
    expect_true(is(obsHapIds[1, 1], "character"))
    expect_equal(dim(obsHapIds), c(2, 38))
})


