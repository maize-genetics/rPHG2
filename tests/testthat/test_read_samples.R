test_that("Sample reading tests from BrAPI/Ktor server", {
    testUrl <- "test-server.brapi.org"

    phgSrvCon <- PHGServerCon(testUrl)

    brapiMeta <- brapiMetadata(phgSrvCon, BRAPI_ENDPOINTS$SAMPLES)
    nSamples  <- brapiMeta$pagination$totalCount


    expect_true(is(readSamples(phgSrvCon), "character"))
    expect_equal(length(readSamples(phgSrvCon)), nSamples)
})


test_that("Sample reading tests from JVM object", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)

    obsSamples <- readSamples(graph)

    expect_true(is(obsSamples, "character"))
    expect_equal(length(obsSamples), 2)
    expect_equal(obsSamples, c("LineA", "LineB"))
})
