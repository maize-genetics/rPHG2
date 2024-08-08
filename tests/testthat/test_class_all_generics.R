test_that("Generic methods for classes tests from BrAPI/Ktor server", {
    brapiUrl <- "https://test-server.brapi.org"

    phgSrvCon <- PHGServerCon(brapiUrl)

    expect_true(is(brapiURL(phgSrvCon), "character"))
    expect_true(is(brapiVersion(phgSrvCon), "character"))
    expect_true(is(host(phgSrvCon), "character"))
    expect_true(is(httProtocol(phgSrvCon), "character"))
    expect_true(is(phgType(phgSrvCon), "character"))
    expect_true(is(port(phgSrvCon), "numeric"))
    expect_true(is(readSamples(phgSrvCon), "character"))
    expect_true(is(serverInfo(phgSrvCon), "tbl"))
})


test_that("Generic methods for classes tests from JVM", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)


    expect_true(is(javaMemoryAddress(graph), "character"))
    expect_true(is(numberOfChromosomes(graph), "numeric"))
    expect_true(is(numberOfRefRanges(graph), "numeric"))
    expect_equal(numberOfRefRanges(graph), 38)
    expect_true(is(numberOfSamples(graph), "numeric"))
    expect_equal(numberOfSamples(graph), 2)

})
