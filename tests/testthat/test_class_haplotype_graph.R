test_that("HaplotypeGraph class construction tests", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    testUrl     <- "test-server.brapi.org"
    srvCon      <- PHGServerCon(testUrl)

    expect_error(
        buildHaplotypeGraph(mtcars),
        regexp = "phgLocalCon object is not of type PHGLocalCon"
    )

    expect_error(
        buildHaplotypeGraph(srvCon),
        regexp = "phgLocalCon object is not of type PHGLocalCon"
    )

    locConBad <- locCon
    locConBad@host <- "1"

    expect_error(buildHaplotypeGraph(locConBad))


    graph <- buildHaplotypeGraph(locCon)
    locConOutput <- utils::capture.output(graph)

    expect_equal(length(locConOutput), 4)

})

