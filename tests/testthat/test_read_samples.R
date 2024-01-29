test_that("Sample reading tests", {
    testUrl <- "test-server.brapi.org"

    phgSrvCon <- PHGServerCon(testUrl)

    brapiMeta <- brapiMetadata(phgSrvCon, BRAPI_ENDPOINTS$SAMPLES)
    nSamples  <- brapiMeta$pagination$totalCount


    expect_true(is(readSamples(phgSrvCon), "character"))
    expect_equal(length(readSamples(phgSrvCon)), nSamples)
})


