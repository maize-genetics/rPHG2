test_that("Generic methods for classes tests", {
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


