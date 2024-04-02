test_that("BrAPI utitlity tests", {
    negControl1 <- "https://www.google.com"
    negControl2 <- "does-not-exist"
    posControl1 <- "https://test-server.brapi.org"
    posControl2 <- file.path(posControl1, "brapi/v2")
    posControl3 <- file.path(posControl2, BRAPI_ENDPOINTS$SERVER_INFO)
    posControl4 <- "https://httpstat.us"

    # brapiEndpointExists
    expect_true(brapiEndpointExists(posControl2))
    expect_false(brapiEndpointExists(negControl2))
    expect_false(brapiEndpointExists(negControl2))

    # brapiMetadata
    expect_true(
        is(
            object = brapiMetadata(
                PHGServerCon(posControl1),
                BRAPI_ENDPOINTS$SAMPLES
            ),
            "list"
        )
    )

    # httResp
    expect_true(is(httpResp(file.path(posControl4, 200)), "list"))
    for (i in seq(200, 500, 100)) {
        expect_equal(httpResp(file.path(posControl4, i))$status, i)
    }

    # jsonToTibble
    expect_true(
        is(
            object = jsonToTibble(
                PHGServerCon(posControl1),
                BRAPI_ENDPOINTS$SAMPLES
            ),
            "tbl"
        )
    )

    # parseJSON
    expect_error(parseJSON(negControl1))
    expect_error(parseJSON(negControl2))
    expect_true(is(parseJSON(posControl3), "list"))
    expect_message(parseJSON(posControl3, verbose = TRUE))

    obsUrl  <- "https://filesamples.com/samples/code/json/sample1.json"
    obsJson <- rPHG2:::parseJSON(obsUrl)
    expect_true(is(obsJson, "list"))
})



