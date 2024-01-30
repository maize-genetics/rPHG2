test_that("Generic connection object tests", {
    testPhgCon <- methods::new(
        "PHGCon",
        phgType = "local",
        host = "localhost"
    )

    expect_true(is(testPhgCon, "PHGCon"))
    expect_error(
        object = methods::new(
            "PHGCon",
            phgType = "locallll",
            host = "localhost"
        ),
        regexp = "Given PHG connection type is not allowed"
    )

    expect_true(is(host(testPhgCon), "character"))
    expect_equal(host(testPhgCon), "localhost")
    expect_true(is(phgType(testPhgCon), "character"))
    expect_equal(phgType(testPhgCon), "local")
})



