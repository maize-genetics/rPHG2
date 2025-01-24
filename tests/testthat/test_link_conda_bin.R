test_that("linkCondaBin works as expected", {
    options("phgv2_agc_path" = NULL)
    expect_true(is.null(options()$phgv2_agc_path))
    linkCondaBin(agcCondaPath, verbose = FALSE)
    expect_true(!is.null(options()$phgv2_agc_path))
    expect_message(linkCondaBin(agcCondaPath, verbose = TRUE))
})


