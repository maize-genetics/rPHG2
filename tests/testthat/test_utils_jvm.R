test_that("JVM utility tests", {
    negControl1 <- rJava::.jarray(c(1, 2, 3))
    expect_error(
        kotlinListToRDataFrame(negControl1),
        regexp = "Object does not have a \'RList\' signature"
    )
    expect_error(
        kotlin2DArrayToRMatrix(negControl1),
        regexp = "Object does not have a \'MatrixWithNames\' signature"
    )

    expect_silent(initPhg(phgLibPath, verbose = FALSE))
})


