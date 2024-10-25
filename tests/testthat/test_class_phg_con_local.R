test_that("General local connection tests", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)

    expect_error(PHGLocalCon(mtcars))
    expect_error(PHGLocalCon(tempdir()))
    expect_error(PHGLocalCon(tempfile()))

    locConOutput <- utils::capture.output(locCon)
    expect_equal(length(locConOutput), 3)

})


test_that("Local connection from directory", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    locCon      <- PHGLocalCon(hVcfFileDir)
    locConOutput <- utils::capture.output(locCon)
    expect_equal(length(locConOutput), 3)

})
