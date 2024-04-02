test_that("Local connection tests", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)

    locConOutput <- utils::capture.output(locCon)

    expect_equal(length(locConOutput), 3)

})

