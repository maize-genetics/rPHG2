test_that("Hap ID positional metadata reading tests from JVM", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)

    obsHapIdPosMetaData <- readHapIdPosMetaData(graph)

    expect_error(readHapIdPosMetaData(mtcars))
    expect_true(is(obsHapIdPosMetaData, "data.frame"))
    expect_equal(dim(obsHapIdPosMetaData), c(76, 5))
    expect_equal(
        colnames(obsHapIdPosMetaData),
        c("hap_id", "contig_start", "contig_end", "start", "end")
    )
})
