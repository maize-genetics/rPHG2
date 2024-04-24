test_that("Hap ID metadata reading tests from JVM", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)

    obsHapIdMetaData <- readHapIdMetaData(graph)

    expect_error(readHapIdMetaData(mtcars))
    expect_true(is(obsHapIdMetaData, "data.frame"))
    expect_equal(dim(obsHapIdMetaData), c(76, 6))
    expect_equal(
        colnames(obsHapIdMetaData),
        c("hap_id", "sample_name", "description", "source", "checksum", "ref_range_hash")
    )
})


test_that("Hap ID metadata reading tests from PHGDataSet", {
    hVcfFileDir <- system.file("extdata", package = "rPHG2")
    hVcfFiles   <- list.files(hVcfFileDir, pattern = ".h.vcf$", full.names = TRUE)
    locCon      <- PHGLocalCon(hVcfFiles)
    graph       <- buildHaplotypeGraph(locCon)
    pds         <- readPhgDataSet(graph)

    obsHapIdMetaData <- readHapIdMetaData(pds)
    expect_true(is(obsHapIdMetaData, "data.frame"))
    expect_equal(dim(obsHapIdMetaData), c(76, 6))
    expect_equal(
        colnames(obsHapIdMetaData),
        c("hap_id", "sample_name", "description", "source", "checksum", "ref_range_hash")
    )
})


