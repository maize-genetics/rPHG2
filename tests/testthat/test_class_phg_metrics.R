test_that("PHGMetrics class construction tests", {
    metricDir <- system.file("extdata", package = "rPHG2")
    algnPath  <- system.file("extdata", "toy_anchors_s01.anchorspro", package = "rPHG2")
    gvcfPath  <- system.file("extdata", "toy_gvcf_metrics.tsv", package = "rPHG2")
    hvcfPath  <- system.file("extdata", "LineA.h.vcf", package = "rPHG2")
    algnDf    <- read.table(algnPath, header = TRUE, sep = "\t")

    met <- PHGMetrics(c(algnPath, gvcfPath))

    expect_error(PHGMetrics(mtcars), regexp = "No valid file paths or directories")
    expect_error(PHGMetrics(c("error1", "error2")), regexp = "No valid paths given")
    expect_error(PHGMetrics(hvcfPath), regexp = "No valid gVCF or AnchorWave")
    expect_true(is(met, "PHGMetrics"))
    expect_true(is(PHGMetrics(metricDir), "PHGMetrics"))
    expect_true(is(PHGMetrics(algnPath), "PHGMetrics"))
    expect_true(is(PHGMetrics(gvcfPath), "PHGMetrics"))
})


test_that("PHGMetrics console display tests", {
    metricDir <- system.file("extdata", package = "rPHG2")
    algnPath  <- system.file("extdata", "toy_anchors_s01.anchorspro", package = "rPHG2")
    gvcfPath  <- system.file("extdata", "toy_gvcf_metrics.tsv", package = "rPHG2")
    hvcfPath  <- system.file("extdata", "LineA.h.vcf", package = "rPHG2")
    algnDf    <- read.table(algnPath, header = TRUE, sep = "\t")

    metOut01 <- utils::capture.output(PHGMetrics(algnPath))
    metOut02 <- utils::capture.output(PHGMetrics(gvcfPath))
    metOut03 <- utils::capture.output(PHGMetrics(c(algnPath, gvcfPath)))

    expect_equal(length(metOut01), 4)
    expect_equal(length(metOut02), 4)
    expect_equal(length(metOut03), 7)
})


test_that("PHGMetrics override tests", {
    metricDir <- system.file("extdata", package = "rPHG2")
    algnPath  <- system.file("extdata", "toy_anchors_s01.anchorspro", package = "rPHG2")
    gvcfPath  <- system.file("extdata", "toy_gvcf_metrics.tsv", package = "rPHG2")

    met <- PHGMetrics(c(algnPath, gvcfPath))

    expect_equal(met$toy_gvcf_metrics, "toy_gvcf_metrics")
    expect_equal(met$toy_anchors_s01, "toy_anchors_s01")
    expect_equal(.DollarNames(met, "toy_anchors_s01"), "toy_anchors_s01")
    expect_equal(length(.DollarNames(met, "not_in_object")), 0)
})


test_that("PHGMetrics general metric ID return and update tests", {
    metricDir <- system.file("extdata", package = "rPHG2")
    algnPath  <- system.file("extdata", "toy_anchors_s01.anchorspro", package = "rPHG2")
    gvcfPath  <- system.file("extdata", "toy_gvcf_metrics.tsv", package = "rPHG2")
    hvcfPath  <- system.file("extdata", "LineA.h.vcf", package = "rPHG2")
    algnDf    <- read.table(algnPath, header = TRUE, sep = "\t")

    metBase <- PHGMetrics(c(algnPath, gvcfPath))

    # metricsIds()
    expect_true(is(metricsIds(metBase), "character"))
    expect_equal(metricsIds(metBase), c("toy_anchors_s01", "toy_gvcf_metrics"))
    expect_equal(metricsIds(metBase, type = "align"), "toy_anchors_s01")
    expect_equal(metricsIds(metBase, type = "gvcf"), "toy_gvcf_metrics")
    expect_error(metricsIds(metBase, "invalid"), regexp = "must be one of")

    # metricsIds<-
    metTest <- metBase
    metricsIds(metTest) <- c(metTest$toy_anchors_s01 %T% "new_anchor_id")
    expect_equal(metricsIds(metTest, type = "align"), "new_anchor_id")
    expect_error(metricsIds(metTest) <- "invalid", regexp = "is not a named")
    expect_error(metricsIds(metTest) <- mtcars, regexp = "Elements in vector")
    expect_error(
        object = metricsIds(metTest) <- c(
            metTest$new_anchorr %T% "newer_anchor_id"
        ),
        regexp = "Failed evaluation of"
    )
    expect_error(
        object = metricsIds(metTest) <- c(
            metTest$new_anchor_id %T% "new_id_1",
            metTest$new_anchor_id %T% "new_id_2"
        ),
        regexp = "Duplicated object IDs found"
    )
    expect_error(
        object = metricsIds(metTest) <- c(
            metTest$new_anchor_id %T% "new_id_1",
            metTest$toy_gvcf_metrics %T% "new_id_1"
        ),
        regexp = "Duplicated new IDs found"
    )
    metTest <- metBase
    expect_error(
        object = metricsIds(metTest) <- c("new_anchorrr" = "newer_id"),
        regexp = "No provided IDs found in object"
    )
    expect_error(
        object = metricsIds(metTest) <- c("toy_anchors_s01" = "toy_anchors_s01"),
        regexp = "No IDs changed"
    )
    metTest <- metBase
    metricsIds(metTest) <- c(
        metTest$toy_anchors_s01 %T% "toy_anchors_s01",
        metTest$toy_gvcf_metrics %T% "new_gvcf_id"
    )
    expect_equal(
        object = metricsIds(metTest),
        expected = c("toy_anchors_s01", "new_gvcf_id")
    )
    metTest <- metBase
    metricsIds(metTest) <- c(
        "id_not_in_object" %T% "new_anchor_id",
        metTest$toy_gvcf_metrics %T% "new_gvcf_id"
    )
    expect_equal(
        object = metricsIds(metTest),
        expected = c("toy_anchors_s01", "new_gvcf_id")
    )

    # seqnames
    expect_equal(
        object = seqnames(metBase),
        expected = c(
            "1", "2", "3",
            "Vu01", "Vu02", "Vu03", "Vu04", "Vu05",
            "Vu06", "Vu07", "Vu08", "Vu09", "Vu10",
            "Vu11", "contig_206", "contig_178"
        )
    )

    metTest <- metBase
    expect_error(object = seqnames(metTest) <- data.frame(old_id = "error"))
    expect_error(object = seqnames(metTest) <- c("another", "error"))
    seqnames(metTest) <- data.frame(
        old_id = c("Vu01", "Vu03", "1"),
        new_id = c("CHR_NEW_01", "CHR_NEW_03", "ALGN_CHR_01")
    )
    expect_true(all(c("CHR_NEW_01", "CHR_NEW_03", "ALGN_CHR_01") %in% seqnames(metTest)))
})


test_that("PHGMetrics general metric table return and update tests", {
    metricDir <- system.file("extdata", package = "rPHG2")
    algnPath  <- system.file("extdata", "toy_anchors_s01.anchorspro", package = "rPHG2")
    gvcfPath  <- system.file("extdata", "toy_gvcf_metrics.tsv", package = "rPHG2")
    hvcfPath  <- system.file("extdata", "LineA.h.vcf", package = "rPHG2")
    algnDf    <- read.table(algnPath, header = TRUE, sep = "\t")
    gvcfDf    <- read.table(gvcfPath, header = TRUE, sep = "\t")

    metBase <- PHGMetrics(c(algnPath, gvcfPath))

    # metricsTable()
    metTest <- metBase
    metTables <- metricsTable(metTest)
    expect_true(is(metTables, "list"))
    expect_equal(length(metTables), 2)
    lapply(metTables, function(x) {
        expect_true(is(x, "tbl_df"))
        expect_true(is(x, "data.frame"))
    })
    expect_error(
        object = metricsTable(metTest, name = "non_existant"),
        regexp = "Provided \'name\' not found in object"
    )
    expect_true(
        is(
            metricsTable(metTest, metTest$toy_anchors_s01, type = "gvcf"),
            "tbl_df"
        )
    )
    resGvcf <- metricsTable(metTest, type = "gvcf")
    resAlgn <- metricsTable(metTest, type = "align")
    expect_true(is(resGvcf, "tbl_df"))
    expect_true(is(resAlgn, "tbl_df"))
    expect_equal(ncol(resGvcf), 19)
    expect_equal(ncol(resAlgn), 10)
    expect_equal(colnames(resAlgn), camelToSnake(PHG_METRICS$VALID_ANCHOR_HEADERS))
    expect_equal(colnames(resGvcf), camelToSnake(PHG_METRICS$VALID_GVCF_HEADERS))
    expect_error(metricsTable(metTest, type = "invalid"))

    # metricsTable<-
    metTest <- metBase
    expect_error(
        object = metricsTable(metTest) <- "invalid",
        regexp = "Provided \'value\' not of type \'list\'"
    )
    expect_error(
        object = metricsTable(metTest) <- mtcars,
        regexp = "Provided \'value\' must be in a named \'list\'"
    )
    expect_error(
        object = metricsTable(metTest) <- list(mtcars),
        regexp = "Provided \'list\' object does not have names"
    )
    expect_error(
        object = metricsTable(metTest) <- list("invalid" = mtcars, "invalid" = iris),
        regexp = "Provided \'list\' object has duplicated IDs"
    )
    expect_error(
        object = metricsTable(metTest) <- list("invalid" = mtcars),
        regexp = "No valid metrics tables could be identified"
    )

    metTest <- metBase
    metricsTable(metTest) <- list(
        "valid_gvcf_from_path" = gvcfPath,
        "valid_gvcf_from_df"   = gvcfDf
    )
    expect_true(is(metricsTable(metTest), "list"))
    expect_equal(
        object   = metricsIds(metTest),
        expected = c(
            "toy_anchors_s01",
            "toy_gvcf_metrics",
            "valid_gvcf_from_path",
            "valid_gvcf_from_df"
        )
    )

    metTest <- metBase
    metricsTable(metTest) <- list(
        "valid_algn_from_path" = algnPath,
        "valid_algn_from_df"   = algnDf
    )
    expect_true(is(metricsTable(metTest), "list"))
    expect_equal(
        object   = metricsIds(metTest),
        expected = c(
            "toy_anchors_s01",
            "valid_algn_from_path",
            "valid_algn_from_df",
            "toy_gvcf_metrics"
        )
    )

    metTest <- metBase
    metricsTable(metTest) <- list(
        "valid_gvcf_from_path" = gvcfPath,
        "valid_gvcf_from_df"   = gvcfDf,
        "valid_algn_from_path" = algnPath,
        "valid_algn_from_df"   = algnDf
    )
    expect_true(is(metricsTable(metTest), "list"))
    expect_equal(
        object   = metricsIds(metTest),
        expected = c(
            "toy_anchors_s01",
            "valid_algn_from_path",
            "valid_algn_from_df",
            "toy_gvcf_metrics",
            "valid_gvcf_from_path",
            "valid_gvcf_from_df"
        )
    )

    metTest <- metBase
    metricsTable(metTest) <- list(
        "valid_gvcf_from_path"    = gvcfPath,
        "valid_gvcf_from_df"      = gvcfDf,
        "invalid_gvcf_from_path"  = "invalid_path",
        "invalid_gvcf_from_path2" = hvcfPath,
        "invalid_gvcf_from_df"    = mtcars,
        "valid_algn_from_path"    = algnPath,
        "valid_algn_from_df"      = algnDf,
        "invalid_algn_from_path"  = "invalid_path",
        "invalid_algn_from_path2" = hvcfPath,
        "invalid_algn_from_df"    = mtcars
    )
    expect_true(is(metricsTable(metTest), "list"))
    expect_true(is(metricsTable(metTest, type = "gvcf"), "list"))
    expect_true(is(metricsTable(metTest, type = "align"), "list"))
    expect_equal(
        object   = metricsIds(metTest),
        expected = c(
            "toy_anchors_s01",
            "valid_algn_from_path",
            "valid_algn_from_df",
            "toy_gvcf_metrics",
            "valid_gvcf_from_path",
            "valid_gvcf_from_df"
        )
    )
    metOut01 <- utils::capture.output(metTest)
    expect_equal(length(metOut01), 9)

    metTest <- metBase
    metricsTable(metTest) <- list(
        "toy_anchors_s01"      = algnDf,
        "valid_algn_from_path" = algnPath
    )
    expect_equal(
        object   = metricsIds(metTest),
        expected = c(
            "toy_anchors_s01",
            "valid_algn_from_path",
            "toy_gvcf_metrics"
        )
    )

})


test_that("PHGMetrics general dot plot tests", {
    metricDir <- system.file("extdata", package = "rPHG2")

    metBase <- PHGMetrics(metricDir)

    expect_true(is(plotDot(metBase, metBase$toy_anchors_s01), "ggplot"))
    expect_true(is(plotDot(metBase, metBase$toy_anchors_s01, colorId = "score"), "ggplot"))
    expect_true(is(plotDot(metBase, metBase$toy_anchors_s01, refSeqId = c("1", "2")), "ggplot"))
    expect_true(is(plotDot(metBase, metBase$toy_anchors_s01, querySeqId = c("1", "2")), "ggplot"))
    expect_true(is(plotDot(metBase), "ggplot"))
    expect_error(plotDot(metBase, metBase$toy_anchors_s01, colordId = "not_a_column"))
    expect_error(plotDot(metBase, metBase$toy_anchors_s01, refSeqId = "1", querySeqId = "2"))
    expect_error(plotDot(metBase, metBase$toy_gvcf_metrics))
    expect_error(plotDot(metBase, metBase$invalid_selection), regexp = "ID is not a valid AnchorWave table")
    expect_error(
        plotDot(
            object = metBase,
            metricId = c(
                metBase$toy_anchors_s01,
                metBase$toy_anchors_s01
            ),
            refSeqId = "1",
            querySeqId = "2"
        )
    )

})


test_that("PHGMetrics general gVCF plot tests", {
    metricDir <- system.file("extdata", package = "rPHG2")

    metBase <- PHGMetrics(metricDir)
    expect_true(is(plotGvcf(metBase, metBase$toy_gvcf_metrics), "ggplot"))
    expect_error(is(plotGvcf(metBase, c(metBase$toy_gvcf_metrics, metBase$toy_anchors_s01)), "ggplot"))
    expect_true(is(plotGvcf(metBase), "ggplot"))
    expect_true(is(plotGvcf(metBase, f = ALL ~ ALL), "ggplot"))
    expect_true(is(plotGvcf(metBase, f = ALL ~ Vu01 + Vu05), "ggplot"))
    expect_true(is(plotGvcf(metBase, f = num_ns ~ Vu01 + Vu05), "ggplot"))
    expect_error(is(plotGvcf(metBase, f = ALL + CORE ~ ALL + Vu02), "ggplot"))
    expect_error(is(plotGvcf(metBase, f = CORE + num_ns ~ ALL + Vu02), "ggplot"))
    expect_error(is(plotGvcf(metBase, f = num_nsss ~ ALL + Vu02), "ggplot"))
    expect_error(is(plotGvcf(metBase, f = num_nsss ~ ALL), "ggplot"))

})


