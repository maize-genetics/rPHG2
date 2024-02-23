## ----
#' Plot haplotype IDs by reference range
#'
#' @param m A haplotype ID matrix
#' @param sampleId Sample ID to compare against ancestors
#'
#' @export
plotHapIds <- function(m, sampleId = "F1_test_G1") {
    nM <- convertHapIdToNumber(m)
    nMDf <- convertMatrixToDf(nM) |>
        dplyr::group_by(x) |>
        dplyr::mutate(
            source = dplyr::if_else(
                y == sampleId,
                dplyr::if_else(
                    sum(value == value[y == sampleId]) > 2,
                    "F1_test_G1",
                    y[which(value == value[y == sampleId])[2]]
                ),
                y
            )
        ) |>
        ungroup()

    coreColors <- c(
        "#e6cf45",
        "#45a1e6",
        "#63c965",
        "grey"
    )
    coreColors <- setNames(coreColors, c("LineA_G1", "LineB_G1", "Ref_G1", sampleId))
    scaleColor <- scale_fill_manual(values = coreColors)

    nMDf$x <- factor(nMDf$x, levels = unique(nMDf$x))

    p <- ggplot(nMDf, aes(x = x, y = y, fill = source)) +
        geom_tile(color = "white", lwd = 0.5) +
        scaleColor +
        coord_fixed() +
        scale_x_discrete(
            guide = guide_axis(angle = 90),
            position = "top"
        ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            axis.title = element_blank()
        )

    return(p)
}


