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
        dplyr::ungroup()

    coreColors <- c(
        "#e6cf45",
        "#45a1e6",
        "#63c965",
        "grey"
    )
    coreColors <- setNames(coreColors, c("LineA_G1", "LineB_G1", "Ref_G1", sampleId))
    scaleColor <- ggplot2::scale_fill_manual(values = coreColors)

    nMDf$x <- factor(nMDf$x, levels = unique(nMDf$x))

    p <- ggplot2::ggplot(nMDf, ggplot2::aes(x = x, y = y, fill = source)) +
        ggplot2::geom_tile(color = "white", lwd = 0.5) +
        scaleColor +
        ggplot2::coord_fixed() +
        ggplot2::scale_x_discrete(
            guide = ggplot2::guide_axis(angle = 90),
            position = "top"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = "none",
            axis.title = ggplot2::element_blank()
        )

    return(p)
}


