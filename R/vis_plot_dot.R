## ----
plotDotFromMetrics <- function(
    df,
    metricId,
    anchorPath,
    querySeqId = NULL,
    refSeqId = NULL,
    queryLab = NULL,
    refLab = NULL,
    colorId
) {

    if (is.null(queryLab)) queryLab <- "Query"
    if (is.null(refLab)) refLab <- "Reference"

    if (colorId == "score") {
        scaleUnit <- ggplot2::scale_color_viridis_c()
    } else if (colorId == "strand") {
        scaleUnit <- ggplot2::scale_color_manual(
            values = c(
                "+" = "#DA897C",
                "-" = "#0D6A82"
            )
        )
    } else {
        scaleUnit <- NULL
    }

    if (!is.null(refSeqId)) df <- df[which(df$ref_chr   %in% refSeqId), ]
    if (!is.null(querySeqId)) df <- df[which(df$query_chr %in% querySeqId), ]

    toMb <- function(x) x / 1e6

    p <- ggplot2::ggplot(data = df) +
        ggplot2::aes(
            x = !!rlang::sym("query_start"),
            y = !!rlang::sym("reference_start"),
            color = !!rlang::sym(colorId)
        ) +
        ggplot2::geom_point(size = 0.3) +
        ggplot2::scale_y_continuous(labels = toMb) +
        ggplot2::scale_x_continuous(labels = toMb) +
        ggplot2::facet_grid(
            rows   = ggplot2::vars(!!rlang::sym("ref_chr")),
            col    = ggplot2::vars(!!rlang::sym("query_chr")),
            scales = "free",
            space  = "free"
        ) +
        scaleUnit +
        ggplot2::xlab(paste(queryLab, "(Mbp)")) +
        ggplot2::ylab(paste(refLab, "(Mbp)"))

    return(p)
}


