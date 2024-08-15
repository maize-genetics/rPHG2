## ----
# Plot a Dot Plot from AnchorWave metrics
#
# @param df
# A data frame containing genomic data with columns for
# query and reference start positions, and optionally scores and
# strand directions.
# @param metricId
# Identifier for the metric to be plotted.
# @param anchorPath
# Path for additional data anchoring (not used in
# the current implementation).
# @param querySeqId
# Optional vector of query sequence identifiers
# for filtering. Default is NULL.
# @param refSeqId
# Optional vector of reference sequence identifiers for filtering.
# Default is NULL.
# @param queryLab
# Optional label for the query axis. Default is "Query".
# @param refLab
# Optional label for the reference axis. Default is "Reference".
# @param colorId
# Identifier for coloring method. Accepts "score" for color scale based on
# scores, "strand" for manual color mapping of strand directions, or other
# identifiers for no coloring.
#
# @return A ggplot object representing the dot plot.
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

    scaleUnit <- switch(
        EXPR     = colorId,
        "score"  = ggplot2::scale_color_viridis_c(),
        "strand" = ggplot2::scale_color_manual(values = c("+" = "#DA897C", "-" = "#0D6A82")),
    )

    if (!is.null(refSeqId)) df <- df[which(df$ref_chr %in% refSeqId), ]
    if (!is.null(querySeqId)) df <- df[which(df$query_chr %in% querySeqId), ]

    if (nrow(df) == 0) {
        rlang::abort("No data remains with provided ref/query chromosome selections")
    }

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
        ggplot2::ggtitle(metricId) +
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


