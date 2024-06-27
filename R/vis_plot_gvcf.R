## ----
# Plot GVCF Metrics from Dataframe
#
# This function creates bar plots of GVCF metrics based on the given dataframe
# and formula.
#
# @param df
# A dataframe containing the GVCF metrics data.
# @param formula
# A formula specifying the metrics to plot on the left-hand side (LHS) and the
# chromosomes to filter by on the right-hand side (RHS). Use "ALL" on the LHS
# to include all metrics, or "CORE" to include core metrics.
# @param nRow
# An integer specifying the number of rows for the combined plot layout.
# @param nCol
# An integer specifying the number of columns for the combined plot layout.
# @param tag
# What tag type do you want passed to final plot?
#
# @details
# This function parses the formula to determine which metrics to plot and
# which chromosomes to filter by. It then creates individual bar plots for
# each metric and combines them into a single plot using the `patchwork`
# package.
#
# @return A combined plot of the specified GVCF metrics.
plotGvcfFromMetrics <- function(df, formula, nRow, nCol, tag) {
    parsedForm <- parseFormula(formula)
    lhsVars <- parsedForm$lhs
    rhsVars <- parsedForm$rhs

    coreIds <- c(
        "num_snps", "num_ns", "num_ins", "num_del",
        "percent_mapped_to_ref", "percent_identity_with_ref",
        "largest_insertion", "largest_deletion"
    )

    if (any(lhsVars == "ALL")) {
        filtGvcfMap <- GVCF_MAP
    } else if (any(lhsVars == "CORE")) {
        filtGvcfMap <- GVCF_MAP[GVCF_MAP$id %in% coreIds, ]
    } else {
        filtGvcfMap <- GVCF_MAP[GVCF_MAP$id %in% lhsVars, ]
    }

    if (length(filtGvcfMap) == 0) {
        rlang::abort("No valid metrics found in left-hand side of equation")
    }

    filtData <- df[df$chrom %in% rhsVars, ]

    if (any(rhsVars == "ALL")) {
        facetLayer <- NULL
    } else {
        facetLayer <- ggplot2::facet_wrap(
            facets = ggplot2::vars(!!rlang::sym("chrom")),
            nrow = 1
        )
    }

    # Create a list to store individual plots
    plotList <- list()

    # Loop through each column in "filtered" colKeepMap and create a bar plot
    for (i in seq_len(nrow(filtGvcfMap))) {
        row <- filtGvcfMap[i, ]
        col <- row$id
        if (col %in% colnames(filtData)) {
            p <- ggplot2::ggplot(filtData) +
                ggplot2::aes(
                    x = !!rlang::sym("taxa"),
                    y = !!rlang::sym(col),
                    fill = !!rlang::sym("taxa")
                ) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::labs(
                    title = row$plt,
                    x = NULL,
                    y = row$axs,
                    fill = "Sample"
                ) +
                ggplot2::scale_y_continuous(
                    labels = scales::label_number(
                        scale_cut = scales::cut_short_scale()
                    )
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank(),
                    legend.position = "bottom"
                ) +
                facetLayer

            # Add plot to the list
            plotList[[row$id]] <- p
        }
    }

    # Combine all plots into one using patchwork and add letters
    combinedPlot <- patchwork::wrap_plots(plotList, ncol = nCol, nrow = nRow) +
        patchwork::plot_annotation(tag_levels = tag) +
        patchwork::plot_layout(guides = 'collect') &
        ggplot2::theme(
            legend.position = "bottom",
            plot.tag = ggplot2::element_text(face = 'bold'),
            legend.title = ggplot2::element_text(face = "bold")
        )

    # Return the combined plot
    return(combinedPlot)
}


