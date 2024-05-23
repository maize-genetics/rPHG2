plotGvcfFromMetrics <- function(object, metricId) {
    
    data <- metricsTable(object, metricId)
    
    # Named vector for column titles
    colKeepMap <- c(
        "num_snps" = "# of SNPs",
        "num_ns" = "# of Ns",
        "num_ins" = "# of insertions", 
        "num_del" = "# of deletions",
        "percent_mapped_to_ref" = "% mapped to ref", 
        "percent_identity_with_ref" = "% identity with ref",
        "largest_insertion" = "Largest insertion", 
        "largest_deletion" = "Largest deletion"
    )
    
    colAxisMap <- c(
        "num_snps" = "N",
        "num_ns" = "N",
        "num_ins" = "N", 
        "num_del" = "N",
        "percent_mapped_to_ref" = "%", 
        "percent_identity_with_ref" = "%",
        "largest_insertion" = "bp", 
        "largest_deletion" = "bp"
    )
    
    # Filter the rows where "chrom" is "ALL"
    filteredData <- data[data$chrom == "ALL", ]
    
    # Create a list to store individual plots
    plotList <- list()
    
    # Loop through each column in colKeepMap and create a bar plot
    for (col in names(colKeepMap)) {
        if (col %in% colnames(filteredData)) {
            p <- ggplot(filteredData) + 
                aes(
                    x = !!rlang::sym("taxa"), 
                    y = !!rlang::sym(col), 
                    fill = !!rlang::sym("taxa")
                ) +
                geom_bar(stat = "identity") +
                labs(
                    title = colKeepMap[col], 
                    x = NULL, y = colAxisMap[col],
                    fill = "Sample") +
                theme(
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    legend.position = "bottom"
                )
            
            # Add plot to the list
            plotList[[colKeepMap[col]]] <- p
        }
    }
    
    # Combine all plots into one using patchwork and add letters
    combinedPlot <- wrap_plots(plotList, ncol = 2) +
        plot_annotation(tag_levels = "A") +
        plot_layout(guides = 'collect') &
        theme(
            legend.position = "bottom", 
            plot.tag = element_text(face = 'bold'),
            legend.title = element_text(face = "bold")
        )
    
    # Return the combined plot
    return(combinedPlot)
}

