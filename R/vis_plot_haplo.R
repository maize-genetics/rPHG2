## ----
# Helper function to construct common ggplot theme
custHapTheme <- function() {
    ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
            )
        )
}


## ----
# Helper function to construct the geometry based on the 'geom' parameter
selectGeom <- function(geom) {
    switch(
        EXPR = geom,
        "l"  = ggplot2::geom_line(ggplot2::aes(group = 1)),
        "b"  = ggplot2::geom_bar(stat = "identity"),
        "p"  = ggplot2::geom_point()
    )
}


## ----
# Plot Haplotype Data from PHG Dataset
#
# @description
# This function creates a ggplot visualization of haplotype data from a PHG
# (Practical Haplotype Graph) dataset, allowing for multiple geometric
# representations (lines, bars, points). The plot can either cover the entire
#  dataset or a specific genomic range provided by the user.
#
# @param object
# A PHG dataset object containing haplotype information. This is typically the
# output of a function like `loadPhgDataSet()` or similar.
# @param gr
# A \code{GRanges} object specifying a genomic range for filtering the
# haplotype data. If \code{NULL}, the function will plot the data across the
# entire dataset.
# @param geom
# A character string specifying the type of geometric representation for the
# plot. Accepted values are:
# \itemize{
#   \item \code{"l"} for lines (default),
#   \item \code{"b"} for bars,
#   \item \code{"p"} for points.
# }
# If an invalid value is provided, the function will raise an error.
#
# @details
# When no genomic range is provided (i.e., \code{gr = NULL}), the function
# plots the number of unique haplotypes across the entire reference genome or
# dataset. If a genomic range is provided, it filters the data based on
# overlaps between the reference ranges in the dataset and the query range. In
# both cases, the resulting plot uses \code{ggplot2} for visualization, and
# different geometries can be selected via the \code{geom} parameter.
#
# @return A \code{ggplot} object visualizing the haplotype counts. When
# \code{gr} is \code{NULL}, the plot shows the number of unique haplotypes
# across reference positions. When \code{gr} is provided, the plot is filtered
# to display haplotype counts within the specified range.
plotHaploFromPhgDataSet <- function(object, gr = NULL, geom = "l") {
    # Validate 'geom' parameter
    if (!geom %in% c("l", "b", "p")) {
        rlang::abort("'geom' must be one of 'l' (line), 'b' (bar), or 'p' (point).")
    }

    nHaplo <- numberOfHaplotypes(object, byRefRange = TRUE)

    # Plot when 'gr' is NULL
    if (is.null(gr)) {
        return(
            ggplot2::ggplot(nHaplo) +
                ggplot2::aes(
                    x = !!rlang::sym("start"),
                    y = !!rlang::sym("n_haplo"),
                    alpha = 0.01
                ) +
                selectGeom(geom) +
                ggplot2::scale_y_continuous(
                    breaks = seq_len(max(nHaplo$n_haplo)),
                    limits = c(1, max(nHaplo$n_haplo))
                ) +
                ggplot2::scale_x_continuous(
                    labels = scales::label_number(
                        scale_cut = scales::cut_short_scale()
                    )
                ) +
                ggplot2::xlab("Position (bp)") +
                ggplot2::ylab("Number of unique haplotypes") +
                ggplot2::facet_wrap(~ seqnames, scales = "free_x") +
                ggplot2::guides(alpha = "none") +
                custHapTheme()
        )
    }

    # Validate 'gr' type
    if (!is(gr, "GRanges")) {
        rlang::abort("'gr' object is not of type 'GRanges'")
    }

    # Get ref ranges GRanges object and add 'sub_id" parameters
    refRanges <- readRefRanges(object)
    gr$sub_id <- paste0("QR ", GenomeInfoDb::seqnames(gr), ":", IRanges::ranges(gr))

    # Find overlaps
    overlaps <- suppressWarnings(GenomicRanges::findOverlaps(refRanges, gr))

    if (length(overlaps) == 0) {
        rlang::abort("No reference ranges identified with given query")
    }

    # Filter and merge data
    filtGr <- refRanges[S4Vectors::queryHits(overlaps)]
    filtGr$sub_id <- gr$sub_id[S4Vectors::subjectHits(overlaps)]
    filtGrDf <- merge(as.data.frame(filtGr), nHaplo)

    # Plot with filtered data
    ggplot2::ggplot(filtGrDf) +
        ggplot2::aes(x = !!rlang::sym("rr_id"), y = !!rlang::sym("n_haplo")) +
        selectGeom(geom) +
        ggplot2::scale_y_continuous(breaks = seq(0, max(nHaplo$n_haplo), by = 1)) +
        ggplot2::xlab("Reference range ID") +
        ggplot2::ylab("Number of unique haplotypes") +
        ggplot2::facet_grid(~ sub_id, scales = "free_x", space = "free") +
        custHapTheme()
}


