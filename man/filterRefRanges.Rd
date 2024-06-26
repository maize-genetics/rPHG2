% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_all_generics.R, R/class_phg_dataset.R
\name{filterRefRanges}
\alias{filterRefRanges}
\alias{filterRefRanges,PHGDataSet-method}
\title{Filter data by reference range}
\usage{
filterRefRanges(object, ...)

\S4method{filterRefRanges}{PHGDataSet}(object, gRanges)
}
\arguments{
\item{object}{an \code{rPHG2} dataset}

\item{...}{Additional arguments, for use in specific methods}

\item{gRanges}{A \code{GRanges} coordinate object}
}
\description{
Filters \code{rPHG2}-related datasets by reference range coordinates or ID.
}
