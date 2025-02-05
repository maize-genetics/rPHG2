% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_all_generics.R,
%   R/class_haplotype_graph.R, R/class_phg_con.R, R/class_phg_dataset.R
\name{host}
\alias{host}
\alias{host,HaplotypeGraph-method}
\alias{host,PHGCon-method}
\alias{host,PHGDataSet-method}
\title{Return host data}
\usage{
host(object, ...)

\S4method{host}{HaplotypeGraph}(object)

\S4method{host}{PHGCon}(object)

\S4method{host}{PHGDataSet}(object)
}
\arguments{
\item{object}{an \code{rPHG2} local or server connection object}

\item{...}{Additional arguments, for use in specific methods}
}
\description{
Returns the host information for a given object
}
