% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_all_generics.R, R/class_phg_dataset.R
\docType{methods}
\name{readSequence}
\alias{readSequence}
\alias{readSequence,PHGDataSet-method}
\title{Return DNA sequence data}
\usage{
readSequence(object, ...)

\S4method{readSequence}{PHGDataSet}(object, rrId = NULL, hapId = NULL, pad = 0)
}
\arguments{
\item{object}{A \code{PHGDataSet} object.}

\item{...}{Additional arguments, for use in specific methods}

\item{rrId}{An reference range ID. Defaults to \code{NULL}. If specified, this will return
all haplotype sequences for each sample in a given reference range.}

\item{hapId}{A haplotype ID. Defaults to \code{NULL}.}

\item{pad}{An integer value for padding around the sequence region. Defaults to \code{0}.}
}
\value{
Returns sequence data corresponding to the specified
parameters.
}
\description{
Gets DNA sequence from a \code{PHGDataSet}

This S4 method reads sequence data from a \code{PHGDataSet} object.
It is linked to the \code{AGC} CLI program for sequence retrieval.
The method takes optional parameters to specify read range IDs
or haplotype IDs, and an optional \code{pad} value.
}
\details{
This method calls \code{readSequenceFromPds()} to retrieve the
sequence data. Additional arguments may be used to refine
the range.
}
\examples{
\dontrun{
myPds <- PHGDataSet()
readSequence(myPds, rrId = 1, hapId = "hapA", pad = 100)
}

}
