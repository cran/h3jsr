% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unidirectional_edges.R
\name{get_udedges}
\alias{get_udedges}
\title{Get all directed edge indexes for a given H3 cell}
\usage{
get_udedges(h3_address = NULL, simple = TRUE)
}
\arguments{
\item{h3_address}{Character; 15-character index generated by H3.}

\item{simple}{Logical; whether to return a vector of outputs or a data frame
containing both inputs and outputs.}
}
\value{
By default, list of \code{length(h3_address)}. Each list contains a
  character vector of H3 edge indexes.
}
\description{
Get all directed edge indexes for a given H3 cell index.
}
\examples{
# Get all the edge indexes for this cell
get_udedges(h3_address = '86be8d12fffffff')

}
