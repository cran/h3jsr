% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_api.R
\name{cell_to_splitlong}
\alias{cell_to_splitlong}
\title{H3 cell to split long}
\usage{
cell_to_splitlong(h3_address, simple = TRUE)
}
\arguments{
\item{h3_address}{Character; 15-character index generated by H3.}

\item{simple}{Logical; whether to return a vector or a data frame containing
both inputs and outputs.}
}
\value{
list of integer pairs, one for each address supplied.
}
\description{
Convert an H3 cell (64-bit hexidecimal string) into a "split long" - a pair
of 32-bit integers.
}
\examples{
cell_to_splitlong(h3_address = '8abe8d12acaffff')
}
