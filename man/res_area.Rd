% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info_utilities.R
\name{res_area}
\alias{res_area}
\title{Get average cell area}
\usage{
res_area(res = NULL, units = c("m2", "km2"), fast = TRUE)
}
\arguments{
\item{res}{Integer; Desired H3 resolution. See
\url{https://h3geo.org/docs/core-library/restable/} for allowable values and related dimensions.}

\item{units}{Areal unit to report in. Options are square meters or square
kilometers.}

\item{fast}{Logical; whether to retrieve values from a locally stored table or
reclaculate from source.}
}
\value{
Numeric; average H3 cell area.
}
\description{
This function returns the average area of an H3 cell at a given
resolution.
}
\examples{
# Return average H3 cell area at each resolution in square meters
res_area(res = seq(0, 15), units = 'm2')

}
