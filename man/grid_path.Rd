% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h3_algorithms.R
\name{grid_path}
\alias{grid_path}
\title{Path between H3 cells}
\usage{
grid_path(origin = NULL, destination = NULL, simple = TRUE)
}
\arguments{
\item{origin}{Character vector or list of 15-character indices generated by
H3.}

\item{destination}{Character vector or list of 15-character indices
generated by H3.}

\item{simple}{Logical; whether to return a vector of outputs or a list object
containing both inputs and outputs.}
}
\value{
A vector of h3 cells of form c(origin, c(path), destination).
}
\description{
This function returns a path of H3 cells between a start and end cell
(inclusive).
}
\note{
\itemize{
  \item{Input H3 cells must be of the same resolution or results cannot
  be computed. This function may fail to find the distance between two
  indexes if they are very far apart or on opposite sides of a pentagon.}
  \item{The specific output of this function should not be considered stable
  across library versions. The only guarantees the library provides are that
  the line length will be \code{h3_distance(start, end) + 1} and that every
  index in the line will be a neighbor of the preceding index.}
  \item{Lines are drawn in grid space, and may not correspond exactly to
  either Cartesian lines or great arcs}
  }
}
\examples{
\dontrun{
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
nc_pts <- sf::st_centroid(nc[c(1, 2), ])
nc_6 <- point_to_cell(nc_pts, res = 6)
# find a path between these two addresses:
grid_path(nc_6[1], nc_6[2], simple = TRUE)

}
}
