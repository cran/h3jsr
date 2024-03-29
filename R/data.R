#' H3 index utility information table
#'
#' A dataset containing information about h3 cell indexes at each resolution,
#' calculated using H3's built-in functions.
#'
#' @format A data frame with 16 rows and 6 variables: \describe{
#'   \item{h3_resolution}{H3 resolution index number}
#'   \item{avg_area_sqm}{Average area of an H3 cell index at the given
#'     resolution, in square meters.}
#'   \item{avg_area_sqkm}{Average area of an
#'     H3 cell index at the given resolution, in square kilometers.}
#'   \item{avg_edge_m}{Average edge length of an H3 cell index at the given
#'     resolution, in meters.}
#'   \item{avg_edge_km}{Average edge length of an H3
#'     cell index at the given resolution, in kilometers.}
#'   \item{avg_cendist_m}{Average distance between cell centers at the given
#'     resolution, in meters.}
#'   \item{avg_cendist_km}{Average distance between cellcenters at the given
#'     resolution, in kilometers.}
#'   \item{total_unique_indexes}{Total number of H3 cells at the given
#'   resolution.}}
#' @source See also
#'   \url{https://h3geo.org/docs/core-library/restable/}
#'
"h3_info_table"
