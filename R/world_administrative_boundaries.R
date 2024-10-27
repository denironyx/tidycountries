#' world_administrative_boundaries
#'
#' A dataset containing information about world administrative boundaries.
#'
#' This dataset includes country code and the geometries
#'
#' @format A data frame with several rows and the following columns:
#' \describe{
#'   \item{cca3}{Country code (3-letter).}
#'   \item{geometry}{geographic boundaries}
#' }
#'
#' @source Data obtained from the World Food Programme (UN agency) processed for use in this package.
#'
#' @examples
#' # Load the dataset and view the first few rows
#' data(world_administrative_boundaries)
#' head(world_administrative_boundaries)
"world_administrative_boundaries"
