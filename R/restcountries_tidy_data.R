#' restcountries_tidy_data
#'
#' A dataset containing tidied information about countries from the Restcountries API.
#'
#' This dataset includes a variety of country-level data such as country codes, names, capitals,
#' regions, subregions, continents, currencies, population, geographic coordinates, languages, and more.
#'
#' @format A data frame with several rows and the following columns:
#' \describe{
#'   \item{tld}{Top-level domain(s) associated with the country.}
#'   \item{common_name}{Common name of the country.}
#'   \item{official_name}{Official name of the country.}
#'   \item{cca2}{Country code (2-letter).}
#'   \item{cca3}{Country code (3-letter).}
#'   \item{fifa}{FIFA code of the country.}
#'   \item{independent}{Independence status (TRUE/FALSE).}
#'   \item{status}{Country status (e.g., officially assigned).}
#'   \item{un_member}{Whether the country is a UN member (TRUE/FALSE).}
#'   \item{region}{Geographic region.}
#'   \item{subregion}{Subregion.}
#'   \item{population}{Population of the country.}
#'   \item{capital}{Capital city of the country.}
#'   \item{capital_lat}{Latitude of the capital city.}
#'   \item{capital_lon}{Longitude of the capital city.}
#'   \item{continents}{Continent(s) the country is part of.}
#'   \item{lat}{Latitude of the country.}
#'   \item{lon}{Longitude of the country.}
#'   \item{landlocked}{Whether the country is landlocked (TRUE/FALSE).}
#'   \item{borders}{Countries that share a border.}
#'   \item{area}{Total area of the country in square kilometers.}
#'   \item{start_of_week}{Day the week starts (e.g., Monday).}
#'   \item{timezones}{Timezones applicable to the country.}
#'   \item{root}{Root of the country calling code.}
#'   \item{suffixes}{Suffixes of the country calling code.}
#'   \item{car_side}{Which side of the road cars drive on.}
#'   \item{googlemaps}{Google Maps link for the country.}
#'   \item{openstreetmaps}{OpenStreetMap link for the country.}
#'   \item{flags_png}{URL to PNG image of the country flag.}
#'   \item{flags_svg}{URL to SVG image of the country flag.}
#'   \item{flags_alt}{Alternative text for the country flag.}
#'   \item{currencies}{Currencies used in the country.}
#'   \item{languages}{Languages spoken in the country.}
#'   \item{currency_name}{Name of the primary currency used.}
#'   \item{currency_symbol}{Symbol of the primary currency used.}
#'   \item{calling_code}{Calling code(s) associated with the country.}
#' }
#'
#' @source Data obtained from the Restcountries Json file and processed for use in this package.
#'
#' @examples
#' # Load the dataset and view the first few rows
#' data(restcountries_tidy_data)
#' head(restcountries_tidy_data)
"restcountries_tidy_data"
