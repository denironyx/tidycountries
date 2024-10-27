# core.R

# Source the utils.R script to load necessary data and functions
source("R/zzz.R")

## Function to fetch all country information from restcountries .rds
fetch_restcountries_data <- function(){
  restcountries_data
}

#' get_country_info
#'
#' This function retrieves information about a specific country based on its country code (cca2 or cca3) or common name.
#' The function is case-insentive and provides a comprehensive overview of the selected country
#' If "all" is passed as the input, it returns data for all countries. If the input does not match any country,
#' the function returns a list of all available country names.
#' @param country_value A character string representing the country code(cca2 or cca3) or common name. The input is case-insensitive.
#'                      If "all" is passed, the function return data for all countries.
#' @param geometry Logical. If `TRUE`, includes spatial geometry data for the country (boundaries).
#'                 Defaults to `FALSE`. When `TRUE`, an additional column for geographic boundaries  is included.
#'
#' @return A data frame with selected country information. If `geometry = TRUE`, the result includes a geometry column
#'         with boundary data, making the returned object ready to be converted to an `sf` (simple features) data frame for spatial analysis.
#'         If the input is "all", it returns data for all countries. If no match is found, a list of all available country names is printed.
#'
#' @note The returned data frame includes relevant country details. If `geometry = TRUE`, an additional column for geographic boundaries  is included.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Examples usage: Get information for Nigeria
#' nigeria_info <- get_country_info("Nigeria")
#' print(nigeria_info)
#'
#' # Example usage: Get information for a country using it's cca2 code
#' usa_info <- get_country_info("US")
#' print(usa_info)
#' }
#'@importFrom dplyr %>% filter collect all_of distinct select left_join
#'@importFrom stringr str_to_lower
#'
get_country_info <- function(country_value, geometry = FALSE){
  # Convert input to lowercase
  country_value_lower <- str_to_lower(country_value)

  # Filter data for "all" countries or specific country
  result <- if (country_value_lower == 'all') {
    restcountries_data %>%
      select(all_of(select_countries_columns))
  } else {
    restcountries_data %>%
      filter(
        str_to_lower(cca2) == country_value_lower |
          str_to_lower(cca3) ==  country_value_lower |
          str_to_lower(common_name) == country_value_lower
      ) %>%
      select(all_of(select_countries_columns)) %>%
      distinct()
  }

  # Check if result is empty and provide a warning message if no data is found
  if (nrow(result) == 0) {
    warning("Sorry, no data found for the provide input. \n")
    warning("Here is a link to all available countries <https://en.wikipedia.org/wiki/List_of_sovereign_states>")
    return(NULL)
  }

  # If geometry is TRUE, perform a left join with the world administrative boundaries data
  if (geometry) {
    result <- result %>%
      left_join(world_administrative_boundaries, by = "cca3")

  } else {
    result
  }

  return(result)

}

#' get_countries_by_region
#'
#' This function retrieves a list of countries based on a specified region, subregion, or continent.
#' The function is case-insensitive and orders the countries alphabetically by their common names.
#' If the input does not match any region, subregion, or continent, the function provides a list of all available regions, subregions, and continents.
#'
#' @param country_region_value A character string representing the region, subregion, or continent. The input is case-insensitive.
#'
#' @param geometry Logical. If `TRUE`, includes spatial geometry data for each country (boundaries).
#'                 Defaults to `FALSE`. When `TRUE`, an additional column for geographic boundaries is included.
#'
#' @return A data frame with information on countries within the specified region, subregion, or continent.
#'         If `geometry = TRUE`, the result includes a geometry column with boundary data.
#'         If no match is found, a message lists all available regions, subregions, and continents.
#'
#' @note This function returns relevant information on countries in a specified region. When `geometry = TRUE`, it returns an `sf` object, including spatial data.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example usage: Get a list of countries in Africa
#' africa_countries <- get_countries_by_region("Africa")
#' print(africa_countries)
#'
#' # Example usage: Get countries in a specific continent with geometry included
#' western_europe_countries <- get_countries_by_region("Western Europe", geometry = TRUE)
#' print(western_europe_countries)
#'
#' # Example usage: Get a list of countries in the continent of Asia
#' asia_countries <- get_countries_by_region("Asia")
#' print(asia_countries)
#' }
#'
#'@importFrom dplyr %>% filter collect all_of distinct arrange left_join
#'@importFrom stringr str_to_lower
#'
get_countries_by_region <- function(country_region_value, geometry = FALSE) {
  country_region_value_lower <- str_to_lower(country_region_value)

  # Filter and select columns
  result <- restcountries_data %>%
    filter(
      str_to_lower(region) == country_region_value_lower |
        str_to_lower(subregion) == country_region_value_lower |
        str_to_lower(continents) == country_region_value_lower
    ) %>%
    select(all_of(select_countries_columns)) %>%
    distinct() %>%
    arrange(common_name)  # Order alphabetically by country name

  # Check if geometry is requested and join with spatial data
  if (geometry) {
    result <- result %>%
      left_join(world_administrative_boundaries, by = "cca3")
  } else {
    result
  }

  # Check if result is empty and provide appropriate message
  if (nrow(result) == 0) {
    warning("Sorry, no data found for the provided input.")
    message("Here is a list of all available regions, subregions, and continents:")
    all_region_info <- restcountries_data %>%
      select(continents) %>%
      distinct()
    message(all_region_info)

  } else {

    return(result)
  }
}


#' get_countries_by_currency
#'
#' This function retrieves a list of countries where a specified currency is used.
#' The function is case-insensitive and matches the currency name or part of the name.
#' The output is ordered alphabetically by country name.
#'
#' @param currency_input A character string representing the currency name or part of the name. The input is case-insensitive.
#'
#' @param geometry A logical value indicating whether to include geographic boundary data. Defaults to `FALSE`.
#'                 If `TRUE`, the function performs a left join with boundary data and returns a spatial `sf` data frame.
#'
#' @return A data frame containing the list of countries that use the specified currency, ordered alphabetically by country name.
#'         The columns include country codes (CCA2 and CCA3), common name, capital, continents, currency name, currency symbol, latitude, and longitude.
#'         If `geometry = TRUE`, an additional column for geographic boundaries  is included.
#'
#' @note The function utilizes the pre-loaded `restcountries_data` dataset. Ensure that this dataset is loaded before invoking the function.
#'       The function uses a case-insensitive regular expression to match the currency name, allowing partial matches.
#'       If `geometry = TRUE`, the function joins with the `world_administrative_boundaries` dataset, which must also be pre-loaded.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example usage: Find all countries that use the Euro
#' euro_countries <- get_countries_by_currency("Euro")
#' print(euro_countries)
#'
#' # Example usage: Find all countries that use a currency with "dollar" in its name
#' dollar_countries <- get_countries_by_currency("dollar", geometry = TRUE)
#' print(dollar_countries)
#'
#' # Example usage: Find all countries that use the Yen
#' yen_countries <- get_countries_by_currency("Yen")
#' print(yen_countries)
#' }
#'@importFrom dplyr %>% filter collect all_of distinct select arrange left_join
#'@importFrom stringr str_to_lower regex str_detect
#'
get_countries_by_currency <- function(currency_input, geometry = FALSE) {
  # Convert the input currency name to lowercase
  currency_name_lower <- str_to_lower(currency_input)

  # Filter and select relevant columns
  result <- restcountries_data %>%
    filter(
      # Use case-insensitive regex to match substrings
      str_detect(str_to_lower(currencies), regex(currency_name_lower, ignore_case = TRUE))
    ) %>%
    select(
      cca2,
      cca3,
      common_name,
      capital,
      continents,
      currencies,
      currency_name,
      currency_symbol,
      lat,
      lon
    ) %>%
    distinct() %>%
    arrange(common_name) # Order alphabetically by country name

  # Check if geometry is requested and join with spatial data
  if (geometry) {
    result <- result %>%
      left_join(world_administrative_boundaries, by = "cca3")
  } else {
    result
  }

  return(result)
}

#' get_country_by_calling_code
#'
#' This function retrieves information about countries based on a specified calling code or part of it.
#' The input can be a root calling code, suffix, or a full calling code, and the function is case-insensitive.
#'
#' @param call_code A character string representing the calling code, root calling code, or suffix. The input is case-insensitive.
#'
#' @param geometry A logical value indicating whether to include geographic boundary data. Defaults to `FALSE`.
#'                 If `TRUE`, the function performs a left join with boundary data and returns a spatial `sf` data frame.
#'
#' @return A data frame containing the information of countries corresponding to the specified calling code, root, or suffixes.
#'         If `geometry = TRUE`, the result will include an additional column for geographic boundaries (as spatial features).
#'
#' @note The function utilizes the pre-loaded `restcountries_data` dataset. Ensure that this dataset is loaded before invoking the function.
#'       If `geometry = TRUE`, the function joins with the `world_administrative_boundaries` dataset, which must also be pre-loaded.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example usage: Find country information by root calling code
#' us_info <- get_country_by_calling_code("+1")
#' print(us_info)
#'
#' # Example usage: Find country information by calling code suffix
#' uk_info <- get_country_by_calling_code("44")
#' print(uk_info)
#'
#' # Example usage: Find country information by full calling code
#' india_info <- get_country_by_calling_code("+91")
#' print(india_info)
#' }
#'@importFrom dplyr %>% filter collect all_of distinct left_join
#'@importFrom stringr str_to_lower
#'
get_country_by_calling_code <- function(call_code, geometry = FALSE) {
  call_code_lower <- str_to_lower(call_code)

  # Start the base query
  result <- restcountries_data %>%
    filter(
      str_to_lower(root) == call_code_lower |
        str_to_lower(suffixes) == call_code_lower |
        str_to_lower(calling_code) == call_code_lower
    ) %>%
    select(
      cca3,
      cca2,
      common_name,
      official_name,
      capital,
      region,
      subregion,
      continents,
      currencies,
      root,
      suffixes,
      calling_code,
      lat,
      lon
    ) %>%
    distinct()

  # If geometry is TRUE, perform a left join with the world_administrative_boundaries
  if (geometry) {
    result <- result %>%
      left_join(world_administrative_boundaries, by = "cca3")
  }

  # Check if result is empty and provide a warning message
  if (nrow(result) == 0) {
    warning("Sorry, no data found for the provided calling code.")
    return(NULL)
  } else {
    return(result)
  }
}

