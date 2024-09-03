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
#'
#' @return A data frame with selected country information. If the input is "all", it returns data for all countries.
#'        if no match is found, a list of all available country names is printed.
#'
#' @note A data frame with selected country information. If the input is "all", it returns data for all countries.
#'       if no match is found, a list of all available country names is printed
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
#'@importFrom dplyr %>% filter collect all_of distinct
#'@importFrom stringr str_to_lower
#'
get_country_info <- function(country_value){
  # converting input to lowercase
  country_value_lower <- str_to_lower(country_value)

  # if the input is all
  if(country_value_lower == 'all'){
    return(restcountries_data) %>%
      select(all_of(select_countries_columns))
  }else{
    result <- restcountries_data %>%
      filter(
          str_to_lower(cca2) == country_value_lower |
          str_to_lower(cca3) ==  country_value_lower |
          str_to_lower(common_name) == country_value_lower
      ) %>%
      select(all_of(select_countries_columns)) %>%
      distinct()

    # check if result is empty and provide warning message
    if(nrow(result) == 0)
    {
      cat("Sorry, no data found for the provide input. \n")
      cat("Here is a list of all available country names: \n")
      all_country_names <- restcountries_data %>%
        pull(common_name) %>%
        unique() %>%
        sort()
      print(all_country_names)
    } else {
      return(result)
    }
  }
}

#' get_countries_by_region
#'
#' This function retrieves a list of countries based on a specified region, subregion, or continent.
#' The function is case-insensitive and orders the countries alphabetically by their common names.
#' If the input does not match any region, subregion, or continent, the function provides a list of all available regions, subregions, and continents.
#'
#' @param country_region_value A character string representing the region, subregion, or continent. The input is case-insensitive.
#'
#' @return A data frame containing the list of countries within the specified region, subregion, or continent, ordered alphabetically by country name.
#'         If no match is found, a warning message is displayed, and a list of all available regions, subregions, and continents is provided.
#'
#' @note The function utilizes the pre-loaded `restcountries_data` dataset. Ensure that this dataset is loaded before invoking the function.
#'       The selected columns include country codes, names, capital, region, subregion, start of the week, car side, currencies, population, latitude, and longitude.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example usage: Get a list of countries in Africa
#' africa_countries <- get_countries_by_region("Africa")
#' print(africa_countries)
#'
#' # Example usage: Get a list of countries in Western Europe (a subregion)
#' western_europe_countries <- get_countries_by_region("Western Europe")
#' print(western_europe_countries)
#'
#' # Example usage: Get a list of countries in the continent of Asia
#' asia_countries <- get_countries_by_region("Asia")
#' print(asia_countries)
#' }
#'
#'@importFrom dplyr %>% filter collect all_of distinct arrange
#'@importFrom stringr str_to_lower
#'
get_countries_by_region <- function(country_region_value) {
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

  # Check if result is empty and provide appropriate message
  if (nrow(result) == 0) {
    warning("Sorry, no data found for the provided input.")
    message("Here is a list of all available regions, subregions, and continents:")
    all_region_info <- restcountries_data %>%
      select(region, subregion, continents) %>%
      distinct() %>%
      arrange(region, subregion, continents)
    print(all_region_info)
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
#' @param input A character string representing the currency name or part of the name. The input is case-insensitive.
#'
#' @return A data frame containing the list of countries that use the specified currency, ordered alphabetically by country name.
#'         The columns include country codes (CCA2 and CCA3), common name, capital, continents, currency name, currency symbol, latitude, and longitude.
#'
#' @note The function utilizes the pre-loaded `restcountries_data` dataset. Ensure that this dataset is loaded before invoking the function.
#'       The function uses a case-insensitive regular expression to match the currency name, allowing partial matches.
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
#' dollar_countries <- get_countries_by_currency("dollar")
#' print(dollar_countries)
#'
#' # Example usage: Find all countries that use the Yen
#' yen_countries <- get_countries_by_currency("Yen")
#' print(yen_countries)
#' }
#'@importFrom dplyr %>% filter collect all_of distinct
#'@importFrom stringr str_to_lower regex str_detect
#'
get_countries_by_currency <- function(input) {
  # Convert the input currency name to lowercase
  currency_name_lower <- str_to_lower(input)

  restcountries_data %>%
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
      currency_name,
      currency_symbol,
      lat,
      lon
    ) %>%
    distinct() %>%
    arrange(common_name)
}

#' get_country_by_calling_code
#'
#' This function retrieves information about countries based on a specified calling code or part of it.
#' The input can be a root calling code, suffix, or a full calling code, and the function is case-insensitive.
#'
#' @param call_code A character string representing the calling code, root calling code, or suffix. The input is case-insensitive.
#'
#' @return A data frame containing the list of countries that match the provided calling code.
#'         The columns include country codes (CCA2 and CCA3), common name, official name, capital, region, subregion, continents,
#'         currencies, calling code details (root, suffixes, and full calling code), and geographic coordinates (latitude and longitude).
#'
#' @note The function relies on the pre-loaded `restcountries_data` dataset. Ensure that this dataset is loaded before invoking the function.
#'       The function searches across the root calling code, suffixes, and full calling code using case-insensitive matching.
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
#'@importFrom dplyr %>% filter collect all_of distinct
#'@importFrom stringr str_to_lower
#'
get_country_by_calling_code <- function(call_code) {

  call_code_lower <- str_to_lower(call_code)

  restcountries_data %>%
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
    ) %>% distinct()
}

