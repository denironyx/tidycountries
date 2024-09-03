## Function to fetch all country information from restcountries .rds
fetch_restcountries_data <- function(){
  restcountries_data
}


# Function to get country information
get_countries <- function(country_value){
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

# Function to list countries by region, subregion, or continent
get_countries_by_region <- function(country_region_value) {
  country_region_value_lower <- str_to_lower(country_region_value)

  # Filter and select columns
  result <- restcountries_data %>%
    filter(
      str_to_lower(region) == country_region_value_lower |
        str_to_lower(subregion) == country_region_value_lower |
        str_to_lower(continents) == country_region_value_lower
    ) %>%
    select(all_of(select_countries_subset)) %>%
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

#get_countries_by_region('africsa')



# Function to find countries by currency
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

#find_countries_by_currency('dollar') %>% View()


get_countries_by_calling_code <- function(call_code) {

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

