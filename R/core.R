## Function to fetch all country information from restcountries .rds
fetch_restcountries_data <- function(){
  restcountries_data
}


# Function to get country information
get_restcountries_info <- function(country_value){
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

