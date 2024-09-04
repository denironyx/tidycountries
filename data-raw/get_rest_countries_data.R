# Utility functions

# Function to unnest columns and handle the list columns
unnest_columns <- function(df) {
  df %>%
    unnest_wider(tld, names_sep = "") %>%
    unnest_longer(capital, indices_include = FALSE) %>%
    unnest_wider(capital_info, names_sep = "_") %>%
    unnest_longer(continents, indices_include = FALSE) %>%
    mutate(timezones = map_chr(timezones, ~paste(., collapse = ", "))) %>%
    unnest_longer(suffixes, indices_include = FALSE) %>%
    unnest_wider(latlng, names_sep = "_")
}

# Function to process the currencies column into currency_name and currency_symbol
process_currencies <- function(df) {
  df %>%
    mutate(
      currency_name = map_chr(currencies, ~ {
        currency_list <- unlist(.x) # Flatten the inner list
        if (length(currency_list) > 0) {
          currency_list[1] # Extract the currency name
        } else {
          NA_character_
        }
      }),
      currency_symbol = map_chr(currencies, ~ {
        currency_list <- unlist(.x) # Flatten the inner list
        if (length(currency_list) > 1) {
          currency_list[2] # Extract the currency symbol
        } else {
          NA_character_
        }
      }),
      currencies = map_chr(currencies, ~ {
        currency_list <- unlist(.x) # Flatten the inner list
        if (length(currency_list) > 0) {
          paste(currency_list, collapse = " ") # Combine name and symbol
        } else {
          NA_character_
        }
      })
    )
}

# Function to consolidate language columns into a single 'languages' column
process_languages <- function(df) {
  language_cols <- grep("^languages\\.", names(df), value = TRUE)

  df %>%
    unite("languages", all_of(language_cols), sep = ", ", na.rm = TRUE) %>%
    mutate(languages = na_if(languages, ""))  # Convert empty strings back to NA
}


# Function to rename and finalize columns
finalize_df <- function(df) {
  df %>%
    select(-tld2, -tld3, -tld4, -tld5) %>%
    mutate(calling_code = paste0(root, suffixes)) %>%
    rename(
      capital_lat = capital_info_1,
      capital_lon = capital_info_2,
      lat = latlng_1,
      lon = latlng_2,
      tld = tld1
    )
}


# Main processing function
process_country_data <- function(df) {
  df %>%
    select(
      tld = tld,
      common_name = name.common,
      official_name = name.official,
      cca2,
      cca3,
      fifa,
      independent,
      status,
      un_member = unMember,
      region,
      subregion,
      population,
      capital,
      capital_info = capitalInfo.latlng,
      continents,
      latlng,
      landlocked,
      borders,
      area,
      start_of_week = startOfWeek,
      timezones,
      root = idd.root,
      suffixes = idd.suffixes,
      car_side = car.side,
      googlemaps = maps.googleMaps,
      openstreetmaps = maps.openStreetMaps,
      flags_png = flags.png,
      flags_svg = flags.svg,
      flags_alt = flags.alt,
      currencies,
      starts_with("languages.") # select all language
    ) %>%
    unnest_columns() %>%
    process_currencies() %>%
    process_languages() %>%
    finalize_df()
}

parsed_data <- jsonlite::fromJSON('data-raw/countriesV3.1.json', flatten = TRUE)


# Use the function to process your dataframe
restcountries_tidy_data <- process_country_data(parsed_data)


# Specify the path to the /data directory
save(restcountries_tidy_data, file = "data/restcountries_tidy_data.rda")

#
#
# saveRDS(df_processed, 'data/get_restcountries_data.rds')
#
# save(df_processed, file='data/get_restcountries_data.rda')

