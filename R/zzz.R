# zzz.R

# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c(
  "currencies", "cca2", "cca3", "common_name", "capital",
  "continents", "currency_name", "currency_symbol", "lat", "lon",
  "region", "subregion", "root", "suffixes", "calling_code",
  "official_name"
))

# Load the restcountries data when the package is loaded
load("data/restcountries_tidy_data.rda")
restcountries_data <- restcountries_tidy_data

# Define the columns to select for country information
select_countries_columns <- c(
  "cca3", "cca2", "common_name", "official_name", "capital",
  "region", "subregion", "continents", "un_member",
  "landlocked", "timezones", "start_of_week", "car_side",
  "currencies", "population", "area", "root", "lat", "lon"
)


# Load the world administrative boundaries data when the package is loaded
load("data/world_administrative_boundaries.rda")
