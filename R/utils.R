## Read restcountries data
restcountries_data <- load('data/get_restcountries_data.rda')

# Define the columns to select
select_countries_columns <- c(
  "cca3",
  "cca2",
  "common_name",
  "official_name",
  "capital",
  "region",
  "subregion",
  "continents",
  "un_member",
  "landlocked",
  "timezones",
  "start_of_week",
  "car_side",
  "currencies",
  "population",
  "area",
  "root",
  "lat",
  "lon"
)
