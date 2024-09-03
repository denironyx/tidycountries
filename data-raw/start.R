library(jsonlite)
library(tidyverse)
library(httr)
library(listviewer)

# read the raw json into R

raw_json <- read_json('data-raw/countriesV3.1.json')
View(raw_json)

listviewer::jsonedit(raw_json)

raw_json %>% glimpse()

parsed_data <- jsonlite::fromJSON('data-raw/countriesV3.1.json', flatten = TRUE)

parsed_data %>% glimpse()

df <- parsed_data %>%  head(n = 20)

## what is ccn3 learn more about it and cca2-3
df %>%
  select(
    tld = tld,
    common_name = name.common,
    official_name = name.official,
    cca2,
    cca3,
    cioc,
    fifa,
    independent,
    status,
    un_menber = unMember,
    region,
    subregion,
    population,
    capital, #unnest the list
    capital_info = capitalInfo.latlng, # unnest the list
    continents,
    latlng, # list
    landlocked,
    borders,
    area,
    start_of_week = startOfWeek,
    timezones,
    suffixes = idd.suffixes,
    root = idd.root,
    car_side = car.side,
    googlemaps = maps.googleMaps,
    openstreemap = maps.openStreetMaps,
    flags_png = flags.png,
    flags_svg = flags.svg,
    flags_alt = flags.alt,
    currencies
  ) %>%
  mutate(timezones = map_chr(timezones, ~paste(., collapse = ", "))) %>%
  unnest_wider(tld, names_sep = "") %>% #drop tld2 and rename tld1
  unnest_longer(capital, indices_include = FALSE) %>%
  unnest_wider(capital_info, names_sep = "_") %>% #clean this up
  unnest_longer(continents, indices_include = FALSE) %>%
  unnest_longer(suffixes, indices_include = FALSE) %>%
  unnest_wider(latlng, names_sep = "_") %>%
  mutate(currencies = map_chr(currencies, ~ {
    currency_list <- unlist(.x) # flatten the inner list
    if(length(currency_list) > 0){
      paste(currency_list, collapse = " ")# combine the name and symbols
    } else {
      NA_character_
    }
  }))
