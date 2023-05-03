library(readxl)
library(tidyverse)
library(janitor)
library(countrycode)

clist_avail <- c("Antigua and Barbuda","Dominica","Grenada", "St. Kitts and Nevis", "St. Lucia","St. Vincent and the Grenadines", "Montserrat", "Anguilla") %>% countrycode(., "country.name", "country.name")
clist_iso2c <- countrycode(clist_avail, "country.name", "iso2c")
clist_iso3c <- countrycode(clist_avail, "country.name", "iso3c")

current_year <- as.numeric(format(Sys.Date(), format="%Y")) - 1

# UNODC xlsx does not work with Github Actions, run this script when new UNODC is added
# Homicide here: https://dataunodc.un.org/dp-intentional-homicide-victims (click download dataset)
# Robbery here: https://dataunodc.un.org/dp-crime-violent-offences (click download dataset)

homicide_dat <- read_excel("./additional data storage/data_cts_intentional_homicide.xlsx", skip=2) %>%
  clean_names() %>%
  filter(age == "Total", sex == "Total", unit_of_measurement == "Rate per 100,000 population") %>%
  select(country, year, value) %>%
  mutate(country = countrycode(country, "country.name", "country.name"),
         category = "Intentional homicides (per 100,000 people)") %>%
  filter(country %in% clist_avail)

robbery_dat <- read_excel("./additional data storage/data_cts_violent_and_sexual_crime.xlsx", skip=2) %>%
  clean_names() %>%
  filter(age == "Total", sex == "Total", unit_of_measurement == "Rate per 100,000 population", category == "Robbery") %>%
  select(country, year, value) %>%
  mutate(country = countrycode(country, "country.name", "country.name"),
         category = "Rates of police-recorded offenses (robbery) (per 100,000 population)") %>%
  filter(country %in% clist_avail)

new_dat <- rbind(homicide_dat, robbery_dat) %>%
  filter(year > 2020 & year < (current_year + 1)) %>%
  pivot_wider(names_from="year") %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  select("variable" = "category", iso3c, starts_with("20"))

fin_dat <- rbind(read_csv(paste0("data/", "updated_data", ".csv")), new_dat)

write_csv(final_values_cleaned, paste0("data/", format(Sys.Date(), "%B_%d_%Y"), ".csv"))
write_csv(final_values_cleaned, paste0("data/", "updated_data", ".csv"))


