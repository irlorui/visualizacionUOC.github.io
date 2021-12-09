library(dplyr)
library(tidyr)
library(tidyverse)

europeanUnion <- c("AUT", "BEL", "BGR", "HRV", "CYP",
                   "CZE", "DNK", "EST", "FIN", "FRA", 
                   "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", 
                   "LTU", "LUX", "MLT", "NLD", "POL", 
                   "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

commonURL <- "https://public.flourish.studio/country-flags/svg/"
countryURL <- c("at", "be", "bg", "hr", "cy", "cz", "dk", "ee", "fi", "fr", "de", "gr",
                "hu", "ie", "it", "lv", "lt", "lu", "mt", "nl", "pl", "pt", "ro", 
                "sk", "si", "es", "se")
url <- paste0(commonURL, countryURL, ".svg")

country_vax <- read_csv("country_vaccinations.csv",
                        col_types = cols(date = col_date(format = "%Y-%m-%d"),
                                         total_vaccinations = col_double(),
                                         daily_vaccinations = col_skip(),
                                         people_vaccinated = col_skip(), 
                                         people_vaccinated_per_hundred = col_skip(),
                                         people_fully_vaccinated_per_hundred = col_skip(),
                                         people_fully_vaccinated = col_skip(), 
                                         daily_vaccinations_raw = col_skip(), 
                                         total_vaccinations_per_hundred = col_skip(),
                                         daily_vaccinations_per_million = col_skip(),
                                         vaccines = col_skip(), source_name = col_skip(),
                                         source_website = col_skip())) %>%
  filter(iso_code %in% europeanUnion) %>%
  mutate_at(vars(country, iso_code), funs(as.factor(.))) %>%
  filter(date >= strptime("2020-12-27", format = "%Y-%m-%d"))%>%
  arrange(date) 
  # mutate_at(vars(total_vaccinations), ~replace(., is.na(.), 0)) 
  
country_vax2 <- pivot_wider(country_vax, 
                            names_from = date, 
                            values_from = total_vaccinations) %>%
  arrange(country) %>%
  add_column(imageURL = url) %>%
  relocate(imageURL, .after = iso_code)

write.csv(country_vax2, file="country_vax.csv")
