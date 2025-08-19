# 2. Ukrainians: Temporary Protection Directive

# load packages
library(eurostat)
library(tidyverse)
library(skimr)
library(countrycode)

# find data id
search_eurostat("Decisions granting temporary protection")

# refugees from Ukraine 2022: Temporary protection (annual) -------------------- 
# need annual aggregated data for 2022, as the monthly data is way off
data_Ukraine2022 <- get_eurostat("migr_asytpfa", time_format = "num")

data_Ukraine2022 <- data_Ukraine2022 %>% 
  filter(values > 0) %>% 
  dplyr::rename(destination = geo, 
                origin = citizen) %>% 
  # create labels for the following vars:
  mutate(age = factor(age, 
                      levels = c("TOTAL", "Y_LT14", "Y14-17", "Y_LT18", "Y18-34", "Y35-64", "Y_GE65", "UNK"),
                      labels = c("All", "Under 14", "14-17", "Under 18", "18-34", "35-64", "65 and over", "Unknown"),
                      ordered = TRUE),
         gender = factor(sex,
                         levels = c("F", "M", "T", "UNK"),
                         labels = c("Female", "Male", "All", "Unknown")),
         unit = NULL) %>% 
  mutate(decision = "Temporary protection")

# custom names
data_Ukraine2022 <- data_Ukraine2022 %>% 
  mutate(origin_country = ifelse(data_Ukraine2022$origin %in% custom_mapping$code,
                                 custom_mapping$name[match(data_Ukraine2022$origin, custom_mapping$code)], 
                                 countrycode(data_Ukraine2022$origin, origin = "iso2c", destination = "country.name")),
         destination_country = ifelse(data_Ukraine2022$destination %in% custom_mapping$code,
                                      custom_mapping$name[match(data_Ukraine2022$destination, custom_mapping$code)], 
                                      countrycode(data_Ukraine2022$destination, origin = "iso2c", destination = "country.name"))) %>% 
  mutate(origin = origin_country, 
         destination = destination_country) %>% 
  select(-c(sex))

# rename time variable after Eurostat API update
data_Ukraine2022 <- data_Ukraine2022 %>% rename(time = TIME_PERIOD)
skim(data_Ukraine2022)

# refugees from Ukraine 2023: Temporary protection (monthly) --------------------
# get data
data_Ukraine <- get_eurostat("migr_asytpfm", 
                             time_format = "date")

# clean data
data2 <- data_Ukraine %>%
  filter(values > 0) %>% 
  dplyr::rename(destination = geo, 
                origin = citizen) %>% 
  
  # create labels for the following vars:
  mutate(age = factor(age, 
                      levels = c("TOTAL", "Y_LT14", "Y14-17", "Y_LT18", "Y18-34", "Y35-64", "Y_GE65", "UNK"),
                      labels = c("All", "Under 14", "14-17", "Under 18", "18-34", "35-64", "65 and over", "Unknown"),
                      ordered = TRUE),
         gender = factor(sex,
                      levels = c("F", "M", "T", "UNK"),
                      labels = c("Female", "Male", "All", "Unknown")),
         unit = NULL) %>% 
  mutate(decision = "Temporary protection") # all rows in this dataframe are on temporary protection for Ukrainians


# add country names from custom_mapping (created in 1_load_flowdata_annual)
data2 <- data2 %>% 
  mutate(origin_country = ifelse(data2$origin %in% custom_mapping$code,
                                 custom_mapping$name[match(data2$origin, custom_mapping$code)], 
                                 countrycode(data2$origin, origin = "iso2c", destination = "country.name")),
         destination_country = ifelse(data2$destination %in% custom_mapping$code,
                                      custom_mapping$name[match(data2$destination, custom_mapping$code)], 
                                      countrycode(data2$destination, origin = "iso2c", destination = "country.name")))

# rename TIME
data2 <- data2 %>% rename(time = TIME_PERIOD)

# filter 2023 Jan-Dec!!
data2 <- data2 %>% 
  filter(time >= as.Date("2023-01-01") & 
           time <= as.Date("2023-12-30"))


# ANNUAL: merge Ukraine data with other asylum datasets ----------------------------------------------------
# 1: recode time into annual Ukraine data before merging with other asylum data
library(lubridate)
str(data2)
# convert 'time' to year and aggregate 'values' by year in data2
data2_annual <- data2 %>% 
  mutate(time = year(time)) %>%
  group_by(origin, gender, age, destination, time, origin_country, destination_country, decision) %>%
  summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>% 
  mutate(origin = origin_country, 
         destination = destination_country) %>% 
  filter(time == 2023) # remove 2022 data, which is wrong

## checks out: 147 500 to Germany Jan-Jun 2023.
data2_annual %>% 
  filter(destination == "Germany", origin == "Total",
         age == "All", gender =="All")

# rbind 2022 and 2023
data_Ukraine2022$freq <- NULL
data2_annual <- rbind(data2_annual, data_Ukraine2022)

# export data
setwd("~/Library/CloudStorage/OneDrive-OsloMet/R/GOVREIN/Eurostat GOVREIN/API output data")
export(data2_annual, file = "UKR_tempdata_annual.RData")



