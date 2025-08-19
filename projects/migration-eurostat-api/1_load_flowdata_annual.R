# 1. load annual flow data: APPLICATIONS LODGED --------------------------------

# load data --------------------------------------------------------------------
library(rio)
library(eurostat)
library(tidyverse)
asylum_y <- get_eurostat("migr_asyappctza", # not including temp protection!
                         time_format = "num")
skim(asylum_y)
# clean data
annual_applications <- asylum_y %>%
  filter(values > 0) %>% 
  dplyr::rename(origin = citizen,
                destination = geo, 
                time = TIME_PERIOD) %>% 
  
  # create labels for the following vars:
  mutate(asyl_app = factor(asyl_app,
                           levels = c("ASY_APP", "NASY_APP", "SSEQ"),
                           labels = c("All asylum applicants", "First-time applicants", "Subsequent asylum applicants")), 
         age = factor(age, 
                      levels = c("TOTAL", "Y_LT14", "Y14-17", "Y_LT18", "Y18-34", "Y35-64", "Y_GE65", "UNK"),
                      labels = c("All", "Under 14", "14-17", "Under 18", "18-34", "35-64", "65 and over", "Unknown"),
                      ordered = TRUE),
         gender = factor(sex,
                      levels = c("F", "M", "T", "UNK"),
                      labels = c("Female", "Male", "All", "Unknown")),
         unit = NULL)

# add country names ------------------------------------------------------------
library(countrycode)
# all won't match, add the country names manually:
custom_mapping <- data.frame(
  code = c("EL", "EU27_2020", "EU28", "EXT_EU27_2020", "EXT_EU28", "STLS", "TOTAL", "Total", "UK", "UK_OCT", "UNK", "XK"),
  name = c("Greece", "EU27 (from 2020)", "EU28", "Extra-EU27 (from 2020)", "Extra-EU28", "Statistical territories", "Total", "Total", "United Kingdom", "UK (October)", "Unknown", "Kosovo"))

## annual asylum data ----------------------------------------------------------
# add full names to data
annual_applications <- annual_applications %>%
  mutate(origin_country = ifelse(annual_applications$origin %in% custom_mapping$code,
                                 custom_mapping$name[match(annual_applications$origin, custom_mapping$code)], 
                                 countrycode(annual_applications$origin, origin = "iso2c", destination = "country.name")),
         destination_country = ifelse(annual_applications$destination %in% custom_mapping$code,
                                      custom_mapping$name[match(annual_applications$destination, custom_mapping$code)], 
                                      countrycode(annual_applications$destination, origin = "iso2c", destination = "country.name")))

## filter asylum data to relevant countries ------------------------------------
annual_applications <- annual_applications %>% 
  # filter(destination %in% c("NO", "SE", "DK", "FI", "PL", "AT", "UK", "DE")) %>% 
  mutate(destination = factor(destination),
         destination_country = factor(destination_country),
         origin = factor(origin),
         origin_country = factor(origin_country))

# export data
setwd("~/Library/CloudStorage/OneDrive-OsloMet/R/GOVREIN/Eurostat GOVREIN/API output data")
rio::export(annual_applications, file = "flowdata_annual.RData")

