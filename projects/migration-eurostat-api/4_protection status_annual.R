# 4. PROTECTION DECISIONS: 
# First instance decisions - 4 types of protection status grants ---------------

# read data directly from the eurostat package
library(eurostat)
library(tidyverse)

ids <- search_eurostat("decisions on applications")
# first instance decisions: 4 types of protection status grants
asylum_protectionstatus <- get_eurostat("migr_asydcfsta", 
                              time_format = "num")

# clean data -------------------------------------------------------------------
data <- asylum_protectionstatus %>%
  filter(values > 0) %>% 
  dplyr::rename(origin = citizen,
                destination = geo) %>% 
  
  # create labels for the following vars:
  mutate(decision = factor(decision,
                           levels = c("TOTAL", "TOTAL_POS", "GENCONV", "HUMSTAT", "SUB_PROT", "REJECTED"),
                           labels = c("Total", "Total positive decisions", "Geneva Convention status", "Humanitarian status", "Subsidiary protection status", "Rejected")), 
         age = factor(age, 
                      levels = c("TOTAL", "Y_LT14", "Y14-17", "Y_LT18", "Y18-34", "Y35-64", "Y_GE65", "UNK"),
                      labels = c("All", "Under 14", "14-17", "Under 18", "18-34", "35-64", "65 and over", "Unknown"),
                      ordered = TRUE),
         gender = factor(sex,
                      levels = c("T", "F", "M", "UNK"),
                      labels = c("All", "Female", "Male", "Unknown")),
         unit = NULL, sex = NULL)

# print(object.size(asylum_protectionstatus), units = "MB") # 1076 MB
# print(object.size(data), units = "MB") # 54 MB


# add country names ---------------------------------------------------------------------------------
library(countrycode)
# all won't match, add the country names manually:
custom_mapping <- data.frame(
  code = c("EL", "EU27_2020", "EU28", "EXT_EU27_2020", "EXT_EU28", "STLS", "TOTAL", "UK", "UK_OCT", "UNK", "XK"),
  name = c("Greece", "EU27 (from 2020)", "EU28", "Extra-EU27 (from 2020)", "Extra-EU28", "Statistical territories", "Total", "United Kingdom", "UK (October)", "Unknown", "Kosovo"))

## annual asylum data -------------------------------------------------------------------------------
# add full names to data. the following code will produce warnings -- not error, unless warning settings are triggered. reset the settings: 
# options(warn = 0) # reset warnings settings if necessary
data <- data %>%
  mutate(origin_country = ifelse(data$origin %in% custom_mapping$code,
                                 custom_mapping$name[match(data$origin, custom_mapping$code)], 
                                 countrycode(data$origin, origin = "iso2c", destination = "country.name")),
         destination_country = ifelse(data$destination %in% custom_mapping$code,
                                      custom_mapping$name[match(data$destination, custom_mapping$code)], 
                                      countrycode(data$destination, origin = "iso2c", destination = "country.name")))





# merge with data on temporary protection and resettled refugees ------------------------------------
data2_annual <- rio::import("UKR_tempdata_annual.RData")
data_resettled <- rio::import("resettled_annual.RData")

annual_decisions <- bind_rows(data, 
                              data2_annual, # load_data_temporary_protection_Ukraine
                              data_resettled) # load_data_resettledpersons
skimr::skim(annual_decisions)
# factorise
# annual_decisions[,1:5] <- lapply(annual_decisions[,1:5], factor)

# export merged annual data
setwd("~/Library/CloudStorage/OneDrive-OsloMet/R/GOVREIN/Eurostat GOVREIN/API output data")
rio::export(annual_decisions, file = "MERGED_annual.RData")

