################################################################################
#
# Description: This script checks the impact of excluding people hospitalised 
#              in the 30 days before a positive tests
#
# Input: /output/data/data_processed_clean.rds
#
# Output: /output/coverage/data_properties/excl_crit.csv
#
# Author(s): L Nab
# Date created: 01/09/2022
#
################################################################################


# Preliminaries ----

## Import libraries
library(readr)
library(dplyr)

## Import input
data_processed <- read_rds(here::here("output", "data", "data_processed_clean.rds"))

## Check exclusion criteria
n <- nrow(data_processed)

n_excluded <- 
  data_processed %>%
  filter(!is.na(primary_covid_hospital_discharge_date) & !(primary_covid_hospital_discharge_date >= (elig_start - 30) & 
            primary_covid_hospital_discharge_date < (elig_start))) %>% nrow()

## Create output
output <- 
  tibble(
    n = n,
    n_excluded = n_excluded
  )

## Save output
fs::dir_create(here::here("output", "data_properties"))
write_csv(
  output,
  here::here("output", "data_properties", "excl_crit.csv"))
