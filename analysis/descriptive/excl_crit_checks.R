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
data_processed <- read_rds(here::here("output", "data", "data_processed.rds"))

## Check exclusion criteria
n <- nrow(data_processed)

n_not_excluded <- 
  data_processed %>%
  # because treated people are added to study pop regardless of whether they 
  # are eligible or not, eligible people are selected only
  # this filter equals the filter used in the data_process.R script, see:
  # https://github.com/opensafely/antibody-and-antiviral-deployment/blob/main/analysis/process/process_data.R#L385-L396
  filter(
    # Alive and registered
    has_died == 0,
    registered_eligible == 1 | registered_treated == 1,
    
    # Overall eligibility criteria
    covid_test_positive == 1,
    covid_positive_previous_30_days != 1,
    !is.na(high_risk_group_nhsd_combined) | high_risk_group_nhsd_combined != "NA",
    !is.na(elig_start)) %>%
  # filter the people excluded that presumably should not have been
  # All people with primary_covid_hospital_discharge_date not equal to NA are
  # filtered out in the original filter, but this may have been too strict.
  # We may want to add those with a non NA primary_covid_hospital_discharge_date
  # if that date is more than 30 days before elig_start or after elig_start
  filter(!is.na(primary_covid_hospital_discharge_date) & 
           (primary_covid_hospital_discharge_date < (elig_start - 30) | 
              primary_covid_hospital_discharge_date > elig_start)) %>%
  nrow()

n_not_excluded_v2 <- 
  data_processed %>%
  # because treated people are added to study pop regardless of whether they 
  # are eligible or not, eligible people are selected only
  # this filter equals the filter used in the data_process.R script, see:
  # https://github.com/opensafely/antibody-and-antiviral-deployment/blob/main/analysis/process/process_data.R#L385-L396
  filter(
    # Alive and registered
    has_died == 0,
    registered_eligible == 1 | registered_treated == 1,
    
    # Overall eligibility criteria
    covid_test_positive == 1,
    covid_positive_previous_30_days != 1,
    !is.na(high_risk_group_nhsd_combined) | high_risk_group_nhsd_combined != "NA",
    !is.na(elig_start)) %>%
  # filter the people excluded that presumably should not have been
  # filtered out in the original filter, but this may have been too strict.
  # this is version 2, only filtering those with a discharge date more than 30
  # days before elig_start
  filter(!is.na(primary_covid_hospital_discharge_date) & 
           primary_covid_hospital_discharge_date < (elig_start - 30)) %>%
  nrow()

## Create output
output <- 
  tibble(
    n = n,
    n_not_excluded = n_not_excluded,
    n_not_excluded_v2 = n_not_excluded_v2
  )

## Save output
fs::dir_create(here::here("output", "data_properties"))
write_csv(
  output,
  here::here("output", "data_properties", "excl_crit.csv"))
