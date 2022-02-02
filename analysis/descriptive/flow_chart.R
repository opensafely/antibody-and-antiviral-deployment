################################################################################
#
# Description: This script imports processed data and creates indicator 
#              variables for each potential outcome combination of interest
#              and creates a metadata df that describes the cohort
#
# Input: /output/data/data_processed.csv
#
# Output: /output/data_properties/data_processed*.txt
#
# Author(s): M. Green
# Date last updated: 02/02/2022
#
################################################################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('here')
library('glue')

## Import command-line arguments
args <- commandArgs(trailingOnly = TRUE)

## Create output directories
dir.create(here("output", "data"), showWarnings = FALSE, recursive = TRUE)

## Import processed data
data_processed <-  read_rds(here::here("output", "data", "data_processed.rds"))

## Format data
dup_ids <- data_processed %>%
  group_by(patient_id) %>%
  summarise(count = n()) %>%
  filter(count > 1)

data_processed_clean <- data_processed %>%
  mutate(high_risk_group_nhsd = ifelse(is.na(high_risk_group_nhsd), "Non-digital", high_risk_group_nhsd),
         elig_start = as.Date(ifelse(is.na(elig_start) & high_risk_group_nhsd == "Non-digital", treatment_date, elig_start), origin = "1970-01-01"))

# Exclusion criteria ----
data_criteria <- data_processed_clean %>%
  mutate(
    patient_id,
    has_positive_covid_test = (covid_test_positive == 1),
    no_positive_covid_test_previous_30_days = (covid_positive_previous_30_days != 1),
    treated_within_10_days = ((tb_postest_treat <= 10 & tb_postest_treat >= 0) | is.na(tb_postest_treat)),
    high_risk_group = !is.na(high_risk_group_nhsd),
    no_covid_hospital_addmission_last_30_days = (is.na(covid_hospital_addmission_date) | 
                                                   covid_hospital_addmission_date < (elig_start - 30) & 
                                                   covid_hospital_addmission_date >= (elig_start + 1)),
    aged_over_12 = (age >= 12),
    with_elig_start_date = !is.na(elig_start),
    not_duplicated_entries = !(patient_id %in% dup_ids$patient_id),
    
    include = (
      has_positive_covid_test & 
        no_positive_covid_test_previous_30_days & 
        treated_within_10_days & 
        high_risk_group & 
        no_covid_hospital_addmission_last_30_days &
        aged_over_12 &
        with_elig_start_date &
        not_duplicated_entries)
  )

# Flowchart data
data_flowchart <- data_criteria %>%
  transmute(
    c0_all = TRUE,
    c1_has_positive_covid_test = c0_all & has_positive_covid_test,
    c2_no_positive_covid_test_previous_30_days = c0_all & has_positive_covid_test & no_positive_covid_test_previous_30_days,
    c3_treated_within_10_days = c0_all & has_positive_covid_test & no_positive_covid_test_previous_30_days & treated_within_10_days,
    c4_high_risk_group = c0_all & has_positive_covid_test & no_positive_covid_test_previous_30_days & treated_within_10_days & high_risk_group,
    c5_no_covid_hospital_addmission_last_30_days = c0_all & has_positive_covid_test & no_positive_covid_test_previous_30_days & 
      treated_within_10_days & high_risk_group & no_covid_hospital_addmission_last_30_days,
    c6_aged_over_12 = c0_all & has_positive_covid_test & no_positive_covid_test_previous_30_days & 
      treated_within_10_days & high_risk_group & no_covid_hospital_addmission_last_30_days & aged_over_12,
    c7_with_elig_start_date = c0_all & has_positive_covid_test & no_positive_covid_test_previous_30_days & 
      treated_within_10_days & high_risk_group & no_covid_hospital_addmission_last_30_days & aged_over_12 & with_elig_start_date,
    c8_not_duplicated_entries = c0_all & has_positive_covid_test & no_positive_covid_test_previous_30_days & 
      treated_within_10_days & high_risk_group & no_covid_hospital_addmission_last_30_days & aged_over_12 & 
      with_elig_start_date & not_duplicated_entries
  ) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=everything(),
    names_to="criteria",
    values_to="n"
  ) %>%
  mutate(
    n_exclude = lag(n) - n,
    pct_exclude = n_exclude/lag(n),
    pct_all = n / first(n),
    pct_step = n / lag(n),
  )


# Save dataset as .csv files ----
write_csv(data_flowchart, here("output", "data", "flowchart.csv"))

