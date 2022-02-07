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
library(tidyverse)
library(here)
library(glue)
library(reshape2)

## Import command-line arguments
args <- commandArgs(trailingOnly = TRUE)

## Create output directories
dir.create(here("output", "data"), showWarnings = FALSE, recursive = TRUE)

## Import processed data
data_processed <-  read_rds(here::here("output", "data", "data_processed.rds"))

## Format data
dup_ids <- data_processed %>%
  select(patient_id, treatment_date, sotrovimab_covid_therapeutics, molnupiravir_covid_therapeutics, casirivimab_covid_therapeutics) %>%
  filter(!is.na(treatment_date)) %>%
  mutate(sotrovimab_covid_therapeutics = as.Date(sotrovimab_covid_therapeutics, origin="1970-01-01"),
         molnupiravir_covid_therapeutics = as.Date(molnupiravir_covid_therapeutics, origin="1970-01-01"),
         casirivimab_covid_therapeutics = as.Date(casirivimab_covid_therapeutics, origin="1970-01-01"),
         sot_mol_diff = as.numeric(sotrovimab_covid_therapeutics - molnupiravir_covid_therapeutics),
         sot_cas_diff = as.numeric(sotrovimab_covid_therapeutics - casirivimab_covid_therapeutics),
         mol_cas_diff = as.numeric(molnupiravir_covid_therapeutics - casirivimab_covid_therapeutics)) %>%
  melt(id.var = "patient_id", measure.vars = c("sot_mol_diff", "sot_cas_diff", "mol_cas_diff")) %>%
  filter(!is.na(value),
         value <= 14 | value >= -14) %>%
  group_by(patient_id) %>%
  arrange(patient_id) 

# Exclusion criteria ----
data_criteria <- data_processed %>%
  mutate(
    patient_id,
    alive = (has_died == 0),
    registered = (registered_eligible == 1 | registered_treated == 1),
    has_positive_covid_test = (covid_test_positive == 1),
    no_positive_covid_test_previous_30_days = (covid_positive_previous_30_days != 1),
    high_risk_group = !is.na(high_risk_group_nhsd),
    no_covid_hospital_admission_last_30_days = (is.na(covid_hospital_admission_date) | 
                                                   covid_hospital_admission_date < (elig_start - 30) & 
                                                   covid_hospital_admission_date > (elig_start)),
    aged_over_12 = (age >= 12),
    treated_within_5_days = ((tb_postest_treat <= 5 & tb_postest_treat >= 0) | is.na(tb_postest_treat)),
    not_duplicated_entries = !(patient_id %in% dup_ids$patient_id),
    
    include = (
      alive &
        registered & 
        has_positive_covid_test & 
        no_positive_covid_test_previous_30_days & 
        treated_within_5_days & 
        high_risk_group & 
        no_covid_hospital_admission_last_30_days &
        aged_over_12 &
        not_duplicated_entries)
  )

# Flowchart data
data_flowchart <- data_criteria %>%
  transmute(
    c0_all = TRUE,
    c1_alive_and_registered = c0_all & alive & registered,
    c2_has_positive_covid_test = c0_all & alive & registered & has_positive_covid_test,
    c3_no_positive_covid_test_previous_30_days = c0_all & alive & registered & has_positive_covid_test & no_positive_covid_test_previous_30_days,
    c4_high_risk_group = c0_all & alive & registered & has_positive_covid_test & no_positive_covid_test_previous_30_days & high_risk_group,
    c5_no_covid_hospital_admission_last_30_days = c0_all & alive & registered & has_positive_covid_test & no_positive_covid_test_previous_30_days & 
      high_risk_group & no_covid_hospital_admission_last_30_days,
    c6_aged_over_12 = c0_all & alive & registered & has_positive_covid_test & no_positive_covid_test_previous_30_days & 
      high_risk_group & no_covid_hospital_admission_last_30_days & aged_over_12,
    c7_treated_within_5_days = c0_all & alive & registered & has_positive_covid_test & no_positive_covid_test_previous_30_days & 
      high_risk_group & no_covid_hospital_admission_last_30_days & aged_over_12 & treated_within_5_days,
    c8_not_duplicated_entries = c0_all & alive & registered & has_positive_covid_test & no_positive_covid_test_previous_30_days & 
      high_risk_group & no_covid_hospital_admission_last_30_days & aged_over_12 & 
      treated_within_5_days & not_duplicated_entries
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

