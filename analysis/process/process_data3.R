################################################################################
#
# Description: This script cleans up the processed data, filtering out non eligible 
#              patients but keeping treated patients and combining high risk cohorts
#
# Input: /output/data/data_processed.rds
#
# Output: /output/data/data_processed_clean_test.rds
#
# Author(s): M Green
# Date last updated: 09/02/2022
#
################################################################################


# Preliminaries ----

## Import libraries
library(tidyverse)
library(here)
library(glue)
library(gt)
library(gtsummary)
library(reshape2)
library(stringr)

## Import custom user functions
source(here("analysis", "lib", "custom_functions.R"))

## Create output directory
fs::dir_create(here::here("output", "reports", "coverage", "tables"))

## Import data
data_processed <- read_rds(here::here("output", "data", "data_processed.rds"))
   
# Format data ----

## Define high risk cohorts
data_processed_hrc_matched <- data_processed %>%
  mutate(high_risk_cohort_covid_therapeutics = ifelse(is.na(treatment_date), NA, high_risk_cohort_covid_therapeutics)) %>%
  filter(!is.na(high_risk_group_nhsd_date) | !is.na(high_risk_cohort_covid_therapeutics)) %>%
  mutate(
    # Sort naming conventions
    high_risk_cohort_covid_therapeutics = str_replace(high_risk_cohort_covid_therapeutics,
                                                      "haematological diseases,stem cell transplant recipients",
                                                      "haematological diseases and stem cell transplant recipients"),
    high_risk_cohort_covid_therapeutics = str_replace(high_risk_cohort_covid_therapeutics,
                                                      "stem cell transplant recipients,haematological diseases",
                                                      "haematological diseases and stem cell transplant recipients"),
    high_risk_cohort_covid_therapeutics = str_replace(high_risk_cohort_covid_therapeutics,
                                                      "stem cell transplant recipients,haematological diseases",
                                                      "haematological diseases and stem cell transplant recipients"),
    high_risk_cohort_covid_therapeutics = str_replace(high_risk_cohort_covid_therapeutics,
                                                      "haematological malignancies",
                                                      "haematological diseases and stem cell transplant recipients"),
    
    # Find matches between elig and treated high risk cohorts
    ind_therapeutic_groups = map_chr(strsplit(high_risk_cohort_covid_therapeutics, ","), paste,collapse="|"),
    Match = str_detect(high_risk_group_nhsd_combined, ind_therapeutic_groups)
    )  %>%
  mutate(across(
    .cols = where(is.character),
    .fns = ~na_if(.x, "")
  )) %>%
  rowwise() %>%
  mutate(
    
    # Combined elig and treated high risk cohorts
    high_risk_group_combined = as.character(ifelse(Match == TRUE,
                                      paste(high_risk_group_nhsd_combined, high_risk_cohort_covid_therapeutics, sep = ","), "")),
    high_risk_group_combined = ifelse(high_risk_group_combined == "NA", "", high_risk_group_combined),
    high_risk_group_combined = as.character(paste(unique(unlist(strsplit(high_risk_group_combined, ","))), collapse = ",")),
    high_risk_group_combined_count = ifelse(high_risk_group_combined != "", str_count(high_risk_group_combined,",") + 1, NA),
    
    ## Eligible high risk cohorts
    high_risk_group_elig = as.character(ifelse((Match == FALSE & !is.na(high_risk_group_nhsd_combined) & is.na(high_risk_cohort_covid_therapeutics)),
                                  high_risk_group_nhsd_combined, high_risk_group_combined)),
    high_risk_group_combined = ifelse(high_risk_group_elig == "NA", "", high_risk_group_elig),
    high_risk_group_elig = as.character(paste(unique(unlist(strsplit(high_risk_group_elig, ","))), collapse = ",")),
    high_risk_group_elig_count = ifelse(high_risk_group_elig != "", str_count(high_risk_group_elig,",") + 1, NA),

    ## Treated high risk cohorts
    high_risk_group_treated = as.character(ifelse((Match == FALSE & !is.na(high_risk_cohort_covid_therapeutics)),
                                     high_risk_cohort_covid_therapeutics, high_risk_group_combined)),
    high_risk_group_treated = ifelse(high_risk_group_treated == "NA", "", high_risk_group_treated),
    high_risk_group_treated = as.character(paste(unique(unlist(strsplit(high_risk_group_treated, ","))), collapse = ",")),
    high_risk_group_treated_count = ifelse(high_risk_group_treated != "", str_count(high_risk_group_treated,",") + 1, NA)
    )

print(dim(data_processed_hrc_matched))
print(table(data_processed_hrc_matched$high_risk_group_combined_count))
print(table(data_processed_hrc_matched$high_risk_group_elig_count))
print(table(data_processed_hrc_matched$high_risk_group_treated_count))

## Apply eligibility and exclusion criteria
data_processed_eligible <- data_processed_hrc_matched %>%
   filter(
    # Alive and registered
    has_died == 0,
    registered_eligible == 1 | registered_treated == 1,

    # Apply eligibility criteria
    covid_test_positive == 1,
    covid_positive_previous_30_days != 1,
    (tb_postest_treat <= 5 & tb_postest_treat >= 0) | is.na(tb_postest_treat),
    !is.na(high_risk_group_elig),

    # Apply exclusion criteria
    is.na(covid_hospital_admission_date) | covid_hospital_admission_date < (elig_start - 30) & covid_hospital_admission_date > (elig_start),
    age >= 12,

    # Only eligible patients
    !is.na(elig_start),
  ) %>%
  mutate(eligibility_status = "Eligible")

print(dim(data_processed_eligible))
print(table(data_processed_eligible$Match))

## Include treated patients not flagged as eligible
data_processed_treated <- data_processed_hrc_matched %>%
  filter(
    # Treated but non-eligible patients
    !is.na(treatment_date),
    !(patient_id %in% unique(data_processed_eligible$patient_id)),
    ) %>%
  mutate(eligibility_status = "Treated")

print(dim(data_processed_treated))
print(table(data_processed_treated$Match))

# data_processed_combined <- rbind(data_processed_eligible, data_processed_treated)
# 
# ## Exclude patients issued more than one treatment within two weeks
# dup_ids <- data_processed_combined %>%
#   select(patient_id, treatment_date, sotrovimab_covid_therapeutics, molnupiravir_covid_therapeutics, casirivimab_covid_therapeutics) %>%
#   filter(!is.na(treatment_date)) %>%
#   mutate(sotrovimab_covid_therapeutics = as.Date(sotrovimab_covid_therapeutics, origin="1970-01-01"),
#          molnupiravir_covid_therapeutics = as.Date(molnupiravir_covid_therapeutics, origin="1970-01-01"),
#          casirivimab_covid_therapeutics = as.Date(casirivimab_covid_therapeutics, origin="1970-01-01"),
#          sot_mol_diff = as.numeric(sotrovimab_covid_therapeutics - molnupiravir_covid_therapeutics),
#          sot_cas_diff = as.numeric(sotrovimab_covid_therapeutics - casirivimab_covid_therapeutics),
#          mol_cas_diff = as.numeric(molnupiravir_covid_therapeutics - casirivimab_covid_therapeutics)) %>%
#   melt(id.var = "patient_id", measure.vars = c("sot_mol_diff", "sot_cas_diff", "mol_cas_diff")) %>%
#   filter(!is.na(value),
#          value <= 14 | value >= -14) %>%
#   group_by(patient_id) %>%
#   arrange(patient_id) 
# 
# data_processed_clean <- data_processed_combined %>%
#   subset(!(patient_id %in% unique(dup_ids$patient_id)))


# Save dataset(s) ----
write_rds(data_processed_hrc_matched, here::here("output", "data", "data_processed_clean_test.rds"), compress = "gz")
#write_rds(data_processed_clean, here::here("output", "data", "data_processed_clean_test.rds"), compress = "gz")


