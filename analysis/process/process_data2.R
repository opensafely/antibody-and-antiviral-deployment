################################################################################
#
# Description: This script produces metadata, figurs and tables to go into the
#              mabs_and_antivirvals_coverage_report.rmd 
#
# Input: /output/data/data_processed.rds
#
# Output: /output/reports/coverage/tables/table_report_stats.csv
#         /output/reports/coverage/tables/table_elig_treat_redacted.csv
#         /output/reports/coverage/tables/table_demo_clinc_breakdown_redacted.csv
#         /output/reports/coverage/tables/table_high_risk_cohort_comparison.csv
#         /output/reports/coverage/figures/figure_cum_treatment_plot.png
#         /output/reports/coverage/figures/figure_cum_eligiblity_plot.png
#
# Author(s): M Green
# Date last updated: 07/02/2022
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
print(dim(data_processed))

data_processed_hrc_matched <- data_processed %>%
  mutate(high_risk_cohort_covid_therapeutics = ifelse(high_risk_cohort_covid_therapeutics == "other", NA, high_risk_cohort_covid_therapeutics)) %>%
  filter(!is.na(high_risk_group_nhsd_date) | high_risk_cohort_covid_therapeutics == "NA" | is.na(high_risk_cohort_covid_therapeutics)) %>%
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
    ) %>%
  rowwise() %>%
  mutate(
    # Combined elig and treated high risk cohorts
    high_risk_group_combined = ifelse(Match == TRUE,
                                      paste(high_risk_group_nhsd_combined, high_risk_cohort_covid_therapeutics, sep = ","), ""),
    high_risk_group_combined = paste(unique(strsplit(high_risk_group_combined, ",|\\n")[[1]]), collapse = ","),
    high_risk_group_combined_count = ifelse(high_risk_group_combined != "", str_count(high_risk_group_combined,",") + 1, NA),
    
    ## Eligible high risk cohorts
    high_risk_group_elig = ifelse((Match == FALSE & !is.na(high_risk_group_nhsd_combined)), 
                                  high_risk_group_nhsd_combined, high_risk_group_combined),
    high_risk_group_elig = paste(unique(strsplit(high_risk_group_elig, ",|\\n")[[1]]), collapse = ","),
    high_risk_group_elig_count = ifelse(high_risk_group_elig != "", str_count(high_risk_group_elig,",") + 1, NA),
    
    ## Treated high risk cohorts
    high_risk_group_treated = ifelse((Match == FALSE & !is.na(high_risk_cohort_covid_therapeutics)), 
                                     high_risk_cohort_covid_therapeutics, high_risk_group_combined),
    high_risk_group_treated = paste(unique(strsplit(high_risk_group_treated, ",|\\n")[[1]]), collapse = ","),
    high_risk_group_treated_count = ifelse(high_risk_group_treated != "", str_count(high_risk_group_treated,",") + 1, NA)
    ) %>%
  select(-ind_therapeutic_groups)

print(dim(data_processed_hrc_matched))

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
    !is.na(high_risk_group_combined),
    
    # Apply exclusion criteria
    is.na(covid_hospital_admission_date) | covid_hospital_admission_date < (elig_start - 30) & covid_hospital_admission_date > (elig_start),
    age >= 12,
    
    # Only eligible patients
    !is.na(elig_start),
  ) %>%
  mutate(eligibility_status = "Eligible") 

## Include treated patients not flagged as eligible
data_processed_treated <- data_processed_hrc_matched %>%
  filter(
    
    # Treat but non-eligible patients
    !(patient_id %in% unique(data_processed_eligible$patient_id)),
    !is.na(treatment_date),
    
    # Alive and registered
    has_died == 0,
    registered_eligible == 1 | registered_treated == 1
    ) %>%
  mutate(elig_start = as.Date(ifelse(is.na(elig_start), treatment_date, elig_start), origin = "1970-01-01"),
         eligibility_status = "Treated") 

data_processed_combined <- rbind(data_processed_eligible, data_processed_treated)

## Exclude patients issued more than one treatment within two weeks
dup_ids <- data_processed_combined %>%
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


data_processed_clean <- data_processed_combined %>%
  subset(!(patient_id %in% unique(dup_ids$patient_id)))

## Formatting variables
data_processed_clean <- data_processed_clean %>%
  mutate(
    
    # Age groups
    ageband = cut(
      age,
      breaks = c(12, 30, 40, 50, 60, 70, 80, Inf),
      labels = c("12-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
      right = FALSE),
    
    #IMD
    imd = as.character(imd),
    imd = ifelse(imd %in% c("1 most deprived", 2:4, "5 least deprived"), imd, "Unknown"),
    imd = fct_case_when(
      imd == "1 most deprived" ~ "1 most deprived",
      imd == 2 ~ "2",
      imd == 3 ~ "3",
      imd == 4 ~ "4",
      imd == "5 least deprived" ~ "5 least deprived",
      imd == "Unknown" ~ "Unknown",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    # Region
    region = as.character(region_nhs),
    region = fct_case_when(
      region == "London" ~ "London",
      region == "East of England" ~ "East of England",
      region == "East Midlands" ~ "East Midlands",
      region == "North East" ~ "North East",
      region == "North West" ~ "North West",
      region == "South East" ~ "South East",
      region == "South West" ~ "South West",
      region == "West Midlands" ~ "West Midlands",
      region == "Yorkshire and the Humber" ~ "Yorkshire and the Humber",
      #TRUE ~ "Unknown"
      TRUE ~ NA_character_)
    
    # # High risk cohort
    # high_risk_group_nhsd = as.character(high_risk_group_nhsd),
    # high_risk_group_nhsd = fct_case_when(
    #   high_risk_group_nhsd == "Down's syndrome" ~ "Down's syndrome",
    #   high_risk_group_nhsd == "Sickle cell disease" ~ "Sickle cell disease",
    #   high_risk_group_nhsd == "Patients with a solid cancer" ~ "Solid cancer",
    #   high_risk_group_nhsd == "Patients with a haematological diseases and stem cell transplant recipients" ~ "Haematological diseases and stem cell transplant recipients",
    #   high_risk_group_nhsd == "Patients with renal disease" ~ "Renal disease",
    #   high_risk_group_nhsd == "Patients with liver disease" ~ "Liver disease",
    #   high_risk_group_nhsd == "Patients with immune-mediated inflammatory disorders (IMID)" ~ "Immune-mediated inflammatory disorders",
    #   high_risk_group_nhsd == "Primary immune deficiencies" ~ "Primary immune deficiencies",
    #   high_risk_group_nhsd == "HIV/AIDS" ~ "HIV or AIDS",
    #   high_risk_group_nhsd == "Solid organ transplant recipients" ~ "Solid organ transplant recipients",
    #   high_risk_group_nhsd == "Rare neurological conditions" ~ "Rare neurological conditions",
    #   high_risk_group_nhsd == "Not deemed eligible" ~ "Not deemed eligible",
    #   #TRUE ~ "Unknown"
    #   TRUE ~ NA_character_)

  )

# Save dataset(s) ----
write_rds(data_processed_clean, here::here("output", "data", "data_processed_clean.rds"), compress = "gz")


