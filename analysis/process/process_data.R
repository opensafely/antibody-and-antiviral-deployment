################################################################################
#
# Description: This script imports data extracted by the cohort extractor and
#              calculates additional variables needed for subsequent anlyses 
#              (i.e., eligibility criteria window)
#
# Input: /output/data/input.csv.gz
#
# Output: /output/data/data_processed.csv
#
# Author(s): M Green
# Date last updated: 04/02/2022
#
################################################################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('gt')
library('gtsummary')
library('here')
library('tidyverse')
library('lubridate')
library('arrow')
library('reshape2')
library('dplyr')
library('readr')
library('survival')
library('survminer')

## Custom functions
source(here("analysis", "lib", "custom_functions.R"))


# Process data ----

## Read in data (don't rely on defaults)
data_extract0 <- read_csv(
  here::here("output", "data", "input.csv.gz"),
  col_types = cols_only(                      
    
    # PATIENT ID ----
    patient_id = col_integer(),
    
    # CENSORING ----
    death_date = col_date(format = "%Y-%m-%d"),
    dereg_date = col_date(format = "%Y-%m-%d"),
    
    # NEUTRALISING MONOCLONAL ANTIBODIES OR ANTIVIRALS ----
    sotrovimab_covid_therapeutics = col_date(format = "%Y-%m-%d"),
    molnupiravir_covid_therapeutics = col_date(format = "%Y-%m-%d"),
    casirivimab_covid_therapeutics = col_date(format = "%Y-%m-%d"),
    
    # ELIGIBILITY CRITERIA VARIABLES ----
    covid_test_positive = col_logical(),
    covid_test_positive_date = col_date(format = "%Y-%m-%d"),
    #covid_positive_test_type = col_character(),
    covid_positive_previous_30_days = col_logical(),
    symptomatic_covid_test = col_character(),
    covid_symptoms_snomed = col_date(format = "%Y-%m-%d"),
    high_risk_cohort_covid_therapeutics = col_character(),
    covid_hospital_admission_date = col_date(format = "%Y-%m-%d"),
    age = col_integer(),
    
    # HIGH RISK GROUPS ----
    downs_syndrome_nhsd = col_date(format = "%Y-%m-%d"),
    sickle_cell_disease_nhsd = col_date(format = "%Y-%m-%d"),
    cancer_opensafely_snomed = col_date(format = "%Y-%m-%d"),
    haematological_disease_nhsd = col_date(format = "%Y-%m-%d"), 
    ckd_stage_5_nhsd = col_date(format = "%Y-%m-%d"),
    liver_disease_nhsd = col_date(format = "%Y-%m-%d"),
    imid_nhsd = col_date(format = "%Y-%m-%d"),
    immunosupression_nhsd = col_date(format = "%Y-%m-%d"),
    hiv_aids_nhsd = col_date(format = "%Y-%m-%d"),
    solid_organ_transplant_nhsd = col_date(format = "%Y-%m-%d"),
    multiple_sclerosis_nhsd = col_date(format = "%Y-%m-%d"),
    motor_neurone_disease_nhsd = col_date(format = "%Y-%m-%d"),
    myasthenia_gravis_nhsd = col_date(format = "%Y-%m-%d"),
    huntingtons_disease_nhsd = col_date(format = "%Y-%m-%d"),
    
    # CLINICAL/DEMOGRAPHIC COVARIATES ----
    sex = col_character(),
    ethnicity_primis = col_character(),
    ethnicity_sus = col_character(),
    imd = col_character(),
    region_nhs = col_character(),
    region_covid_therapeutics = col_character()
    
  ),
)

print(dim(data_extract0))
print(dim(data_extract0 %>% 
            filter(!is.na(sotrovimab_covid_therapeutics) |
                     !is.na(molnupiravir_covid_therapeutics) |
                     !is.na(casirivimab_covid_therapeutics))))

## Fix bad dummy data
if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){
  data_extract0 <- data_extract0 %>%
    mutate(date = sample(seq(as.Date('2021/11/01'), as.Date('2022/02/01'), by="day"), nrow(data_extract0), replace = TRUE),
           covid_test_positive_date = as.Date(covid_test_positive_date, format = "%Y-%m-%d"),
           covid_test_positive_date = as.Date(ifelse(!is.na(covid_test_positive_date), date, NA), origin = "1970-01-01"),
           
           date2 = as.Date(covid_test_positive_date + sample(-1:10, dim(data_extract0)[1], replace=TRUE), origin = "1970-01-01"),
           sotrovimab_covid_therapeutics = ifelse(!is.na(sotrovimab_covid_therapeutics), date2, NA),
           molnupiravir_covid_therapeutics = ifelse(!is.na(molnupiravir_covid_therapeutics), date2, NA),
           casirivimab_covid_therapeutics = ifelse(!is.na(casirivimab_covid_therapeutics), date2, NA))
}

## Parse NAs
data_extract <- data_extract0 %>%
  mutate(across(
    .cols = where(is.character),
    .fns = ~na_if(.x, "")
  )) %>%
  mutate(across(
    .cols = c(where(is.numeric), -ends_with("_id")), #convert numeric+integer but not id variables
    .fns = ~na_if(.x, 0)
  )) %>%
  arrange(patient_id) %>%
  select(all_of((names(data_extract0))))


## Format columns (i.e, set factor levels)
data_processed <- data_extract %>%
  mutate(
    across(
      where(is.logical),
      ~.x*1L
    )) %>%
  mutate(
    
    # NEUTRALISING MONOCLONAL ANTIBODIES OR ANTIVIRALS ----
    treatment_date = as.Date(pmin(sotrovimab_covid_therapeutics, molnupiravir_covid_therapeutics, casirivimab_covid_therapeutics, na.rm = TRUE), origin = "1970-01-01"),
    treatment_type = ifelse(!is.na(sotrovimab_covid_therapeutics), "Sotrovimab", 
                            ifelse(!is.na(molnupiravir_covid_therapeutics), "Molnupiravir", 
                                   ifelse(!is.na(casirivimab_covid_therapeutics), "Casirivimab", NA))),
    
    
    # ELIGIBILITY VARIABLES ----
    
    ## Time between positive test and treatment
    tb_postest_treat = ifelse(covid_test_positive == 1, as.numeric(treatment_date - covid_test_positive_date), NA),
    
    ## Eligibility window
    rare_neurological_conditions_nhsd =  pmax(multiple_sclerosis_nhsd, motor_neurone_disease_nhsd, myasthenia_gravis_nhsd,
                                              huntingtons_disease_nhsd, na.rm = T),
    
    high_risk_group_date = pmax(downs_syndrome_nhsd, sickle_cell_disease_nhsd, cancer_opensafely_snomed,
                                haematological_disease_nhsd, ckd_stage_5_nhsd, liver_disease_nhsd, imid_nhsd,
                                immunosupression_nhsd, hiv_aids_nhsd, solid_organ_transplant_nhsd, rare_neurological_conditions_nhsd,
                                na.rm = TRUE),
    
    elig_start = as.Date(ifelse(covid_test_positive == 1 & (covid_test_positive_date >= high_risk_group_date), covid_test_positive_date, NA), origin = "1970-01-01"),
    elig_end = as.Date(elig_start + 5, origin = "1970-01-01"),
    
    # HIGH RISK GROUPS ----
    high_risk_group_nhsd = case_when(
      high_risk_group_date == downs_syndrome_nhsd ~ "Down's syndrome", 
      high_risk_group_date == sickle_cell_disease_nhsd ~ "Sickle cell disease", 
      high_risk_group_date == cancer_opensafely_snomed ~ "Patients with a solid cancer", 
      high_risk_group_date == haematological_disease_nhsd ~ "Patients with a haematological diseases and stem cell transplant recipients", 
      high_risk_group_date == ckd_stage_5_nhsd ~ "Patients with renal disease", 
      high_risk_group_date == liver_disease_nhsd ~ "Patients with liver disease", 
      high_risk_group_date == imid_nhsd ~ "Patients with immune-mediated inflammatory disorders (IMID)", 
      high_risk_group_date == immunosupression_nhsd ~ "Primary immune deficiencies", 
      high_risk_group_date == hiv_aids_nhsd ~ "HIV/AIDS", 
      high_risk_group_date == solid_organ_transplant_nhsd ~ "Solid organ transplant recipients",
      high_risk_group_date == rare_neurological_conditions_nhsd ~ "Rare neurological conditions",
      TRUE ~ NA_character_),
    
    downs_syndrome_nhsd = ifelse(!is.na(downs_syndrome_nhsd), "Downs syndrome", NA),
    sickle_cell_disease_nhsd = ifelse(!is.na(sickle_cell_disease_nhsd), "sickle cell disease", NA),
    cancer_opensafely_snomed = ifelse(!is.na(cancer_opensafely_snomed), "solid cancer", NA),
    haematological_disease_nhsd = ifelse(!is.na(haematological_disease_nhsd), "haematological diseases", NA),
    ckd_stage_5_nhsd = ifelse(!is.na(ckd_stage_5_nhsd), "renal disease", NA),
    liver_disease_nhsd = ifelse(!is.na(liver_disease_nhsd), "liver disease", NA),
    imid_nhsd = ifelse(!is.na(imid_nhsd), "IMID", NA),
    immunosupression_nhsd = ifelse(!is.na(immunosupression_nhsd), "primary immune deficiencies", NA),
    hiv_aids_nhsd = ifelse(!is.na(hiv_aids_nhsd), "HIV or AIDS", NA),
    solid_organ_transplant_nhsd = ifelse(!is.na(solid_organ_transplant_nhsd), "solid organ recipients", NA),
    rare_neurological_conditions_nhsd = ifelse(!is.na(rare_neurological_conditions_nhsd), "rare neurological conditions", NA)
    ) %>%
  unite("high_risk_group_nhsd_combined", downs_syndrome_nhsd, sickle_cell_disease_nhsd, cancer_opensafely_snomed,
      haematological_disease_nhsd, ckd_stage_5_nhsd, liver_disease_nhsd, imid_nhsd, immunosupression_nhsd, hiv_aids_nhsd, 
      solid_organ_transplant_nhsd, rare_neurological_conditions_nhsd, sep = ",", na.rm = T) %>%
  mutate(
    
    # CLINICAL/DEMOGRAPHIC COVARIATES ----
    sex = fct_case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      #sex == "I" ~ "Inter-sex",
      #sex == "U" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    ethnicity_filled = ifelse(is.na(ethnicity_primis), ethnicity_sus, ethnicity_primis),
    ethnicity = ifelse(is.na(ethnicity_filled), 6, ethnicity_filled),
    
    ethnicity = fct_case_when(
      ethnicity == "1" ~ "White",
      ethnicity == "2" ~ "Mixed",
      ethnicity == "3" ~ "Asian or Asian British",
      ethnicity == "4" ~ "Black or Black British",
      ethnicity == "5" ~ "Other ethnic groups",
      ethnicity == "6" ~ "Unknown",
      #TRUE ~ "Unknown"
      TRUE ~ NA_character_),
    
    imd = na_if(imd, "0"),
    imd = fct_case_when(
      imd == 1 ~ "1 most deprived",
      imd == 2 ~ "2",
      imd == 3 ~ "3",
      imd == 4 ~ "4",
      imd == 5 ~ "5 least deprived",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    region_nhs = fct_case_when(
      region_nhs == "London" ~ "London",
      region_nhs == "East" ~ "East of England",
      region_nhs == "East Midlands" ~ "East Midlands",
      region_nhs == "North East" ~ "North East",
      region_nhs == "North West" ~ "North West",
      region_nhs == "South East" ~ "South East",
      region_nhs == "South West" ~ "South West",
      region_nhs == "West Midlands" ~ "West Midlands",
      region_nhs == "Yorkshire and The Humber" ~ "Yorkshire and the Humber",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_),
    
  ) %>%
  filter(
    # Exclude patients not classed as eligible or receiving treatment
    !is.na(elig_start) | !is.na(treatment_date)
  ) %>%
  droplevels() %>%
  select(patient_id,
         death_date, dereg_date,
         covid_test_positive, covid_positive_previous_30_days, tb_postest_treat, elig_start, elig_end,
         sotrovimab_covid_therapeutics, molnupiravir_covid_therapeutics, casirivimab_covid_therapeutics, treatment_date, treatment_type,
         high_risk_cohort_covid_therapeutics, high_risk_group_nhsd, high_risk_group_nhsd_date = high_risk_group_date, high_risk_group_nhsd_combined,
         covid_hospital_admission_date, age, sex, ethnicity, imd, region_nhs, region_covid_therapeutics
  )


# Save dataset(s) ----
write_rds(data_processed, here::here("output", "data", "data_processed.rds"), compress = "gz")


