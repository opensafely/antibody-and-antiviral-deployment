################################################################################
#
# Description: This script imports data extracted by the cohort extractor and
#              calculates additional variables needed for subsequent anlyses 
#              (i.e., eligibility criteria window)
#
# Input: /output/data/input.csv.gz
#
# Output: /output/data/data_processed.csv
#         /output/data/data_processed_clean.csv
#
# Author(s): M Green
# Date last updated: 09/02/2022
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
cat("#### process data ####\n")

## Read in data (don't rely on defaults)
data_extract0 <- read_csv(
  here::here("output", "data", "input.csv.gz"),
  col_types = cols_only(                      
    
    # PATIENT ID ----
    patient_id = col_integer(),
    
    # CENSORING ----
    death_date = col_date(format = "%Y-%m-%d"),
    has_died = col_logical(),
    dereg_date = col_date(format = "%Y-%m-%d"),
    registered_eligible = col_logical(),
    registered_treated = col_logical(),
    
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
    covid_hospital_discharge_date = col_date(format = "%Y-%m-%d"),
    age = col_integer(),
    
    # SOLID ORGAN TRANSPLANT - FOR INVESTIGATION ----
    solid_organ_transplant_nhsd_snomed = col_date(format = "%Y-%m-%d"),
    solid_organ_transplant_nhsd_opcs4 = col_date(format = "%Y-%m-%d"),
    transplant_all_y_codes_opcs4 = col_date(format = "%Y-%m-%d"),
    transplant_thymus_opcs4 = col_date(format = "%Y-%m-%d"),
    transplant_conjunctiva_y_code_opcs4 = col_date(format = "%Y-%m-%d"),
    transplant_conjunctiva_opcs4 = col_date(format = "%Y-%m-%d"),
    transplant_stomach_opcs4 = col_date(format = "%Y-%m-%d"),
    transplant_ileum_1_Y_codes_opcs4  = col_date(format = "%Y-%m-%d"),
    transplant_ileum_2_Y_codes_opcs4  = col_date(format = "%Y-%m-%d"),
    transplant_ileum_1_opcs4 = col_date(format = "%Y-%m-%d"),
    transplant_ileum_2_opcs4 = col_date(format = "%Y-%m-%d"),
    
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
    region_covid_therapeutics = col_character(),
    
    # OUTCOMES ----
    covid_positive_test_30_days_post_elig_or_treat = col_date(format = "%Y-%m-%d"),
    covid_hospitalisation_outcome_date = col_date(format = "%Y-%m-%d"),
    covid_hospitalisation_critical_care = col_integer(),
    death_with_covid_on_the_death_certificate_date = col_date(format = "%Y-%m-%d"),
    death_with_28_days_of_covid_positive_test = col_logical(),
    
    # OTHER COVARIATES ----
    vaccination_status = col_character()
  ),
)

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
    haematological_disease_nhsd = ifelse(!is.na(haematological_disease_nhsd), "haematological diseases and stem cell transplant recipients", NA),
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
    
    # OUTCOMES ----
    covid_positive_test_30_days_post_elig_or_treat_date = covid_positive_test_30_days_post_elig_or_treat, 
    covid_positive_test_30_days_post_elig_or_treat = ifelse(!is.na(covid_positive_test_30_days_post_elig_or_treat_date), 1, 0),

    start_date = pmin(covid_test_positive_date, treatment_date, na.rm = T),
    covid_hospital_admission = ifelse(covid_hospitalisation_outcome_date > start_date, 1, 0),
    covid_hospitalisation_critical_care = ifelse(covid_hospitalisation_critical_care > 0 & covid_hospital_admission == 1, 1, 0),
    
    covid_death = ifelse(!is.na(death_with_covid_on_the_death_certificate_date) |
                           death_with_28_days_of_covid_positive_test == 1, 1, 0)
    
  ) %>%
  droplevels() %>%
  select(patient_id,
         has_died, death_date, dereg_date, registered_eligible, registered_treated,
         covid_test_positive, covid_positive_previous_30_days, tb_postest_treat, elig_start, elig_end,
         sotrovimab_covid_therapeutics, molnupiravir_covid_therapeutics, casirivimab_covid_therapeutics, treatment_date, treatment_type,
         high_risk_cohort_covid_therapeutics, high_risk_group_nhsd, high_risk_group_nhsd_date = high_risk_group_date, high_risk_group_nhsd_combined,
         covid_hospital_discharge_date, age, sex, ethnicity, imd, region_nhs, region_covid_therapeutics,
         covid_positive_test_30_days_post_elig_or_treat, covid_hospital_admission, covid_hospitalisation_critical_care, covid_death,
         solid_organ_transplant_nhsd_snomed, solid_organ_transplant_nhsd_opcs4, transplant_all_y_codes_opcs4, transplant_thymus_opcs4, 
         transplant_conjunctiva_y_code_opcs4, transplant_conjunctiva_opcs4, transplant_stomach_opcs4, transplant_ileum_1_Y_codes_opcs4,
         transplant_ileum_2_Y_codes_opcs4, transplant_ileum_1_opcs4, transplant_ileum_2_opcs4
         )


# Save dataset(s) ----
write_rds(data_processed, here::here("output", "data", "data_processed.rds"), compress = "gz")


# Process clean data ----
cat("#### process clean data ####\n")

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
    is.na(covid_hospital_discharge_date) | (covid_hospital_discharge_date < (elig_start - 30) & covid_hospital_discharge_date > (elig_start)),
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

## Remove and save
rm(data_processed_hrc_matched)

data_processed_combined <- rbind(data_processed_eligible, data_processed_treated)

rm(data_processed_eligible)
rm(data_processed_treated)

print(dim(data_processed_combined))
print(table(data_processed_combined$eligibility_status))

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

rm(data_processed_combined)

# Save dataset(s) ----
write_rds(data_processed_clean, here::here("output", "data", "data_processed_clean.rds"), compress = "gz")





