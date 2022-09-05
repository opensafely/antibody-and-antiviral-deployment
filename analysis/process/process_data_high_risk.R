################################################################################
#
# Description: Total number of people in high risk group at start campaign
#
# Input: /output/data/input.csv.gz
#
# Output: /output/data/data_processed_high_risk.rds
#         /output/data_properties/n_high_risk.csv
#
# Author(s): L. Nab
# Date: 05/09/2022
#
################################################################################

# Preliminaries ----

## Import libraries
library('tidyverse')
library('here')
library('readr')


# Data processing ---- 

## Read in data (don't rely on defaults)
data_extract <- read_csv(
  here::here("output", "data", "input_high_risk.csv.gz"),
  col_types = cols_only(                      

    patient_id = col_integer(),
    age = col_integer(),
    has_died = col_logical(),
    registered_eligible = col_logical(),

    # HIGH RISK GROUPS ----
    downs_syndrome_nhsd = col_logical(),
    cancer_opensafely_snomed = col_logical(),
    haematological_disease_nhsd = col_logical(), 
    ckd_stage_5_nhsd = col_logical(),
    liver_disease_nhsd = col_logical(),
    immunosuppresant_drugs_nhsd = col_logical(),
    oral_steroid_drugs_nhsd = col_logical(),
    oral_steroid_drug_nhsd_3m_count = col_integer(),
    oral_steroid_drug_nhsd_12m_count = col_integer(),
    imid_nhsd = col_logical(),
    immunosupression_nhsd = col_logical(),
    hiv_aids_nhsd = col_logical(),
    solid_organ_transplant_nhsd = col_logical(),
    multiple_sclerosis_nhsd = col_logical(),
    motor_neurone_disease_nhsd = col_logical(),
    myasthenia_gravis_nhsd = col_logical(),
    huntingtons_disease_nhsd = col_logical()
    
  ),
)

## Filter people in high risk group
data_processed <- 
  data_extract %>%
  mutate(
    ## IMID - only include patients on corticosteroids (where 2 prescriptions have been issued in 3 month, or 4 prescriptions in 12 months) 
    oral_steroid_drugs_nhsd_2 = ifelse(oral_steroid_drug_nhsd_3m_count >= 2 | 
                                         oral_steroid_drug_nhsd_12m_count >= 4, 
                                       TRUE,
                                       FALSE),
    imid_nhsd_2 = ifelse(oral_steroid_drugs_nhsd_2 | 
                           immunosuppresant_drugs_nhsd,
                         TRUE,
                         FALSE),
    
    # high risk group is defined in this R script and not in the
    # study definition directly because it relies on imid_nhsd_2 defined
    # above
    high_risk_group = ifelse(huntingtons_disease_nhsd |
                               myasthenia_gravis_nhsd |
                               motor_neurone_disease_nhsd |
                               multiple_sclerosis_nhsd |
                               solid_organ_transplant_nhsd |
                               hiv_aids_nhsd |
                               immunosupression_nhsd |
                               imid_nhsd_2 |
                               liver_disease_nhsd |
                               ckd_stage_5_nhsd |
                               haematological_disease_nhsd |
                               cancer_opensafely_snomed |
                               downs_syndrome_nhsd,
                             TRUE,
                             FALSE)
  ) %>%
  filter(high_risk_group)

## Calculate number in high risk group
n_high_risk <-
  data_processed %>%
  nrow()
# save n to tibble (saved in subsequent step)
output <- 
  tibble(
    n = n_high_risk
  )

# Save output ----
write_rds(data_processed, 
          here::here("output", "data", "data_processed_high_risk.rds"), 
          compress = "gz")
fs::dir_create(here::here("output", "data_properties"))
write_csv(
  output,
  here::here("output", "data_properties", "n_high_risk.csv")
)
