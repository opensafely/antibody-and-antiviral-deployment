################################################################################
#
# Description: This script reports number of hospitalisation after +ve test
#
# Input: /output/data/data_processed_clean.rds
#
# Output: /output/coverage/table_admitted_after_pos_test.csv
#
# Author(s): L Nab
# Date created: 08/09/2022
#
################################################################################


# Preliminaries ----

## Import libraries
library(readr)
library(dplyr)

## Import input
data_processed_clean <- 
  read_rds(here::here("output", "data", "data_processed_clean.rds")) %>%
  filter(elig_start >= as.Date("2021-12-11") & elig_start <= as.Date("2022-04-28")) %>%
  mutate(# if someone is admitted to hospital after their pos test,
         # they're not eligible for treatment anymore. 
         no_days_admitted_after_pos_test = 
           ifelse(hospital_admission_date_after_eligible > elig_start,
                  difftime(hospital_discharge_date_before_eligible, elig_start, 
                           units = "days") %>% as.numeric(),
                  NA_integer_))

## Make an overview 

## number of people admitted to hospital after post test on day 1, 2, 3, 4
## NB: number in (ELIGIBLE + NOT TREATED) population
overview_n_admitted_after_pos_test <-
  data_processed_clean %>%
  filter(eligibility_status == "Eligible",
         is.na(treatment_date),
         between(no_days_admitted_after_pos_test, 1, 4)) %>%
  group_by(no_days_admitted_after_pos_test) %>%
  summarise(n = n())

## Redact output
overview_n_admitted_after_pos_test <- 
  overview_n_admitted_after_pos_test %>%
  mutate(n = ifelse(n <= 5 & n > 0, "[REDACTED]", n %>% as.character()))

## Save output
fs::dir_create(here::here("output", "coverage"))
write_csv(
  overview_n_admitted_after_pos_test,
  here::here("output", "coverage", "table_admitted_after_pos_test.csv"))
