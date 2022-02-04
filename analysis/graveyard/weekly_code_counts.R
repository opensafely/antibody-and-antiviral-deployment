################################################################################

# Description: This script imports data extracted by the cohort extractor and
#              calculates the counts and rates of neutralising monoclonal 
#              antibodies and antivirals for non-hospitalised patients with 
#              COVID-19
#
# Input: /output/data/input_*.csv.gz
#
# Output: /output/data/data_weekly_counts.csv
#
# Author(s): M Green
# Date last updated: 05/01/2022
#
################################################################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('gt')
library('gtsummary')
library('here')

## Custom functions
source(here("analysis", "lib", "custom_functions.R"))


# Process data ----
input.files = list.files(path = here::here("output", "data"), pattern = "input_2")

data_weekly_counts = lapply(input.files, FUN = calculate_weekly_counts) %>% 
  bind_rows()


# Redaction ----


# Save data ----

## Save as .csv
write.csv(data_weekly_counts, file = here::here("output", "data", "data_weekly_counts.csv"))

