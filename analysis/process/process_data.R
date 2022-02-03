################################################################################
#
# Description: This script imports data extracted by the cohort extractor and
#              calculates additonal variables needed for subsequent anlyses 
#              (i.e., elibility criteria window)
#
# Input: /output/data/input_*.csv.gz
#
# Output: /output/data/data_processed.csv
#
# Author(s): M Green
# Date last updated: 02/02/2022
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

## Read in and process each input file
input.files = list.files(path = here::here("output", "data"), pattern = "input_not_weekly")

data_processed_full = lapply(input.files, FUN = process_data) %>%
   bind_rows()


# Save dataset(s) ----
write_rds(data_processed_full, here::here("output", "data", "data_processed.rds"), compress = "gz")


