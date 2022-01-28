################################################################################
#
# Description: This script imports the NHS digital codelists from:
#              https://digital.nhs.uk/coronavirus/treatments/methodology/demographics-and-test-result-rules
#              and converts them into seprate codelists based on the different
#              coding systems and uploads them to opencodelists
#
# Author(s): M Green
# Date last updated: 12/01/2022
#
################################################################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('here')
library('httr')
library('readxl')


# Custom functions ----

## Read in .xlxs file with multiple sheets
read_in_data <- function(fname) {
  
  # Get info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # Asign names to data frames
  names(data_frame) <- sheets
  
  print(data_frame)
  
}

## Seperate out different coding systems
seperate <- function(cohort) {
  
  # NHS Cohort codelists 
  codelist_data <- COVID_methodology_cohorts[cohort] %>%
    as.data.frame() %>%
    select(coding_system_id = contains("Dataset"), codes = contains("Code.value"))
  
  # Split out different coding systems
  unique_codelists <- split(codelist_data, f = codelist_data$coding_system_id)
  
  seperate_codelists <- names(unique_codelists) %>% 
    str_detect("ICD-10|SNOMED|DM|HES|ICD10") %>%
    keep(unique_codelists, .)
  
  
  if (length(seperate_codelists) > 0) {
    names(seperate_codelists) <- paste(cohort, " (", names(seperate_codelists), ")", sep = "")
  } else {
    seperate_codelists = list()
  }
  
  seperate_codelists
  
}

## Opencodelists API
openc_api <- function(codelist_name) {
  
  # Generic 
  api_token = "7a046994a6161e7a63bd407b9c6be62d1d32804c"

  # Codelist
  codelist_data <- cohort_codelists[codelist_name] %>%
    as.data.frame() %>%
    select(coding_system = contains("coding_system_id"), codes = contains("codes")) %>%
    left_join(data.frame(coding_system = c("ICD-10", "SNOMED", "DMD+d", "HES", "ICD10"), 
                         coding_system_id = c("icd10", "snomedct", "dmd", "icd10", "icd10")))
  
  # Data
  body = list(
    name =  codelist_name,
    coding_system_id = unique(codelist_data$coding_system_id),
    codes = codelist_data$codes,
    description = paste("Taken from the ",
                        sub("\\(.*", "", codelist_name),
                        "tab in the Population Risk COVID-19 Treatments code list v1.3, published by NHS digital, 13/12/2021.")
    # references = list(
    #   url = c("https://digital.nhs.uk/coronavirus/treatments/methodology/demographics-and-test-result-rules"),
    #   text = c("This document lists all the codes used for the COVID-19 therapeutics detailing the datasets, coding systems, 
    #            code values and code descriptions."))
  )
  
  # Organisation
  organisation = "nhsd"
  
  # URL and headers
  url = paste("https://www.opencodelists.org/api/v1/codelist/", organisation, "/", sep = "")

  rsp = POST(url, body = body, encode = "json", verbose(), add_headers(authorization = paste("Token ", api_token, sep = "")))

  # Check status
  if (status_code(rsp) == 200){
    print("sucess")
  } else {
    print("Failure, talk to Peter")
  }

}
  

# Process NHS codelists ----

## Read in data
path <- here::here("docs", "COVID+methodology+cohorts++.xlsx")
COVID_methodology_cohorts <- read_in_data(path)

## Same column names and types
COVID_methodology_cohorts$`Downs Syndrome` <- COVID_methodology_cohorts$`Downs Syndrome` %>%
  dplyr::rename(Dataset = 'Coding System')

COVID_methodology_cohorts$`Transplant (SPL-AtRiskv4)` <- COVID_methodology_cohorts$`Transplant (SPL-AtRiskv4)` %>%
  dplyr::rename(Dataset = Dateset, 'Code values' = 'Code Values')

COVID_methodology_cohorts$`Sickle (SPL-HES)` <- COVID_methodology_cohorts$`Sickle (SPL-HES)` %>%
  dplyr::rename(`Code value` = 'Code') %>%
  mutate(Dataset = CodeType)

COVID_methodology_cohorts$`Huntington's` <- COVID_methodology_cohorts$`Huntington's` %>%
  mutate(Dataset = ifelse(Dataset == "HES", "ICD10", Dataset))

COVID_methodology_cohorts$`Myasthenia Gravis` <- COVID_methodology_cohorts$`Myasthenia Gravis` %>%
  mutate(Dataset = ifelse(Dataset == "HES", "ICD10", Dataset))

## Separate out different coding systems
cohorts <- names(COVID_methodology_cohorts)[-1]
cohort_codelists <- list()

for (i in 1:length(cohorts)){
  cohort_codelists <- append(cohort_codelists, seperate(cohorts[i]))
  
  print(i)
}


## Save codelists as individual .csvs ----
dir.create(here::here("make_codelists", "NHS Digital codelists"), showWarnings = FALSE, recursive=TRUE)
codelist_names <- names(cohort_codelists)

for (i in 1:length(codelist_names)){
  
  ind_codelists <- cohort_codelists[i] %>%
    as.data.frame() %>%
    select(codes = contains("codes"))
  
  names(ind_codelists) <- NULL
  
  write.csv(ind_codelists, 
            file = (here::here("make_codelists", "NHS Digital codelists", paste(codelist_names[i], ".csv", sep = ""))), 
            row.names = FALSE)
  
  print(i)
  
}


## Upload to opencodelists ----
codelist_names <- names(cohort_codelists)

openc_api(codelist_names[1])
lapply(codelist_names, FUN = openc_api())


