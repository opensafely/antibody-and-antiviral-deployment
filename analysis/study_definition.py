################################################################################
#
# Description: This script provides the formal specification of the study data 
#              that will be extracted from the OpenSAFELY database.
#
# output: output/data/input_*.csv.gz
#
################################################################################


# IMPORT STATEMENTS ----

## Import code building blocks from cohort extractor package
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
  Measure
)

## Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *
  
  
# DEFINE STUDY POPULATION ----

## Define study time variables
from datetime import datetime

start_date = "2021-01-01"
end_date = "2021-01-01"

## Define study population and variables
study = StudyDefinition(
  
  # PRELIMINARIES ----
  
  ## Configure the expectations framework
  default_expectations={
    "date": {"earliest": "1970-01-01", "latest": end_date},
    "rate": "uniform",
    "incidence": 0.2,
  },
  
  ## Set index date
  index_date = "2021-01-01",
  
  
  # POPULATION ----
  population = patients.satisfying(
    """
    NOT has_died
    AND
    registered
    """,
    
    has_died = patients.died_from_any_cause(
      on_or_before = "index_date",
      returning = "binary_flag",
    ),
    
    registered = patients.satisfying(
      "registered_at_start",
      registered_at_start = patients.registered_as_of("index_date"),
    ),
    
  ),

  
  # NEUTRALISING MONOCLONAL ANTIBODIES OR ANTIVIRALS ----
  
  ## Administration of anti-infective agent
  anti_infective_agent = patients.with_these_clinical_events(
    anti_infective_agent_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "number_of_matches_in_period",
    return_expectations = {
      "int": {"distribution": "normal", "mean": 3, "stddev": 1},
      "incidence": 0.5,
    },
  ),
  
  ### Sotrovimab
  sotrovimab = patients.with_these_medications(
    sotrovimab_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "number_of_matches_in_period",
    return_expectations = {
      "int": {"distribution": "normal", "mean": 3, "stddev": 1},
      "incidence": 0.2,
    },
  ),
  
  sotrovimab_code = patients.with_these_medications(
    sotrovimab_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "code",
    return_expectations = {
      "category": {"ratios": {str(40219011000001108): 1.0}}
    },
  ),
  
  sotrovimab_date = patients.with_these_medications(
    sotrovimab_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
  ),
  
  ### Ronapreve
  ronapreve = patients.with_these_medications(
    ronapreve_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "number_of_matches_in_period",
    return_expectations = {
      "int": {"distribution": "normal", "mean": 3, "stddev": 1},
      "incidence": 0.2,
    },
  ),
  
  ronapreve_code = patients.with_these_medications(
    ronapreve_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "code",
    return_expectations = {
      "category": {"ratios": {str(40025711000001108): 0.8, str(39654011000001101): 0.2}}
    },
  ),
  
  ronapreve_date = patients.with_these_medications(
    ronapreve_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
  ),
  
  ## Provision of antiviral therapy
  antiviral_therapy = patients.with_these_clinical_events(
    antiviral_therapy_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "number_of_matches_in_period",
    return_expectations = {
      "int": {"distribution": "normal", "mean": 2, "stddev": 1},
      "incidence": 0.3,
    },
  ),
  
  ### Molnupiravir
  molnupiravir = patients.with_these_medications(
    molnupiravir_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "number_of_matches_in_period",
    return_expectations = {
      "int": {"distribution": "normal", "mean": 3, "stddev": 1},
      "incidence": 0.2,
    },
  ),
  
  molnupiravir_code = patients.with_these_medications(
    molnupiravir_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "code",
    return_expectations = {
      "category": {"ratios": {str(40251211000001109): 1.0}}
    },
  ),
  
  molnupiravir_date = patients.with_these_medications(
    molnupiravir_codes,
    between = ["index_date", "index_date + 6 days"],
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
  ),
  
)

