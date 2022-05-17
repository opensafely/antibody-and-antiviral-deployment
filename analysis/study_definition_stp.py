################################################################################
#
# Description: This script provides the formal specification of the data 
#              that will be extracted from the OpenSAFELY database.
#
# Output: output/data/input_stp.csv.gz
#
# Author(s): M Green
# Date last updated: 17/05/2022
#
################################################################################


# IMPORT STATEMENTS ----

## Import code building blocks from cohort extractor package
from cohortextractor import (
  StudyDefinition,
  patients,
  filter_codes_by_category,
  Measure
)


# DEFINE STUDY POPULATION ----

## Define study population and variables
study = StudyDefinition(
  
  # PRELIMINARIES ----
  
  ## Configure the expectations framework
  default_expectations = {
    "date": {"earliest": "2021-11-01", "latest": "today"},
    "rate": "uniform",
    "incidence": 0.05,
  },
  
  ## Define index date
  index_date = "2020-03-01",
  
  # POPULATION ----
  population = patients.satisfying(
    """
     registered_treated 
    """,
    registered_treated = patients.registered_as_of("index_date"),
  ),
  
  # STP (NHS administration region based on geography, currenty closest match to CMDU)
  stp = patients.registered_practice_as_of(
    "index_date",
    returning = "stp_code",
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "STP1": 0.1,
          "STP2": 0.1,
          "STP3": 0.1,
          "STP4": 0.1,
          "STP5": 0.1,
          "STP6": 0.1,
          "STP7": 0.1,
          "STP8": 0.1,
          "STP9": 0.1,
          "STP10": 0.1,
        }
      },
    },
  ),
  
  
)
