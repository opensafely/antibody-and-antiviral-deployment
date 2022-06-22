################################################################################
#
# Description: This script provides the formal specification of the study data 
#              that will be extracted from the OpenSAFELY database.
#
# Output: output/data/input_sgtf.csv.gz
#
# Author(s): M Green
# Date last updated: 22/06/2022
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
from datetime import date

end_date = "2021-05-30"


## Define study population and variables
study = StudyDefinition(
  
  # PRELIMINARIES ----
  
  ## Configure the expectations framework
  default_expectations = {
    "date": {"earliest": "2021-11-01", "latest": "2021-05-30"},
    "rate": "uniform",
    "incidence": 0.05,
  },
  
  ## Define index date
  index_date = end_date,
  
  # POPULATION ----
  population = patients.satisfying(
    
    """
    registered
    """,
    
    registered = patients.registered_as_of(end_date),
    
  ),
  
  
  # POSITIVE SARS-CoV-2 TEST RESULTS ----
  
  ## First positive SARS-CoV-2 test
  covid_first_test_positive_date_alltests = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = "index_date",
    return_expectations = {
      "date": {"earliest": "2021-12-20"},
      "incidence": 0.9
    },
  ),
  
  covid_first_test_positive_date_earliestspecimen = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = True,
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = "index_date",
    return_expectations = {
      "date": {"earliest": "2021-12-20"},
      "incidence": 0.9
    },
  ),
  
  ## Most recent positive SARS-CoV-2 test
  covid_lastest_test_positive_date_alltests = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_last_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = "index_date",
    return_expectations = {
      "date": {"earliest": "2021-12-20"},
      "incidence": 0.9
    },
  ),
  
  covid_lastest_test_positive_date_earliestspecimen = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_last_match_in_period = True,
    restrict_to_earliest_specimen_date = True,
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = "index_date",
    return_expectations = {
      "date": {"earliest": "2021-12-20"},
      "incidence": 0.9
    },
  ),
  
  
  # S-GENE TARGET FAILURE Target
  
  ## SGTF for first covid test
  sgtf_first_alltests = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_first_match_in_period = True,
    between = ["covid_first_test_positive_date_alltests", "covid_first_test_positive_date_alltests"],
    returning = "s_gene_target_failure",
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"0": 0.7, "1": 0.1, "8": 0.05, "9": 0.05, "": 0.1}},
    },
  ), 
  
  sgtf_first_earliestspeciment = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_first_match_in_period = True,
    between = ["covid_first_test_positive_date_earliestspecimen", "covid_first_test_positive_date_earliestspecimen"],
    returning = "s_gene_target_failure",
    restrict_to_earliest_specimen_date = True,
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"0": 0.7, "1": 0.1, "9": 0.1, "": 0.1}},
    },
  ), 
  
  ## SGTF for latest covid test
  sgtf_lastest_alltests = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_last_match_in_period = True,
    between = ["covid_lastest_test_positive_date_alltests", "covid_lastest_test_positive_date_alltests"],
    returning = "s_gene_target_failure",
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"0": 0.7, "1": 0.1, "8": 0.05, "9": 0.05, "": 0.1}},
    },
  ), 
  
  sgtf_latest_earliestspeciment = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_first_match_in_period = True,
    between = ["covid_lastest_test_positive_date_earliestspecimen", "covid_lastest_test_positive_date_earliestspecimen"],
    returning = "s_gene_target_failure",
    restrict_to_earliest_specimen_date = True,
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"0": 0.7, "1": 0.1, "9": 0.1, "": 0.1}},
    },
  ), 
  
  
  
  # CASE CATEGORY (type of test used)
  covid_positive_test_type_earliestspecimen = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "case_category",
    between = ["covid_first_test_positive_date_earliestspecimen", "covid_first_test_positive_date_earliestspecimen"],
    restrict_to_earliest_specimen_date = True,
    return_expectations = {
      "category": {"ratios": {"LFT_Only": 0.4, "PCR_Only": 0.4, "LFT_WithPCR": 0.2}},
      "incidence": 0.2,
    },
  ),
  
  
  # VARIENT
  variant_first_alltests = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_first_match_in_period = True,
    between = ["covid_first_test_positive_date_alltests", "covid_first_test_positive_date_alltests"],
    restrict_to_earliest_specimen_date = False,
    returning = "variant",
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"B.1.617.2": 0.7, "B.1.1.7+E484K": 0.1, "No VOC detected": 0.1, "Undetermined": 0.1}},
    },
  ), 
  
  variant_latest_alltests = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    find_first_match_in_period = True,
    between = ["covid_lastest_test_positive_date_alltests", "covid_lastest_test_positive_date_alltests"],
    restrict_to_earliest_specimen_date = False,
    returning = "variant",
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"B.1.617.2": 0.7, "B.1.1.7+E484K": 0.1, "No VOC detected": 0.1, "Undetermined": 0.1}},
    },
  ), 
  
  
)
