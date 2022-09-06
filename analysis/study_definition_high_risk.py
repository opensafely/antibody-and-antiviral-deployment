################################################################################
#
# Description: This script extracts people in a high risk cohort regardless of
#              being tested positive or not
#
# Output: output/data/input_high_risk.csv.gz
#
# Author(s): L. Nab
# Date: 05/09/2022
#
################################################################################


# IMPORT STATEMENTS ----

# Import code building blocks from cohort extractor package
from cohortextractor import (
  StudyDefinition,
  patients,
  combine_codelists,
)

# Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

# DEFINE STUDY POPULATION ----

# Define study time variables
campaign_start = "2021-12-16"

# Define study population and variables
study = StudyDefinition(

  # PRELIMINARIES ----

  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "2021-11-01", "latest": "today"},
    "rate": "uniform",
    "incidence": 0.05,
  },

  # Define index date
  index_date=campaign_start,

  # POPULATION ----
  population=patients.satisfying(
    """
    age >= 12 AND age < 110
    AND NOT has_died
    AND registered_eligible
    """,
  ),

  # Age
  age=patients.age_as_of(
    "index_date",
    return_expectations={
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence": 0.9
    },
  ),

  has_died=patients.died_from_any_cause(
    on_or_before="index_date",
    returning="binary_flag",
  ),

  registered_eligible=patients.registered_as_of("index_date"),

  # Definition of high risk using regular codelists
  # Down's syndrome
  downs_syndrome_nhsd_snomed=patients.with_these_clinical_events(
    downs_syndrome_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.05
    },
  ),
  downs_syndrome_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses=downs_syndrome_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.05
    },
  ),
  downs_syndrome_nhsd=patients.satisfying(
    "downs_syndrome_nhsd_snomed OR downs_syndrome_nhsd_icd10",
    return_expectations={
      "incidence": 0.05,
    },
  ),
  # Solid cancer
  cancer_opensafely_snomed=patients.with_these_clinical_events(
    combine_codelists(
      non_haematological_cancer_opensafely_snomed_codes,
      lung_cancer_opensafely_snomed_codes,
      chemotherapy_radiotherapy_opensafely_snomed_codes
    ),
    between=["index_date - 6 months", "index_date"],
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  # Haematological diseases
  haematopoietic_stem_cell_transplant_nhsd_snomed=patients.with_these_clinical_events(
    haematopoietic_stem_cell_transplant_nhsd_snomed_codes,
    between=["index_date - 12 months", "index_date"],
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  haematopoietic_stem_cell_transplant_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    between=["index_date - 12 months", "index_date"],
    with_these_diagnoses=haematopoietic_stem_cell_transplant_nhsd_icd10_codes,
    find_last_match_in_period=True,
    return_expectations={
      "incidence": 0.4
    },
  ),
  haematopoietic_stem_cell_transplant_nhsd_opcs4=patients.admitted_to_hospital(
    returning="binary_flag",
    between=["index_date - 12 months", "index_date"],
    with_these_procedures=haematopoietic_stem_cell_transplant_nhsd_opcs4_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  haematological_malignancies_nhsd_snomed=patients.with_these_clinical_events(
    haematological_malignancies_nhsd_snomed_codes,
    between=["index_date - 24 months", "index_date"],
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  haematological_malignancies_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    between=["index_date - 24 months", "index_date"],
    with_these_diagnoses=haematological_malignancies_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  sickle_cell_disease_nhsd_snomed=patients.with_these_clinical_events(
    sickle_cell_disease_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  sickle_cell_disease_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses=sickle_cell_disease_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  haematological_disease_nhsd=patients.satisfying(
    """
    haematopoietic_stem_cell_transplant_nhsd_snomed OR
    haematopoietic_stem_cell_transplant_nhsd_icd10 OR
    haematopoietic_stem_cell_transplant_nhsd_opcs4 OR
    haematological_malignancies_nhsd_snomed OR
    haematological_malignancies_nhsd_icd10 OR
    sickle_cell_disease_nhsd_snomed OR
    sickle_cell_disease_nhsd_icd10
    """,
    return_expectations={
      "incidence": 0.05,
    },
  ),
  # Renal disease
  ckd_stage_5_nhsd_snomed=patients.with_these_clinical_events(
    ckd_stage_5_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  ckd_stage_5_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses=ckd_stage_5_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  ckd_stage_5_nhsd=patients.satisfying(
    "ckd_stage_5_nhsd_snomed OR ckd_stage_5_nhsd_icd10",
    return_expectations={
      "incidence": 0.05
    },
  ),
  # Liver disease
  liver_disease_nhsd_snomed=patients.with_these_clinical_events(
    liver_disease_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  liver_disease_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses=liver_disease_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  liver_disease_nhsd=patients.satisfying(
    "liver_disease_nhsd_snomed OR liver_disease_nhsd_icd10",
    return_expectations={
      "incidence": 0.05
    },
  ),
  # Immune-mediated inflammatory disorders (IMID)
  immunosuppresant_drugs_nhsd=patients.with_these_medications(
    codelist=combine_codelists(
      immunosuppresant_drugs_dmd_codes, 
      immunosuppresant_drugs_snomed_codes),
    returning="binary_flag",
    between=["index_date - 6 months", "index_date"],
    return_expectations={
      "incidence": 0.4
    },
  ),
  oral_steroid_drugs_nhsd=patients.with_these_medications(
    codelist=combine_codelists(
      oral_steroid_drugs_dmd_codes,
      oral_steroid_drugs_snomed_codes),
    returning="binary_flag",
    between=["index_date - 12 months", "index_date"],
    return_expectations={
      "incidence": 0.4
    },
  ),
  oral_steroid_drug_nhsd_3m_count=patients.with_these_medications(
    codelist=combine_codelists(
      oral_steroid_drugs_dmd_codes,
      oral_steroid_drugs_snomed_codes),
    returning="number_of_matches_in_period",
    between=["index_date - 3 months", "index_date"],
    return_expectations={"incidence": 0.1,
      "int": {"distribution": "normal", "mean": 2, "stddev": 1},
    },
  ),
  oral_steroid_drug_nhsd_12m_count=patients.with_these_medications(
    codelist=combine_codelists(
      oral_steroid_drugs_dmd_codes,
      oral_steroid_drugs_snomed_codes),
    returning="number_of_matches_in_period",
    between=["index_date - 12 months", "index_date"],
    return_expectations={"incidence": 0.1,
      "int": {"distribution": "normal", "mean": 3, "stddev": 1},
    },
  ),
  imid_nhsd=patients.satisfying(
    "immunosuppresant_drugs_nhsd OR oral_steroid_drugs_nhsd",
    return_expectations={
      "incidence": 0.05
    },
  ),
  # Primary immune deficiencies
  immunosupression_nhsd=patients.with_these_clinical_events(
    immunosupression_nhsd_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  # HIV/AIDs
  hiv_aids_nhsd_snomed=patients.with_these_clinical_events(
    hiv_aids_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  hiv_aids_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses=hiv_aids_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  hiv_aids_nhsd=patients.satisfying(
    "hiv_aids_nhsd_snomed OR hiv_aids_nhsd_icd10",
    return_expectations={
      "incidence": 0.05
    },
  ),
  # Solid organ transplant
  solid_organ_transplant_nhsd_snomed=patients.with_these_clinical_events(
    solid_organ_transplant_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  solid_organ_transplant_nhsd_opcs4=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_procedures=solid_organ_transplant_nhsd_opcs4_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  transplant_all_y_codes_opcs4=patients.admitted_to_hospital(
    returning="date_admitted",
    with_these_procedures=replacement_of_organ_transplant_nhsd_opcs4_codes,
    on_or_before="index_date",
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
    return_expectations={
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  transplant_thymus_opcs4=patients.admitted_to_hospital(
    returning="binary_flag",
    with_these_procedures=thymus_gland_transplant_nhsd_opcs4_codes,
    between=["transplant_all_y_codes_opcs4","transplant_all_y_codes_opcs4"],
    return_expectations={
      "incidence": 0.4
    },
  ),
  transplant_conjunctiva_y_code_opcs4=patients.admitted_to_hospital(
    returning="date_admitted",
    with_these_procedures=conjunctiva_y_codes_transplant_nhsd_opcs4_codes,
    on_or_before="index_date",
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
    return_expectations={
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  transplant_conjunctiva_opcs4=patients.admitted_to_hospital(
    returning="binary_flag",
    with_these_procedures=conjunctiva_transplant_nhsd_opcs4_codes,
    between=["transplant_conjunctiva_y_code_opcs4","transplant_conjunctiva_y_code_opcs4"],
    return_expectations={
      "incidence": 0.4
    },
  ),
  transplant_stomach_opcs4=patients.admitted_to_hospital(
    returning="binary_flag",
    with_these_procedures=stomach_transplant_nhsd_opcs4_codes,
    between=["transplant_all_y_codes_opcs4","transplant_all_y_codes_opcs4"],
    return_expectations={
      "incidence": 0.4
    },
  ),
  transplant_ileum_1_Y_codes_opcs4=patients.admitted_to_hospital(
    returning="date_admitted",
    with_these_procedures=ileum_1_y_codes_transplant_nhsd_opcs4_codes,
    on_or_before="index_date",
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
    return_expectations={
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  transplant_ileum_2_Y_codes_opcs4=patients.admitted_to_hospital(
    returning="date_admitted",
    with_these_procedures=ileum_1_y_codes_transplant_nhsd_opcs4_codes,
    on_or_before="index_date",
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
    return_expectations={
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  transplant_ileum_1_opcs4=patients.admitted_to_hospital(
    returning="binary_flag",
    with_these_procedures=ileum_1_transplant_nhsd_opcs4_codes,
    between=["transplant_ileum_1_Y_codes_opcs4","transplant_ileum_1_Y_codes_opcs4"],
    return_expectations={
      "incidence": 0.4
    },
  ),
  transplant_ileum_2_opcs4=patients.admitted_to_hospital(
    returning="binary_flag",
    with_these_procedures=ileum_2_transplant_nhsd_opcs4_codes,
    between=["transplant_ileum_2_Y_codes_opcs4","transplant_ileum_2_Y_codes_opcs4"],
    return_expectations={
      "incidence": 0.4
    },
  ),
  solid_organ_transplant_nhsd=patients.satisfying(
    """
    solid_organ_transplant_nhsd_snomed OR
    solid_organ_transplant_nhsd_opcs4 OR
    transplant_thymus_opcs4 OR
    transplant_conjunctiva_opcs4 OR
    transplant_stomach_opcs4 OR
    transplant_ileum_1_opcs4 OR
    transplant_ileum_2_opcs4
    """,
    return_expectations={
      "incidence": 0.05
    },
  ),
  # Rare neurological conditions
  # Multiple sclerosis
  multiple_sclerosis_nhsd_snomed=patients.with_these_clinical_events(
    multiple_sclerosis_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  multiple_sclerosis_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses=multiple_sclerosis_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  multiple_sclerosis_nhsd=patients.satisfying(
    "multiple_sclerosis_nhsd_snomed OR multiple_sclerosis_nhsd_icd10",
    return_expectations={
      "incidence": 0.05
    },
  ),
  # Motor neurone disease
  motor_neurone_disease_nhsd_snomed=patients.with_these_clinical_events(
    motor_neurone_disease_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  motor_neurone_disease_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses=motor_neurone_disease_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  motor_neurone_disease_nhsd=patients.satisfying(
    "motor_neurone_disease_nhsd_snomed OR motor_neurone_disease_nhsd_icd10",
    return_expectations={
      "incidence": 0.05
    },
  ),
  # Myasthenia gravis
  myasthenia_gravis_nhsd_snomed=patients.with_these_clinical_events(
    myasthenia_gravis_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  myasthenia_gravis_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses=myasthenia_gravis_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  myasthenia_gravis_nhsd=patients.satisfying(
    "myasthenia_gravis_nhsd_snomed OR myasthenia_gravis_nhsd_icd10",
    return_expectations={
      "incidence": 0.05
    },
  ),
  # Huntington’s disease
  huntingtons_disease_nhsd_snomed=patients.with_these_clinical_events(
    huntingtons_disease_nhsd_snomed_codes,
    on_or_before="index_date",
    returning="binary_flag",
    return_expectations={
      "incidence": 0.4
    },
  ),
  huntingtons_disease_nhsd_icd10=patients.admitted_to_hospital(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses=huntingtons_disease_nhsd_icd10_codes,
    return_expectations={
      "incidence": 0.4
    },
  ),
  huntingtons_disease_nhsd=patients.satisfying(
    "huntingtons_disease_nhsd_snomed OR huntingtons_disease_nhsd_icd10",
    return_expectations={
      "incidence": 0.05
    },
  ),
)
