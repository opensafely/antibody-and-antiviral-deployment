################################################################################
#
# Description: This script provides the formal specification of the study data 
#              that will be extracted from the OpenSAFELY database.
#
# Output: output/data/input_*.csv.gz
#
# Author(s): M Green
# Date last updated: 31/01/2022
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

campaign_start = "2021-12-16"
index_date = date.today().isoformat()


## Define study population and variables
study = StudyDefinition(
  
  # PRELIMINARIES ----
  
  ## Configure the expectations framework
  default_expectations = {
    "date": {"earliest": "2021-11-01", "latest": "today"},
    "rate": "uniform",
    "incidence": 0.4,
  },
  
  ## Define index date
  index_date = index_date,
  
  # POPULATION ----
  population = patients.satisfying(
    """
    NOT has_died
    AND
    registered
    AND
    (sotrovimab_covid_therapeutics OR molnupiravir_covid_therapeutics OR covid_test_positive)
    """,
    
    has_died = patients.died_from_any_cause(
      on_or_before = "index_date",
      returning = "binary_flag",
    ),
    
    registered = patients.registered_as_of("index_date"),
    
  ),
  
  
  # CENSORING ----
  
  ## Death of any cause
  death_date = patients.died_from_any_cause(
    returning = "date_of_death",
    date_format = "YYYY-MM-DD",
    between = ["index_date", "index_date + 7 days"],
    return_expectations = {
      "date": {"earliest": "2021-12-20", "latest": "index_date"},
      "incidence": 0.1
      },
  ),
  
  ## De-registration
  dereg_date = patients.date_deregistered_from_all_supported_practices(
    between = ["index_date", "index_date + 7 days"],
    date_format = "YYYY-MM-DD",
    return_expectations = {
      "date": {"earliest": "2021-12-20", "latest": "index_date"},
      "incidence": 0.1
      },
  ),
  
  
  # NEUTRALISING MONOCLONAL ANTIBODIES OR ANTIVIRALS ----

  ## Sotrovimab
  sotrovimab_covid_therapeutics = patients.with_covid_therapeutics(
    with_these_statuses = ["Approved", "Treatment Complete"],
    with_these_therapeutics = "Sotrovimab",
    with_these_indications = "non_hospitalised",
    between = ["index_date", "index_date + 7 days"],
    find_first_match_in_period = True,
    returning = "date",
    date_format = "YYYY-MM-DD",
    return_expectations = {
      "date": {"earliest": "2021-12-20"},
      "incidence": 0.4
      },
  ),
  
  ### Molnupiravir
  molnupiravir_covid_therapeutics = patients.with_covid_therapeutics(
    with_these_statuses = ["Approved", "Treatment Complete"],
    with_these_therapeutics = "Molnupiravir",
    with_these_indications = "non_hospitalised",
    between = ["index_date", "index_date + 7 days"],
    find_first_match_in_period = True,
    returning = "date",
    date_format = "YYYY-MM-DD",
    return_expectations = {
      "date": {"earliest": "2021-12-20"},
      "incidence": 0.4
      },
  ),
  
  
  
  # ELIGIBILITY CRITERIA VARIABLES ----
  
  ## Inclusion criteria variables
  
  ### SARS-CoV-2 test
  covid_test_positive = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "binary_flag",
    between = ["index_date", "index_date + 7 days"],
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "incidence": 0.2
    },
  ),
  
  covid_test_date = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "any",
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    returning = "date",
    date_format = "YYYY-MM-DD",
    between = ["index_date", "index_date + 7 days"],
    return_expectations = {
      "date": {"earliest": "2021-12-20", "latest": "index_date"},
      "incidence": 0.9
      },
  ),
  

  covid_positive_test_type = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "case_category",
    between = ["index_date", "index_date + 7 days"],
    restrict_to_earliest_specimen_date = True,
    return_expectations = {
      "category": {"ratios": {"LFT_Only": 0.4, "PCR_Only": 0.4, "LFT_WithPCR": 0.2}},
      "incidence": 0.2,
    },
  ),
  
  covid_positive_previous_30_days = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "binary_flag",
    between = ["covid_test_date - 31 days", "covid_test_date - 1 day"],
    find_last_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "incidence": 0.05
    },
  ),
  
  ### Onset of symptoms of COVID-19
  symptomatic_covid_test = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "any",
    returning = "symptomatic",
    between = ["index_date", "index_date + 7 days"],
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations={
      "incidence": 0.1,
      "category": {
        "ratios": {
          "": 0.2,
          "N": 0.2,
          "Y": 0.6,
        }
      },
    },
  ),
  
  covid_symptoms_snomed = patients.with_these_clinical_events(
    covid_symptoms_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    between = ["index_date", "index_date + 7 days"],
  ),
  
  ### Blueteq ‘high risk’ cohort
  high_risk_cohort_covid_therapeutics = patients.with_covid_therapeutics(
    with_these_statuses = ["Approved", "Treatment Complete"],
    with_these_therapeutics = ["Sotrovimab", "Molnupiravir"],
    with_these_indications = "non_hospitalised",
    between = ["index_date", "index_date + 7 days"],
    find_first_match_in_period = True,
    returning = "risk_group",
    date_format = "YYYY-MM-DD",
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "Down's syndrome": 0.1,
          "Sickle cell disease": 0.1,
          "Patients with a solid cancer": 0.1,
          "Patients with a haematological diseases and stem cell transplant recipients": 0.1,
          "Patients with renal disease": 0.1,
          "Patients with liver disease": 0.1,
          "Patients with immune-mediated inflammatory disorders (IMID)": 0.2,
          "Primary immune deficiencies": 0.1,
          "HIV/AIDS": 0.1,},},
    },
  ),
  
  ### NHSD ‘high risk’ cohort (codelist to be defined if/when data avaliable)
  # high_risk_cohort_nhsd = patients.with_these_clinical_events(
  #   high_risk_cohort_nhsd_codes,
  #   between = [campaign_start, index_date],
  #   returning = "date",
  #   date_format = "YYYY-MM-DD",
  #   find_first_match_in_period = True,
  # ),
  
  ## Exclusion criteria
  
  ### Pattern of clinical presentation indicates that there is recovery rather than risk of deterioration from infection
  #   (not currently possible to define/code)
  
  ### Require hospitalisation for COVID-19
  covid_hospital_addmission_date = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = covid_icd10_codes,
    on_or_after = "index_date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "index_date", "latest": "index_date + 7 days"},
      "rate": "uniform",
      "incidence": 0.05
    },
  ),
  
  ### New supplemental oxygen requirement specifically for the management of COVID-19 symptoms
  #   (not currently possible to define/code)
  
  ### Children weighing less than 40kg
  #   (not currently possible to define/code)
  
  ### Children aged under 12 years
  age = patients.age_as_of(
    "index_date",
    return_expectations = {
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence" : 0.9
    },
  ),
  
  ### Known hypersensitivity reaction to the active substances or to any of the excipients of sotrovimab
  #   (not currently possible to define/code)
  
  
  
  # HIGH RISK GROUPS ----
  
  ## Down's syndrome
  downs_syndrome_nhsd_snomed = patients.with_these_clinical_events(
    downs_syndrome_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  downs_syndrome_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = downs_syndrome_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  downs_syndrome_nhsd = patients.minimum_of("downs_syndrome_nhsd_snomed", "downs_syndrome_nhsd_icd10"), 
  
  ## Sickle cell disease
  sickle_cell_disease_nhsd_snomed = patients.with_these_clinical_events(
    sickle_cell_disease_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  sickle_cell_disease_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = sickle_cell_disease_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  sickle_cell_disease_nhsd = patients.minimum_of("sickle_cell_disease_nhsd_snomed", "sickle_cell_disease_nhsd_icd10"), 
  
  ## Solid cancer
  cancer_opensafely_snomed = patients.with_these_clinical_events(
    combine_codelists(
      non_haematological_cancer_opensafely_snomed_codes,
      lung_cancer_opensafely_snomed_codes,
      chemotherapy_radiotherapy_opensafely_snomed_codes
    ),
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  ## Haematological diseases
  haematopoietic_stem_cell_transplant_nhsd_snomed = patients.with_these_clinical_events(
    haematopoietic_stem_cell_transplant_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  haematopoietic_stem_cell_transplant_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = haematopoietic_stem_cell_transplant_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  haematopoietic_stem_cell_transplant_nhsd_opcs4 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_procedures = haematopoietic_stem_cell_transplant_nhsd_opcs4_codes,
    on_or_before = "index_date + 7 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  
  # haematological_malignancies_nhsd_snomed = patients.with_these_clinical_events(
  #   haematological_malignancies_nhsd_snomed_codes,
  #   returning = "date",
  #   date_format = "YYYY-MM-DD",
  #   find_first_match_in_period = True,
  #   on_or_before = "index_date + 7 days",
  # ),
  
  haematological_malignancies_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = haematological_malignancies_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  haematological_disease_nhsd = patients.minimum_of("haematopoietic_stem_cell_transplant_nhsd_snomed", 
                                                    "haematopoietic_stem_cell_transplant_nhsd_icd10", 
                                                    "haematopoietic_stem_cell_transplant_nhsd_opcs4", 
                                                    #"haematological_malignancies_nhsd_snomed", 
                                                    "haematological_malignancies_nhsd_icd10"), 
  
  
  ## Renal disease
  ckd_stage_5_nhsd_snomed = patients.with_these_clinical_events(
    ckd_stage_5_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  ckd_stage_5_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = ckd_stage_5_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  ckd_stage_5_nhsd = patients.minimum_of("ckd_stage_5_nhsd_snomed", "ckd_stage_5_nhsd_icd10"), 
  
  ## Liver disease
  liver_disease_nhsd_snomed = patients.with_these_clinical_events(
    ckd_stage_5_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  liver_disease_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = ckd_stage_5_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  liver_disease_nhsd = patients.minimum_of("liver_disease_nhsd_snomed", "liver_disease_nhsd_icd10"), 
  
  ## Immune-mediated inflammatory disorders (IMID)
  imid_nhsd = patients.with_these_clinical_events(
    codelist = combine_codelists(immunosuppresant_drugs_dmd_codes, immunosuppresant_drugs_snomed_codes, 
                                 oral_steroid_drugs_dmd_codes, 
                                 oral_steroid_drugs_snomed_codes),
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "index_date + 7 days",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Primary immune deficiencies
  immunosupression_nhsd = patients.with_these_clinical_events(
    immunosupression_nhsd_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "index_date + 7 days",
    date_format = "YYYY-MM-DD",
  ),
  
  ## HIV/AIDs
  hiv_aids_nhsd_snomed = patients.with_these_clinical_events(
    hiv_aids_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  hiv_aids_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = hiv_aids_nhsd_icd10_codes,
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
    date_format = "YYYY-MM-DD",
  ),
  
  hiv_aids_nhsd = patients.minimum_of("hiv_aids_nhsd_snomed", "hiv_aids_nhsd_icd10"),
  
  ## Solid organ transplant
  solid_organ_transplant_nhsd_snomed = patients.with_these_clinical_events(
    solid_organ_transplant_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  solid_organ_transplant_nhsd_opcs4 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_procedures = solid_organ_transplant_nhsd_opcs4_codes,
    on_or_before = "index_date + 7 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  
  solid_organ_transplant_nhsd = patients.minimum_of("solid_organ_transplant_nhsd_snomed", "solid_organ_transplant_nhsd_opcs4"), 
  
  ## Rare neurological conditions
  
  ### Multiple sclerosis
  multiple_sclerosis_nhsd_snomed = patients.with_these_clinical_events(
    multiple_sclerosis_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  multiple_sclerosis_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = multiple_sclerosis_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  multiple_sclerosis_nhsd = patients.minimum_of("multiple_sclerosis_nhsd_snomed", "multiple_sclerosis_nhsd_icd10"), 
  
  ### Motor neurone disease
  motor_neurone_disease_nhsd_snomed = patients.with_these_clinical_events(
    motor_neurone_disease_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  motor_neurone_disease_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = motor_neurone_disease_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  motor_neurone_disease_nhsd = patients.minimum_of("motor_neurone_disease_nhsd_snomed", "motor_neurone_disease_nhsd_icd10"),
  
  ### Myasthenia gravis
  myasthenia_gravis_nhsd_snomed = patients.with_these_clinical_events(
    myasthenia_gravis_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  myasthenia_gravis_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = myasthenia_gravis_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  myasthenia_gravis_nhsd = patients.minimum_of("myasthenia_gravis_nhsd_snomed", "myasthenia_gravis_nhsd_icd10"),
  
  ### Huntington’s disease
  huntingtons_disease_nhsd_snomed = patients.with_these_clinical_events(
    huntingtons_disease_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date + 7 days",
  ),
  
  huntingtons_disease_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = huntingtons_disease_nhsd_icd10_codes,
    on_or_before = "index_date + 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  huntingtons_disease_nhsd = patients.minimum_of("huntingtons_disease_nhsd_snomed", "huntingtons_disease_nhsd_icd10"),
  
  
  
  
  
  # OTHER VARIABLES ----
  
  
)


