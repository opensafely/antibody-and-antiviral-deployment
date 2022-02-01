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
    "date": {"earliest": "2019-01-01", "latest": "today"},
    "rate": "uniform",
    "incidence": 0.2,
  },
  
  ## Define index date
  index_date = index_date,
  
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
    
    registered = patients.registered_as_of("index_date"),
    
  ),
  
  
  # NEUTRALISING MONOCLONAL ANTIBODIES OR ANTIVIRALS ----
  
  ## Administration of anti-infective agent
  anti_infective_agent = patients.with_these_clinical_events(
    anti_infective_agent_codes,
    between = [campaign_start, index_date],
    returning = "binary_flag",
    return_expectations = {
      "incidence": 0.5,
    },
  ),
  
  ### Sotrovimab
  sotrovimab = patients.with_these_medications(
    sotrovimab_codes,
    between = [campaign_start, index_date],
    returning = "binary_flag",
    return_expectations = {
      "incidence": 0.2,
    },
  ),
  
  sotrovimab_code = patients.with_these_medications(
    sotrovimab_codes,
    between = [campaign_start, index_date],
    returning = "code",
    return_expectations = {
      "category": {"ratios": {str(40219011000001108): 1.0}}
    },
  ),
  
  sotrovimab_date = patients.with_these_medications(
    sotrovimab_codes,
    between = [campaign_start, index_date],
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
  ),
  
  ### Ronapreve
  ronapreve = patients.with_these_medications(
    ronapreve_codes,
    between = [campaign_start, index_date],
    returning = "binary_flag",
    return_expectations = {
      "incidence": 0.2,
    },
  ),
  
  ronapreve_code = patients.with_these_medications(
    ronapreve_codes,
    between = [campaign_start, index_date],
    returning = "code",
    return_expectations = {
      "category": {"ratios": {str(40025711000001108): 0.8, str(39654011000001101): 0.2}}
    },
  ),
  
  ronapreve_date = patients.with_these_medications(
    ronapreve_codes,
    between = [campaign_start, index_date],
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
  ),
  
  ## Provision of antiviral therapy
  antiviral_therapy = patients.with_these_clinical_events(
    antiviral_therapy_codes,
    between = [campaign_start, index_date],
    returning = "binary_flag",
    return_expectations = {
      "incidence": 0.3,
    },
  ),
  
  ### Molnupiravir
  molnupiravir = patients.with_these_medications(
    molnupiravir_codes,
    between = [campaign_start, index_date],
    returning = "binary_flag",
    return_expectations = {
      "incidence": 0.2,
    },
  ),
  
  molnupiravir_code = patients.with_these_medications(
    molnupiravir_codes,
    between = [campaign_start, index_date],
    returning = "code",
    return_expectations = {
      "category": {"ratios": {str(40251211000001109): 1.0}}
    },
  ),
  
  molnupiravir_date = patients.with_these_medications(
    molnupiravir_codes,
    between = [campaign_start, index_date],
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
  ),
  
  ## Date of (first) nMABS or antiviral
  nMABS_antiviral_date = patients.with_these_medications(
    codelist = combine_codelists(sotrovimab_codes, ronapreve_codes, molnupiravir_codes),
    between = [campaign_start, index_date],
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
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
      "incidence": 0.4
    },
  ),
  
  covid_test_date = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "any",
    returning = "date",
    date_format = "YYYY-MM-DD",
    between = ["index_date", "index_date + 7 days"],
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.55
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
    returning = "date",
    date_format = "YYYY-MM-DD",
    between = ["covid_test_date - 31 days", "covid_test_date - 1 day"],
    find_last_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01
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
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
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
    on_or_before = "index_date - 7 days",
  ),
  
  downs_syndrome_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = downs_syndrome_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
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
    on_or_before = "index_date - 7 days",
  ),
  
  sickle_cell_disease_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = sickle_cell_disease_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
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
    on_or_before = "index_date - 7 days",
  ),
  
  ## Haematological diseases
  haematopoietic_stem_cell_transplant_nhsd_snomed = patients.with_these_clinical_events(
    haematopoietic_stem_cell_transplant_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date - 7 days",
  ),
  
  haematopoietic_stem_cell_transplant_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = haematopoietic_stem_cell_transplant_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  haematopoietic_stem_cell_transplant_nhsd_opcs4 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_procedures = haematopoietic_stem_cell_transplant_nhsd_opcs4_codes,
    on_or_before = "index_date - 7 days",
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
  #   on_or_before = "index_date - 7 days",
  # ),
  
  haematological_malignancies_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = haematological_malignancies_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
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
    on_or_before = "index_date - 7 days",
  ),
  
  ckd_stage_5_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = ckd_stage_5_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
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
    on_or_before = "index_date - 7 days",
  ),
  
  liver_disease_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = ckd_stage_5_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
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
    on_or_before = "index_date - 7 days",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Primary immune deficiencies
  immunosupression_nhsd = patients.with_these_clinical_events(
    immunosupression_nhsd_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "index_date - 7 days",
    date_format = "YYYY-MM-DD",
  ),
  
  ## HIV/AIDs
  hiv_aids_nhsd_snomed = patients.with_these_clinical_events(
    hiv_aids_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date - 7 days",
  ),
  
  hiv_aids_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = hiv_aids_nhsd_icd10_codes,
    find_first_match_in_period = True,
    on_or_before = "index_date - 7 days",
    date_format = "YYYY-MM-DD",
  ),
  
  hiv_aids_nhsd = patients.minimum_of("hiv_aids_nhsd_snomed", "hiv_aids_nhsd_icd10"),
  
  ## Solid organ transplant
  solid_organ_transplant_nhsd_snomed = patients.with_these_clinical_events(
    solid_organ_transplant_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date - 7 days",
  ),
  
  solid_organ_transplant_nhsd_opcs4 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_procedures = solid_organ_transplant_nhsd_opcs4_codes,
    on_or_before = "index_date - 7 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  
transplant_multiple_opcs4_thymus_OS=patients.satisfying(
    """
    (has_thymus_transplant = has_transplant_y_code)
    """,

  has_transplant_y_code = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_procedures = replacement_of_organ_transplant_nhsd_opcs4_codes,
    on_or_before = "index_date - 7 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  
  has_thymus_transplant = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_procedures = thymus_gland_transplant_nhsd_opcs4_codes,
    on_or_before = "index_date - 7 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
),

transplant_multiple_opcs4_conjunctiva_OS=patients.satisfying(
    """
    (has_conjunctiva_OS_transplant = has_conjunctiva_transplant_y_code)
    """,

  has_conjunctiva_transplant_y_code = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_procedures = conjunctiva_y_codes_transplant_nhsd_opcs4_codes,
    on_or_before = "index_date - 7 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  
  has_conjunctiva_OS_transplant = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_procedures = conjunctiva_transplant_nhsd_opcs4_codes,
    on_or_before = "index_date - 7 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
),

  solid_organ_transplant_nhsd = patients.minimum_of("solid_organ_transplant_nhsd_snomed", "solid_organ_transplant_nhsd_opcs4","transplant_multiple_opcs4_thymus_OS"), 
  
  ## Rare neurological conditions
  
  ### Multiple sclerosis
  multiple_sclerosis_nhsd_snomed = patients.with_these_clinical_events(
    multiple_sclerosis_nhsd_snomed_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    on_or_before = "index_date - 7 days",
  ),
  
  multiple_sclerosis_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = multiple_sclerosis_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
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
    on_or_before = "index_date - 7 days",
  ),
  
  motor_neurone_disease_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = motor_neurone_disease_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
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
    on_or_before = "index_date - 7 days",
  ),
  
  myasthenia_gravis_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = myasthenia_gravis_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
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
    on_or_before = "index_date - 7 days",
  ),
  
  huntingtons_disease_nhsd_icd10 = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = huntingtons_disease_nhsd_icd10_codes,
    on_or_before = "index_date - 7 days",
    find_first_match_in_period = True,
    date_format = "YYYY-MM-DD",
  ),
  
  huntingtons_disease_nhsd = patients.minimum_of("huntingtons_disease_nhsd_snomed", "huntingtons_disease_nhsd_icd10"),
  
  
 
  
  
  
  # OTHER VARIABLES ----
  
  
)


