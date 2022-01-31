######################################

# Some covariates used in the study are created from codelists of clinical conditions or 
# numerical values available on a patient's records.
# This script fetches all of the codelists identified in codelists.txt from OpenCodelists.

######################################


# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from cohortextractor import (codelist, codelist_from_csv, combine_codelists)


# --- CODELISTS ---


## NEUTRALISING MONOCLONAL ANTIBODIES OR ANTIVIRALS ----

### Administration of anti-infective agent
anti_infective_agent_codes = codelist(["47943005"], system = "snomed")

#### Sotrovimab
sotrovimab_codes = codelist(["40219011000001108"], system = "snomed")

#### Ronapreve
ronapreve_codes = codelist(["40025711000001108", "39654011000001101"], system = "snomed")

### Provision of antiviral therapy
antiviral_therapy_codes = codelist(["427314002"], system = "snomed")

### Molnupiravir
molnupiravir_codes = codelist(["40251211000001109"], system = "snomed")


## ELIGIBILITY CRITERIA VARIABLES ----

### Onset of symptoms of COVID-19
covid_symptoms_snomed_codes = codelist_from_csv(
  "codelists/user-MillieGreen-covid-19-symptoms.csv",
  system = "snomed",
  column = "code",
)

### Require hospitalisation for COVID-19
covid_icd10_codes = codelist_from_csv(
  "codelists/opensafely-covid-identification.csv",
  system = "icd10",
  column = "icd10_code",
)


## HIGH RISK GROUPS ----

### Down's syndrome
downs_syndrome_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-downs-syndrome-snomed-ct.csv",
  system = "snomed",
  column = "code",
)

downs_syndrome_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-downs-syndrome-icd-10.csv",
  system = "icd10",
  column = "code",
)

### Sickle cell disease
sickle_cell_disease_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-sickle-spl-atriskv4-snomed-ct.csv",
  system = "snomed",
  column = "code",
)

sickle_cell_disease_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-sickle-spl-hes-icd-10.csv",
  system = "icd10",
  column = "code",
)

### Solid cancer
non_haematological_cancer_opensafely_snomed_codes = codelist_from_csv(
  "codelists/opensafely-cancer-excluding-lung-and-haematological-snomed.csv",
  system = "snomed",
  column = "id",
)

lung_cancer_opensafely_snomed_codes = codelist_from_csv(
  "codelists/opensafely-lung-cancer-snomed.csv", 
  system = "snomed", 
  column = "id"
)

chemotherapy_radiotherapy_opensafely_snomed_codes = codelist_from_csv(
  "codelists/opensafely-chemotherapy-or-radiotherapy-snomed.csv", 
  system = "snomed", 
  column = "id"
)

### Patients with a haematological diseases
haematopoietic_stem_cell_transplant_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-haematopoietic-stem-cell-transplant-snomed.csv", 
  system = "snomed", 
  column = "code"
)

haematopoietic_stem_cell_transplant_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-haematopoietic-stem-cell-transplant-icd-10.csv", 
  system = "icd10", 
  column = "code"
)

haematopoietic_stem_cell_transplant_nhsd_opcs4_codes = codelist_from_csv(
  "codelists/nhsd-haematopoietic-stem-cell-transplant-opcs4.csv", 
  system = "opcs4", 
  column = "code"
)

# haematological_malignancies_nhsd_snomed_codes = codelist_from_csv(
#   "codelists/nhsd-haematological-malignancies-snomed.csv", 
#   system = "snomed", 
#   column = "code"
# )

haematological_malignancies_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-haematological-malignancies-icd-10.csv", 
  system = "icd10", 
  column = "code"
)

### Patients with renal disease

#### CKD stage 5
ckd_stage_5_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-ckd-stage-5-snomed-ct.csv", 
  system = "snomed", 
  column = "code"
)

ckd_stage_5_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-ckd-stage-5-icd-10.csv", 
  system = "icd10", 
  column = "code"
)

### Patients with liver disease
liver_disease_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-liver-cirrhosis.csv", 
  system = "snomed", 
  column = "code"
)

liver_disease_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-liver-cirrhosis-icd-10.csv", 
  system = "icd10", 
  column = "code"
)

### Immune-mediated inflammatory disorders (IMID)
immunosuppresant_drugs_dmd_codes = codelist_from_csv(
  "codelists/nhsd-immunosuppresant-drugs-pra-dmd.csv", 
  system = "snomed", 
  column = "code"
)

immunosuppresant_drugs_snomed_codes = codelist_from_csv(
  "codelists/nhsd-immunosuppresant-drugs-pra-snomed.csv", 
  system = "snomed", 
  column = "code"
)

oral_steroid_drugs_dmd_codes = codelist_from_csv(
  "codelists/nhsd-oral-steroid-drugs-pra-dmd.csv",
  system = "snomed",
  column = "dmd_id",
)

oral_steroid_drugs_snomed_codes = codelist_from_csv(
  "codelists/nhsd-oral-steroid-drugs-snomed.csv", 
  system = "snomed", 
  column = "code"
)

### Primary immune deficiencies
immunosupression_nhsd_codes = codelist_from_csv(
  "codelists/nhsd-immunosupression-pcdcluster-snomed-ct.csv",
  system = "snomed",
  column = "code",
)

## HIV/AIDs
hiv_aids_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-hiv-aids-snomed.csv", 
  system = "snomed", 
  column = "code"
)

hiv_aids_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-hiv-aids-icd10.csv", 
  system = "icd10", 
  column = "code"
)

## Solid organ transplant
solid_organ_transplant_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-transplant-spl-atriskv4-snomed-ct.csv",
  system = "snomed",
  column = "code",
)

solid_organ_transplant_nhsd_opcs4_codes = codelist_from_csv(
  "codelists/nhsd-transplant-spl-hes-opcs4.csv", 
  system = "opcs4", 
  column = "code"
)

### Rare neurological conditions

#### Multiple sclerosis
multiple_sclerosis_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-multiple-sclerosis-snomed-ct.csv",
  system = "snomed",
  column = "code",
)

multiple_sclerosis_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-multiple-sclerosis.csv",
  system = "icd10",
  column = "code",
)

#### Motor neurone disease
motor_neurone_disease_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-motor-neurone-disease-snomed-ct.csv",
  system = "snomed",
  column = "code",
)

motor_neurone_disease_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-motor-neurone-disease-icd-10.csv",
  system = "icd10",
  column = "code",
)

#### Myasthenia gravis
myasthenia_gravis_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-myasthenia-gravis-snomed-ct.csv",
  system = "snomed",
  column = "code",
)

myasthenia_gravis_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-myasthenia-gravis.csv",
  system = "icd10",
  column = "code",
)

#### Huntington’s disease
huntingtons_disease_nhsd_snomed_codes = codelist_from_csv(
  "codelists/nhsd-huntingtons-snomed-ct.csv",
  system = "snomed",
  column = "code",
)

huntingtons_disease_nhsd_icd10_codes = codelist_from_csv(
  "codelists/nhsd-huntingtons.csv",
  system = "icd10",
  column = "code",
)  

