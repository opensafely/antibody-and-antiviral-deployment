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
  
  
