################################################################################
#
# Description: This script produces metadata, figurs and tables to go into the
#              mabs_and_antivirvals_coverage_report.rmd 
#
# Input: /output/data/data_processed.rds
#
# Output: /output/reports/coverage/tables/table_report_stats.csv
#         /output/reports/coverage/tables/table_elig_treat_redacted.csv
#         /output/reports/coverage/tables/table_demo_clinc_breakdown_redacted.csv
#         /output/reports/coverage/tables/table_high_risk_cohort_comparison.csv
#         /output/reports/coverage/figures/figure_cum_treatment_plot.png
#         /output/reports/coverage/figures/figure_cum_eligiblity_plot.png
#
# Author(s): M Green
# Date last updated: 09/02/2022
#
################################################################################


# Preliminaries ----

## Import libraries
library(tidyverse)
library(here)
library(glue)
library(gt)
library(gtsummary)
library(reshape2)
library(stringr)

## Import custom user functions
source(here("analysis", "lib", "custom_functions.R"))

## Create output directory
output_dir <- here::here("output", "reports", "coverage")
fs::dir_create(output_dir)

## Redaction threshold
threshold = 5

## Import data
data_processed <- read_rds(here::here("output", "data", "data_processed_clean.rds"))

## Formatting variables
data_processed_clean <- data_processed %>%
  filter(has_died == 0,
         registered_eligible == 1 | registered_treated == 1) %>%
  mutate(
    
    # Age groups
    ageband = cut(
      age,
      breaks = c(12, 30, 40, 50, 60, 70, 80, Inf),
      labels = c("12-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
      right = FALSE),
  )


# Numbers for text ----
study_start <- min(data_processed_clean$elig_start, na.rm = T)
study_end <- max(data_processed_clean$elig_start, na.rm = T)
eligible_patients <- plyr::round_any(data_processed_clean %>% filter(eligibility_status == "Eligible") %>% nrow(), 10)
treated_patients <- plyr::round_any(data_processed_clean %>% filter(!is.na(treatment_date)) %>% nrow(), 10)

treated_sotrovimab <- plyr::round_any(data_processed_clean %>% filter(treatment_type == "Sotrovimab") %>% nrow(), 10)
treated_molnupiravir <- plyr::round_any(data_processed_clean %>% filter(treatment_type == "Molnupiravir") %>% nrow(), 10)
treated_casirivimab <- plyr::round_any(data_processed_clean %>% filter(treatment_type == "Casirivimab") %>% nrow(), 10)

high_risk_cohort_2plus <- plyr::round_any(subset(data_processed_clean, high_risk_group_combined_count > 1) %>% nrow(), 10)
high_risk_cohort_lower <- min(data_processed_clean$high_risk_group_combined_count, na.rm = T)
high_risk_cohort_upper <- max(data_processed_clean$high_risk_group_combined_count, na.rm = T)

deregistered <- plyr::round_any(print(length(unique(subset(data_processed, !is.na(treatment_date))$patient_id)) -
                        length(unique(subset(data_processed_clean, !is.na(treatment_date))$patient_id))), 10)

text <- data.frame(study_start, study_end, 
                   eligible_patients, treated_patients, treated_sotrovimab, treated_molnupiravir, treated_casirivimab, 
                   high_risk_cohort_2plus, high_risk_cohort_lower, high_risk_cohort_upper, deregistered)

write_csv(text, fs::path(output_dir, "table_report_stats_redacted.csv"))

print(text)


# Coverage ----

## Eligibility
plot_data_coverage <- data_processed_clean %>%
  select(elig_start, downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  group_by(elig_start) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=c(downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
           hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to="high_risk_cohort",
    values_to="n"
  ) %>%
  group_by(elig_start) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  arrange(elig_start) %>%
  complete(elig_start = seq.Date(min(elig_start, na.rm = T), max(elig_start, na.rm = T), by="day"))  %>%
  mutate(count = ifelse(is.na(n), 0, n),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10),
         high_risk_cohort = "All") %>%
  select(-n)

plot_data_coverage_groups <- data_processed_clean %>%
  select(elig_start, downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  group_by(elig_start) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=c(downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
           hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "n"
  ) %>%
  group_by(high_risk_cohort) %>%
  arrange(high_risk_cohort, elig_start) %>%
  complete(elig_start = seq.Date(min(elig_start, na.rm = T), max(elig_start, na.rm = T), by="day"))  %>%
  mutate(count = ifelse(is.na(n), 0, n),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10)) %>%
  select(-n)

plot_order <- rbind(plot_data_coverage, plot_data_coverage_groups) %>%
  group_by(high_risk_cohort) %>%
  mutate(order = max(cum_count_redacted, na.rm = T)) %>%
  arrange(desc(order)) %>%
  filter(cum_count_redacted == order) %>%
  select(high_risk_cohort, order) %>%
  distinct()

coverage_plot_data <- rbind(plot_data_coverage, plot_data_coverage_groups) %>%
  mutate(high_risk_cohort = factor(high_risk_cohort, levels = plot_order$high_risk_cohort))

write_csv(coverage_plot_data %>% select(elig_start, cum_count_redacted, high_risk_cohort), fs::path(output_dir, "table_cum_eligiblity_redacted.csv"))

print("coverage_plot_data saved")

## Treatment (all)
plot_data_treatment <- data_processed_clean %>%
  select(treatment_date, downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  group_by(treatment_date) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=c(downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
           hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "n"
  ) %>%
  group_by(treatment_date) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  arrange(treatment_date) %>%
  complete(treatment_date = seq.Date(min(treatment_date, na.rm = T), max(treatment_date, na.rm = T), by="day"))  %>%
  mutate(count = ifelse(is.na(n), 0, n),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10),
         high_risk_cohort = "All") %>%
  select(-n)

plot_data_treatment_groups <- data_processed_clean %>%
  select(treatment_date, downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  group_by(treatment_date) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=c(downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
           hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "n"
  ) %>%
  group_by(high_risk_cohort) %>%
  arrange(high_risk_cohort, treatment_date) %>%
  complete(treatment_date = seq.Date(min(treatment_date, na.rm = T), max(treatment_date, na.rm = T), by="day"))  %>%
  mutate(count = ifelse(is.na(n), 0, n),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10)) %>%
  select(-n)

plot_order <- rbind(plot_data_treatment, plot_data_treatment_groups) %>%
  group_by(high_risk_cohort) %>%
  mutate(order = max(cum_count_redacted, na.rm = T)) %>%
  arrange(desc(order)) %>%
  filter(cum_count_redacted == order) %>%
  select(high_risk_cohort, order) %>%
  distinct()

treatment_plot_data <- rbind(plot_data_treatment, plot_data_treatment_groups) %>%
  mutate(high_risk_cohort = factor(high_risk_cohort, levels = plot_order$high_risk_cohort))

write_csv(treatment_plot_data %>% select(treatment_date, cum_count_redacted, high_risk_cohort), fs::path(output_dir, "table_cum_treatment_redacted.csv"))

print("treatment_plot_data saved")


# Delivery ----

## Eligible and treated table
eligibility_table <- data_processed_clean %>%
  select(downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
           hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "Eligibile"
  ) %>%
  add_row(high_risk_cohort = "All", Eligibile = (data_processed_clean %>% filter(!is.na(elig_start)) %>% nrow())) %>%
  arrange(desc(Eligibile))

treatment_table <- data_processed_clean %>%
  mutate(All = ifelse(!is.na(high_risk_group_combined), 1, 0)) %>%
  select(treatment_type, All, downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  group_by(treatment_type) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(All, downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
           hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "Treated"
  ) %>%
  pivot_wider(
    id_cols = high_risk_cohort,
    names_from = treatment_type,
    values_from = Treated)

table_elig_treat_redacted <- left_join(eligibility_table, treatment_table, by = "high_risk_cohort") %>%
  mutate(Treated = Casirivimab + Molnupiravir + Sotrovimab) %>%
  select(high_risk_cohort, Eligibile, Treated, Sotrovimab, Molnupiravir, Casirivimab) %>%
  mutate(
  # Redact values < 8
    Eligibile = ifelse(Eligibile < threshold, NA, as.numeric(Eligibile)),
    Treated = ifelse(Treated < threshold, NA, as.numeric(Treated)),
    Sotrovimab = ifelse(Sotrovimab < threshold, NA, as.numeric(Sotrovimab)),
    Molnupiravir = ifelse(Molnupiravir < threshold, NA, as.numeric(Molnupiravir)),
    Casirivimab = ifelse(Casirivimab < threshold, NA, as.numeric(Casirivimab)),
    # Round to nearest 10
    Eligibile = plyr::round_any(Eligibile, 10),
    Treated = plyr::round_any(Treated, 10),
    Sotrovimab = plyr::round_any(as.numeric(Sotrovimab), 10),
    Molnupiravir = plyr::round_any(Molnupiravir, 10),
    Casirivimab = plyr::round_any(Casirivimab, 10))

write_csv(table_elig_treat_redacted, fs::path(output_dir, "table_elig_treat_redacted.csv"))


## Clinical and demographics table
variables <- c("ageband", "sex", "ethnicity", "imd", "region_nhs", "autism_nhsd", "care_home_primis",
               "dementia_nhsd", "learning_disability_primis", "serious_mental_illness_nhsd", 
               "housebound_opensafely", "shielded_primis", "vaccination_status")

table_demo_clinc_breakdown_base <- data_processed_clean %>%
  select(all_of(variables)) %>%
  tbl_summary()

table_demo_clinc_breakdown_base$inputs$data <- NULL

table_demo_clinc_breakdown_base <- table_demo_clinc_breakdown_base$table_body %>%
  separate(stat_0, c("stat_0","perc0"), sep = " ([(])") %>%
  select(Group = variable, Variable = label, 
         All = stat_0) %>%
  mutate(All = as.numeric(gsub(",", "", All))) %>%
  data.frame() %>%
  arrange(Group, Variable)

table_demo_clinc_breakdown <- data_processed_clean %>%
  filter(!is.na(treatment_type)) %>%
  select(treatment_type, all_of(variables)) %>%
  tbl_summary(by = treatment_type) %>%
  add_overall() %>%
  modify_header(label ~ "**Demographic/clinical characteristics**") %>%
  modify_spanning_header(c("stat_0", "stat_1", "stat_2") ~ "**Treatment Received**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  bold_labels()

table_demo_clinc_breakdown$inputs$data <- NULL

table_demo_clinc_breakdown <- table_demo_clinc_breakdown$table_body %>%
  separate(stat_0, c("stat_0","perc0"), sep = " ([(])") %>%
  separate(stat_1, c("stat_1","perc1"), sep = " ([(])") %>%
  separate(stat_2, c("stat_2","perc2"), sep = " ([(])") %>%
  separate(stat_3, c("stat_3","perc3"), sep = " ([(])") %>%
  select(Group = variable,
         Variable = label, 
         Treated = stat_0,
         Casirivimab = stat_1,
         Molnupiravir = stat_2,
         Sotrovimab = stat_3) %>%
  mutate(Treated = as.numeric(gsub(",", "", Treated)),
         Casirivimab = as.numeric(gsub(",", "", Casirivimab)),
         Molnupiravir = as.numeric(gsub(",", "", Molnupiravir)),
         Sotrovimab = as.numeric(gsub(",", "", Sotrovimab))) %>%
  data.frame() %>%
  arrange(Group, Variable)

table_demo_clinc_breakdown_redacted <- left_join(table_demo_clinc_breakdown_base, table_demo_clinc_breakdown, by = c("Group", "Variable")) %>%
  # Redact values < 8
  mutate(All = ifelse(All < threshold, NA, as.numeric(All)),
         Treated = ifelse(Treated < threshold, NA, as.numeric(Treated)),
         Casirivimab = ifelse(Casirivimab < threshold, NA, as.numeric(Casirivimab)),
         Molnupiravir = ifelse(Molnupiravir < threshold, NA, as.numeric(Molnupiravir)),
         Sotrovimab = ifelse(Sotrovimab < threshold, NA, as.numeric(Sotrovimab))
  ) %>%
  # Round to nearest 10
  mutate(All = plyr::round_any(All, 10),
         Treated = plyr::round_any(Treated, 10),
         Casirivimab = plyr::round_any(as.numeric(Casirivimab), 10),
         Molnupiravir = plyr::round_any(Molnupiravir, 10),
         Sotrovimab = plyr::round_any(Sotrovimab, 10))

write_csv(table_demo_clinc_breakdown_redacted, fs::path(output_dir, "table_demo_clinc_breakdown_redacted.csv"))


# Concordance with guidance ----
non_elig_treated <-  data_processed_clean %>%
  filter(!is.na(treatment_date),
         eligibility_status == "Treated") %>%
  mutate(
    patient_id,
    alive = (has_died == 0),
    registered = (registered_eligible == 1 | registered_treated == 1),
    has_positive_covid_test = (covid_test_positive == 1),
    no_positive_covid_test_previous_30_days = (covid_positive_previous_30_days != 1),
    high_risk_group_nhsd = !is.na(high_risk_group_nhsd_combined),
    no_primary_covid_hospital_admission_last_30_days = (is.na(primary_covid_hospital_discharge_date) | 
                                                  primary_covid_hospital_discharge_date < (treatment_date - 30) & 
                                                  primary_covid_hospital_discharge_date > (treatment_date)),
    no_covid_hospital_admission_last_30_days = (is.na(any_covid_hospital_discharge_date) | 
                                                  any_covid_hospital_discharge_date < (treatment_date - 30) & 
                                                  any_covid_hospital_discharge_date > (treatment_date)),
    aged_over_12 = (age >= 12),
    treated_within_5_days = ((tb_postest_treat <= 5 & tb_postest_treat >= 0) | is.na(tb_postest_treat)),
    high_risk_group = !is.na(high_risk_cohort_covid_therapeutics),
    
    include = (
      alive &
        registered & 
        has_positive_covid_test & 
        no_positive_covid_test_previous_30_days & 
        treated_within_5_days & 
        high_risk_group_nhsd & 
        no_primary_covid_hospital_admission_last_30_days &
        no_covid_hospital_admission_last_30_days &
        aged_over_12 &
        high_risk_group)
  )

data_flowchart <- non_elig_treated %>%
  ungroup() %>%
  transmute(
    c0_all = TRUE,
    c1_alive_and_registered = c0_all & alive & registered,
    c1_alive = c0_all & alive,
    c1_registered = c0_all & registered,
    c2_has_positive_covid_test = c0_all & has_positive_covid_test,
    c3_no_positive_covid_test_previous_30_days = c0_all & no_positive_covid_test_previous_30_days,
    c4_high_risk_group_nhsd = c0_all & high_risk_group_nhsd,
    c5_no_primary_covid_hospital_admission_last_30_days = c0_all & no_primary_covid_hospital_admission_last_30_days,
    c5_no_covid_hospital_admission_last_30_days = c0_all & no_covid_hospital_admission_last_30_days,
    c6_aged_over_12 = c0_all & aged_over_12,
    c7_treated_within_5_days = c0_all & treated_within_5_days,
    c9_high_risk_group = c0_all & high_risk_group
  )  %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=everything(),
    names_to="criteria",
    values_to="n"
  )  %>%
  mutate(n = ifelse(n < 5, NA, n),
         n = plyr::round_any(as.numeric(n), 5))

write_csv(data_flowchart, fs::path(output_dir, "table_non_elig_flowchart_redacted.csv"))

print("data_flowchart saved")


# High risk patient cohorts ----
print(table(data_processed_clean$match))

high_risk_cohort_comparison_redacted <- data_processed_clean %>%
  filter(!is.na(treatment_date),
         eligibility_status == "Treated") %>%
  filter(match == FALSE) %>%
  select(high_risk_group_nhsd_combined, high_risk_cohort_covid_therapeutics) %>%
  group_by(high_risk_group_nhsd_combined, high_risk_cohort_covid_therapeutics) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(n = ifelse(n < 5, NA, n),
         n = plyr::round_any(as.numeric(n), 5))

write_csv(high_risk_cohort_comparison_redacted, fs::path(output_dir, "table_non_elig_high_risk_cohort_comparison_redacted.csv"))


# Time to treatment ----
all <- data_processed_clean %>%
  filter(!is.na(treatment_type)) %>%
  group_by(tb_postest_treat, treatment_type) %>%
  tally() %>%
  mutate(high_risk_group_elig = "All",
         n = ifelse(n < 5, NA, n),
         n = plyr::round_any(as.numeric(n), 5))

groups <- data_processed_clean %>%
  filter(!is.na(treatment_type)) %>%
  select(tb_postest_treat, downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions) %>%
  group_by(tb_postest_treat) %>%
  filter(!is.na(tb_postest_treat)) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(downs_syndrome, sickle_cell_disease, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
             hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "Count"
  ) %>%
  mutate(Count = ifelse(Count < 5, NA, Count),
         Count = plyr::round_any(as.numeric(Count), 5))

write_csv(rbind(all, groups), fs::path(output_dir, "table_time_to_treat_redacted.csv"))


# COVID-19 related events ----
all <- data_processed_clean %>%
  mutate(treated_status = ifelse(!is.na(treatment_type), "treated", "not treated"),
         covid_hospitalisation_outcome = ifelse(covid_hospitalisation_outcome_date > covid_test_positive_date, 1, 0),
         covid_hospitalisation_outcome = ifelse(is.na(covid_hospitalisation_outcome), 0, covid_hospitalisation_outcome),
         covid_hospitalisation_critical_care = ifelse(!is.na(covid_hospitalisation_critical_care), 1, 0)) %>%
  select(covid_positive_test_30_days_post_elig_or_treat, covid_hospitalisation_outcome, covid_hospitalisation_critical_care, 
         covid_death, treated_status) %>%
  tibble() %>%
  group_by(treated_status) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(covid_positive_test_30_days_post_elig_or_treat, covid_hospitalisation_outcome, covid_hospitalisation_critical_care,
             covid_death),
    names_to = "Outcome",
    values_to = "Count"
  ) %>%
  mutate(Count = ifelse(Count < 5, NA, Count),
         Count = plyr::round_any(as.numeric(Count), 5))


write_csv(all, fs::path(output_dir, "covid_outcomes_redacted.csv"))





