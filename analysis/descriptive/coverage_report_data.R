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
# Date last updated: 25/02/2022
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
output_dir <- here::here("output", "coverage")
output_dir2 <- here::here("output", "coverage", "for-checks")

fs::dir_create(output_dir)
fs::dir_create(output_dir2)

## Redaction threshold
threshold = 5

## Import data
data_processed <- read_rds(here::here("output", "data", "data_processed_clean.rds"))

## Remove patients no longer registered at time of treatment and format variables
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
    
    # Vaccination status
    vaccination_status = fct_case_when(
      vaccination_status == "Un-vaccinated" ~ "Un-vaccinated",
      vaccination_status == "Un-vaccinated (declined)" ~ "Un-vaccinated (declined)",
      vaccination_status== "One vaccination" ~ "One vaccination",
      vaccination_status == "Two vaccinations" ~ "Two vaccinations",
      vaccination_status == "Three or more vaccinations" ~ "Three or more vaccinations",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_
    )
    
  )


# Numbers for text ----
print(dim(data_processed_clean))
print(length(unique(data_processed_clean$patient_id)))

study_start <- min(data_processed_clean$elig_start, na.rm = T)
study_end <- max(data_processed_clean$elig_start, na.rm = T)
eligible_patients <- plyr::round_any(data_processed_clean %>% nrow(), 10)
treated_patients <- plyr::round_any(data_processed_clean %>% filter(!is.na(treatment_date)) %>% nrow(), 10)

treated_paxlovid <- plyr::round_any(data_processed_clean %>% filter(treatment_type == "Paxlovid") %>% nrow(), 10)
treated_sotrovimab <- plyr::round_any(data_processed_clean %>% filter(treatment_type == "Sotrovimab") %>% nrow(), 10)
treated_remdesivir <- plyr::round_any(data_processed_clean %>% filter(treatment_type == "Remdesivir") %>% nrow(), 10)
treated_molnupiravir <- plyr::round_any(data_processed_clean %>% filter(treatment_type == "Molnupiravir") %>% nrow(), 10)
treated_casirivimab <- plyr::round_any(data_processed_clean %>% filter(treatment_type == "Casirivimab") %>% nrow(), 10)

print("High risk cohort count")
hrc_counts <- data_processed_clean %>% 
  group_by(high_risk_group_combined_count) %>% 
  select(high_risk_group_combined_count) %>% 
  tally() %>%
  filter(n>5)
print(hrc_counts)

high_risk_cohort_2plus <- plyr::round_any(subset(data_processed_clean, high_risk_group_combined_count > 1) %>% nrow(), 10)
high_risk_cohort_lower <- min(hrc_counts$high_risk_group_combined_count, na.rm = T)
high_risk_cohort_upper <- max(hrc_counts$high_risk_group_combined_count, na.rm = T)

deregistered <- plyr::round_any(print(length(unique(subset(data_processed, !is.na(treatment_date))$patient_id)) -
                        length(unique(subset(data_processed_clean, !is.na(treatment_date))$patient_id))), 10)

text <- data.frame(study_start, study_end, 
                   eligible_patients, treated_patients, treated_paxlovid, treated_sotrovimab, treated_remdesivir, treated_molnupiravir, 
                   treated_casirivimab, high_risk_cohort_2plus, high_risk_cohort_lower, high_risk_cohort_upper, deregistered)

write_csv(text, fs::path(output_dir, "table_report_stats_redacted.csv"))

print(text)


# Eligibility for treatment ----

## Cumulative total of eligible patients 
plot_data_coverage <- data_processed_clean %>%
  select(elig_start)  %>%
  group_by(elig_start) %>%
  tally() %>%
  ungroup() %>%
  arrange(elig_start) %>%
  complete(elig_start = seq.Date(min(elig_start, na.rm = T), max(elig_start, na.rm = T), by="day"))  %>%
  mutate(count = ifelse(is.na(n), 0, n),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10),
         high_risk_cohort = "All") %>%
  select(-n)

plot_data_coverage_groups <- data_processed_clean %>%
  filter(!is.na(elig_start)) %>%
  select(elig_start, downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  group_by(elig_start) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=c(downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
           hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "n"
  ) %>%
  group_by(high_risk_cohort, elig_start) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  arrange(high_risk_cohort, elig_start) %>%
  group_by(high_risk_cohort) %>%
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
write_csv(coverage_plot_data, fs::path(output_dir2, "table_cum_eligiblity.csv"))

print("coverage_plot_data saved")


# Coverage of COVID-19 treatment ----

## Cumulative total of treated patients
plot_data_treatment <- data_processed_clean %>%
  filter(!is.na(treatment_date)) %>%
  select(treatment_date)  %>%
  group_by(treatment_date) %>%
  tally() %>%
  ungroup() %>%
  arrange(treatment_date) %>%
  complete(treatment_date = seq.Date(min(treatment_date, na.rm = T), max(treatment_date, na.rm = T), by="day"))  %>%
  mutate(count = ifelse(is.na(n), 0, n),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10),
         high_risk_cohort = "All") %>%
  select(-n)

plot_data_treatment_groups <- data_processed_clean %>%
  filter(!is.na(treatment_date)) %>%
  select(treatment_date, downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  group_by(treatment_date) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=c(downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
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
write_csv(treatment_plot_data, fs::path(output_dir2, "table_cum_treatment.csv"))

print("treatment_plot_data saved")

## Cumulative total of treated types
plot_data_treatment_type <- data_processed_clean %>%
  filter(!is.na(treatment_date)) %>%
  select(treatment_date, treatment_type)  %>%
  rbind(data_processed_clean %>%
          filter(!is.na(treatment_date)) %>%
          select(treatment_date, treatment_type) %>%
          mutate(treatment_type = "All")) %>%
  group_by(treatment_date, treatment_type) %>%
  tally() %>%
  group_by(treatment_type) %>%
  arrange(treatment_type, treatment_date) %>%
  complete(treatment_date = seq.Date(min(treatment_date, na.rm = T), max(treatment_date, na.rm = T), by="day"))  %>%
  mutate(count = ifelse(is.na(n), 0, n),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10)) %>%
  select(-n) %>%
  arrange(treatment_type, treatment_date)

plot_order <- plot_data_treatment_type %>%
  group_by(treatment_type) %>%
  mutate(order = max(cum_count_redacted, na.rm = T)) %>%
  arrange(desc(order)) %>%
  filter(cum_count_redacted == order) %>%
  select(treatment_type, order) %>%
  distinct()

treatment_plot_data <- plot_data_treatment_type %>%
  mutate(treatment_type = factor(treatment_type, levels = plot_order$treatment_type))

write_csv(treatment_plot_data %>% select(treatment_date, cum_count_redacted, treatment_type), fs::path(output_dir, "table_cum_treatment_type_redacted.csv"))
write_csv(treatment_plot_data, fs::path(output_dir2, "table_cum_treatment_type.csv"))

print("treatment_type_plot_data saved")

## Proportion treated
plot_data_treated_groups <- data_processed_clean %>%
  filter(!is.na(treatment_type)) %>%
  mutate(week = cut(elig_start -2 , "week")) %>%
  select(week, treatment_type, downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  rbind(data_processed_clean %>%
          filter(!is.na(treatment_type)) %>%
          mutate(treatment_type = "All",
                 week = cut(elig_start -2 , "week")) %>%
          select(week, treatment_type, downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
                 hiv_aids, solid_organ_transplant, rare_neurological_conditions)) %>%
  group_by(treatment_type, week) %>%  
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
             hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "n"
  ) %>%
  group_by(high_risk_cohort, treatment_type, week) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  group_by(high_risk_cohort, treatment_type) %>%
  mutate(count = ifelse(is.na(n), 0, n),
         count_redacted =  plyr::round_any(count, 10)) %>%
  select(-n) %>%
  arrange(high_risk_cohort, treatment_type, week)

plot_data_treated_all <- data_processed_clean %>%
  filter(!is.na(treatment_type)) %>%
  mutate(week = cut(elig_start -2 , "week")) %>%
  select(week, treatment_type)  %>%
  rbind(data_processed_clean %>%
          filter(!is.na(treatment_type)) %>%
          mutate(treatment_type = "All",
                 week = cut(elig_start -2 , "week")) %>%
          select(week, treatment_type)) %>%
  group_by(treatment_type, week) %>%  
  tally() %>%
  mutate(count = ifelse(is.na(n), 0, n),
         count_redacted =  plyr::round_any(count, 10),
         high_risk_cohort = "All") %>%
  select(-n) %>%
  arrange(high_risk_cohort, treatment_type, week) %>%
  rbind(plot_data_treated_groups)

plot_data_groups <- data_processed_clean %>%
  mutate(week = cut(elig_start -2 , "week")) %>%
  select(week, downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  group_by(week) %>%  
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
             hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "n"
  ) %>%
  group_by(high_risk_cohort, week) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  group_by(high_risk_cohort) %>%
  mutate(count = ifelse(is.na(n), 0, n),
         count_redacted =  plyr::round_any(count, 10)) %>%
  select(-n) %>%
  arrange(high_risk_cohort, week)

plot_data_all <- data_processed_clean %>%
  mutate(week = cut(elig_start -2 , "week")) %>%
  select(week)  %>%
  rbind(data_processed_clean %>%
          mutate(week = cut(elig_start -2 , "week")) %>%
          select(week)) %>%
  group_by(week) %>%  
  tally() %>%
  mutate(count = ifelse(is.na(n), 0, n),
         count_redacted =  plyr::round_any(count, 10),
         high_risk_cohort = "All") %>%
  select(-n) %>%
  arrange(high_risk_cohort, week) %>%
  rbind(plot_data_groups)

print(head(plot_data_all))

plot_data_prop_treated <- left_join(plot_data_treated_all, plot_data_all, 
                                    by = c("high_risk_cohort", "week")) %>%
  mutate(week = as.Date(week) - 2,
         Treated = ifelse(is.na(count_redacted.x), 0, count_redacted.x),
         Total = ifelse(is.na(count_redacted.y), 0, count_redacted.y),
         prop = Treated/Total,
         prop_redacted = ifelse((Total < threshold | Treated < threshold), NA, round(prop, digits = 4)))

write_csv(plot_data_prop_treated %>% select(elig_start = week, prop_redacted, treatment_type, high_risk_cohort) %>% filter(high_risk_cohort == "All"), 
          fs::path(output_dir, "table_prop_treated_redacted.csv"))

write_csv(plot_data_prop_treated, fs::path(output_dir2, "table_prop_treated.csv"))

print("prop_treated_data saved")

## Eligible and treated table
eligibility_table <- data_processed_clean %>%
  select(downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
           hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "Eligibile"
  ) %>%
  add_row(high_risk_cohort = "All", Eligibile = (data_processed_clean %>% filter(!is.na(elig_start)) %>% nrow())) %>%
  arrange(desc(Eligibile))

treatment_table <- data_processed_clean %>%
  filter(!is.na(treatment_type)) %>%
  mutate(All = ifelse(!is.na(high_risk_group_combined), 1, 0)) %>%
  select(treatment_type, All, downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions)  %>%
  group_by(treatment_type) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(All, downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
           hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "Treated"
  ) %>%
  pivot_wider(
    id_cols = high_risk_cohort,
    names_from = treatment_type,
    values_from = Treated)

table_elig_treat_redacted <- left_join(eligibility_table, treatment_table, by = "high_risk_cohort") %>%
  mutate(Treated = Casirivimab + Molnupiravir + Sotrovimab + Paxlovid + Remdesivir) %>%
  select(high_risk_cohort, Eligibile, Treated, Paxlovid, Sotrovimab, Remdesivir, Molnupiravir, Casirivimab) %>%
  mutate(
  # Redact values < 8
    Eligibile = ifelse(Eligibile < threshold, NA, as.numeric(Eligibile)),
    Treated = ifelse(Treated < threshold, NA, as.numeric(Treated)),
    Paxlovid = ifelse(Paxlovid < threshold, NA, as.numeric(Paxlovid)),
    Sotrovimab = ifelse(Sotrovimab < threshold, NA, as.numeric(Sotrovimab)),
    Remdesivir = ifelse(Remdesivir < threshold, NA, as.numeric(Remdesivir)),
    Molnupiravir = ifelse(Molnupiravir < threshold, NA, as.numeric(Molnupiravir)),
    Casirivimab = ifelse(Casirivimab < threshold, NA, as.numeric(Casirivimab)),
    # Round to nearest 10
    Eligibile = plyr::round_any(Eligibile, 10),
    Treated = plyr::round_any(Treated, 10),
    Paxlovid = plyr::round_any(as.numeric(Paxlovid), 10),
    Sotrovimab = plyr::round_any(as.numeric(Sotrovimab), 10),
    Remdesivir = plyr::round_any(as.numeric(Remdesivir), 10),
    Molnupiravir = plyr::round_any(Molnupiravir, 10),
    Casirivimab = plyr::round_any(Casirivimab, 10))

write_csv(table_elig_treat_redacted, fs::path(output_dir, "table_elig_treat_redacted.csv"))


## Clinical and demographics table
variables <- c("ageband", "sex", "ethnicity", "imd", "rural_urban", "region_nhs", "autism_nhsd", "care_home_primis",
               "dementia_nhsd", "learning_disability_primis", "serious_mental_illness_nhsd", 
               "housebound_opensafely", "shielded_primis", "sickle_cell_disease_nhsd", "vaccination_status")

table_demo_clinc_breakdown_base <- data_processed_clean %>%
  select(all_of(variables)) %>%
  tbl_summary()

table_demo_clinc_breakdown_base$inputs$data <- NULL

table_demo_clinc_breakdown_base <- table_demo_clinc_breakdown_base$table_body %>%
  separate(stat_0, c("stat_0","perc0"), sep = " ([(])") %>%
  select(Group = variable, Variable = label, 
         All = stat_0) %>%
  mutate(All = as.numeric(gsub(",", "", All))) %>%
  data.frame()

table_demo_clinc_breakdown <- data_processed_clean %>%
  filter(!is.na(treatment_type)) %>%
  select(treatment_type, all_of(variables)) %>%
  tbl_summary(by = treatment_type) %>%
  add_overall() %>%
  modify_header(label ~ "**Demographic/clinical characteristics**") %>%
  modify_spanning_header(c("stat_0", "stat_1", "stat_2", "stat_3", "stat_4","stat_5") ~ "**Treatment Received**") %>%
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
  separate(stat_4, c("stat_4","perc4"), sep = " ([(])") %>%
  separate(stat_5, c("stat_5","perc5"), sep = " ([(])") %>%
  select(Group = variable,
         Variable = label, 
         Treated = stat_0,
         Sotrovimab = stat_5,
         Remedesivir = stat_4,
         Paxlovid = stat_3,
         Molnupiravir = stat_2,
         Casirivimab = stat_1) %>%
  mutate(Treated = as.numeric(gsub(",", "", Treated)),
         Casirivimab = as.numeric(gsub(",", "", Casirivimab)),
         Molnupiravir = as.numeric(gsub(",", "", Molnupiravir)),
         Paxlovid = as.numeric(gsub(",", "", Paxlovid)),
         Remedesivir = as.numeric(gsub(",", "", Remedesivir)),
         Sotrovimab = as.numeric(gsub(",", "", Sotrovimab))) %>%
  data.frame()

table_demo_clinc_breakdown_redacted <- left_join(table_demo_clinc_breakdown_base, table_demo_clinc_breakdown, by = c("Group", "Variable")) %>%
  # Redact values < 8
  mutate(All = ifelse(All < threshold, NA, as.numeric(All)),
         Treated = ifelse(Treated < threshold, NA, as.numeric(Treated)),
         Casirivimab = ifelse(Casirivimab < threshold, NA, as.numeric(Casirivimab)),
         Molnupiravir = ifelse(Molnupiravir < threshold, NA, as.numeric(Molnupiravir)),
         Sotrovimab = ifelse(Sotrovimab < threshold, NA, as.numeric(Sotrovimab)),
         Remedesivir = as.numeric(ifelse(Remedesivir < threshold, NA, as.numeric(Remedesivir))),
         Paxlovid = ifelse(Paxlovid < threshold, NA, as.numeric(Paxlovid))
         ) %>%
  # Round to nearest 10
  mutate(All = plyr::round_any(All, 10),
         Treated = plyr::round_any(Treated, 10),
         Casirivimab = plyr::round_any(as.numeric(Casirivimab), 10),
         Molnupiravir = plyr::round_any(Molnupiravir, 10),
         Sotrovimab = plyr::round_any(Sotrovimab, 10),
         Remedesivir = plyr::round_any(Remedesivir, 10),
         Paxlovid = plyr::round_any(Paxlovid, 10))

write_csv(table_demo_clinc_breakdown_redacted, fs::path(output_dir, "table_demo_clinc_breakdown_redacted.csv"))


# Concordance with guidance ----
non_elig_treated <-  data_processed_clean %>%
  filter(!is.na(treatment_date),
         eligibility_status == "Treated"
         ) %>%
  mutate(
    patient_id,
    no_positive_covid_test = (covid_test_positive != 1),
    positive_covid_test_previous_30_days = (covid_positive_previous_30_days == 1),
    no_high_risk_group_nhsd = is.na(high_risk_group_nhsd_combined),
    no_high_risk_group_match =  is.na(match) & !is.na(high_risk_group_nhsd_combined),
    primary_covid_hospital_admission_last_30_days = (!is.na(primary_covid_hospital_discharge_date) | 
                                                  primary_covid_hospital_discharge_date > (treatment_date - 30) & 
                                                  primary_covid_hospital_discharge_date < (treatment_date)),
    any_covid_hospital_admission_last_30_days = (!is.na(any_covid_hospital_discharge_date) | 
                                                  any_covid_hospital_discharge_date > (treatment_date - 30) & 
                                                  any_covid_hospital_discharge_date < (treatment_date)),
    
    include = (
      no_positive_covid_test  &
        positive_covid_test_previous_30_days &
        no_high_risk_group_nhsd &
        no_high_risk_group_match &
        primary_covid_hospital_admission_last_30_days &
        any_covid_hospital_admission_last_30_days
    )
  )

data_flowchart <- non_elig_treated %>%
  ungroup() %>%
  transmute(
    c_all = TRUE,
    c_no_positive_covid_test = c_all & no_positive_covid_test,
    c_positive_covid_test_previous_30_days = c_all & positive_covid_test_previous_30_days,
    c_no_high_risk_group_nhsd = c_all & no_high_risk_group_nhsd,
    c_no_high_risk_group_match = c_all & no_high_risk_group_match,
    c_primary_covid_hospital_admission_last_30_days = c_all & primary_covid_hospital_admission_last_30_days,
    c_any_covid_hospital_admission_last_30_days =  c_all & any_covid_hospital_admission_last_30_days)  %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=everything(),
    names_to="criteria",
    values_to="n"
  )  %>%
  mutate(n = ifelse(n < 8 & n > 0, "<8", n),
         n = ifelse(n != "<8", plyr::round_any(as.numeric(n), 10), n))

all_treated <-  data_processed_clean %>%
  filter(!is.na(treatment_date),
  ) %>%
  mutate(
    patient_id,
    not_symptomatic_covid_test = (symptomatic_covid_test != "Y"),
    not_treated_within_5_days_paxlovid = ((tb_postest_treat > 5 & treatment_type == "Paxlovid") | 
                                            (tb_postest_treat < 0 & treatment_type == "Paxlovid")) & !is.na(tb_postest_treat),
    renal_liver_paxlovid = ((renal_disease == 1 | liver_disease == 0) & treatment_type == "Paxlovid"),
    aged_under_18_paxlovid = (age < 18 & treatment_type == "Paxlovid"),
    pregnancy_paxlovid = (pregnancy != 1 & treatment_type == "Paxlovid"),
    
    not_treated_within_5_days_sotrovimab = (((tb_postest_treat > 5 & treatment_type == "Sotrovimab") | 
                                               (tb_postest_treat < 0 & treatment_type == "Sotrovimab")) & !is.na(tb_postest_treat)),
    aged_under_12_sotrovimab = (age < 12 & treatment_type == "Sotrovimab"),
    weight_sotrovimab = (weight <= 40 & (age >=12 | age <= 17) & treatment_type == "Sotrovimab"),
    
    not_treated_within_7_days_remdesivir = ((tb_postest_treat > 7 & treatment_type == "Remdesivir") | 
                                              (tb_postest_treat < 0 & treatment_type == "Remdesivir")) & !is.na(tb_postest_treat),
    age_under_12_remdesivir = (age < 12 & treatment_type == "Remdesivir"),
    weight_remdesivir = (weight < 40 & (age >=12 | age <= 17) & treatment_type == "Remdesivir"),
    
    not_treated_within_7_days_molnupiravir = ((tb_postest_treat > 5 & treatment_type == "Molnupiravir") | 
                                                (tb_postest_treat < 0 & treatment_type == "Molnupiravir")) & !is.na(tb_postest_treat),
    age_under_18_molnupiravir = (age < 18 & treatment_type =="Molnupiravir"),
    pregnancy_molnupiravir  = (pregnancy == 1 & treatment_type == "Molnupiravir"),
    
    include = (
      not_symptomatic_covid_test &
      not_treated_within_5_days_paxlovid &
        renal_liver_paxlovid &
        aged_under_18_paxlovid &
        pregnancy_paxlovid &
        not_treated_within_5_days_sotrovimab &
        aged_under_12_sotrovimab &
        weight_sotrovimab &
        not_treated_within_7_days_remdesivir &
        age_under_12_remdesivir &
        weight_remdesivir &
        not_treated_within_7_days_molnupiravir &
        pregnancy_molnupiravir
    )
  )

data_flowchart2 <- all_treated %>%
  ungroup() %>%
  transmute(
    c_all = TRUE,
    c_not_symptomatic_covid_test = c_all & not_symptomatic_covid_test,
    c_not_treated_within_5_days_paxlovid = c_all & not_treated_within_5_days_paxlovid,
    c_renal_liver_paxlovid = c_all & renal_liver_paxlovid,
    c_aged_under_18_paxlovid = c_all & aged_under_18_paxlovid,
    c_pregnancy_paxlovid = c_all & pregnancy_paxlovid,
    c_not_treated_within_5_days_sotrovimab = c_all & not_treated_within_5_days_sotrovimab,
    c_aged_under_12_sotrovimab = c_all & aged_under_12_sotrovimab,
    c_weight_sotrovimab = c_all & weight_sotrovimab,
    c_not_treated_within_7_days_remdesivir = c_all & not_treated_within_7_days_remdesivir,
    c_age_under_12_remdesivir = c_all & age_under_12_remdesivir,
    c_weight_remdesivir = c_all & weight_remdesivir,
    c_not_treated_within_7_days_molnupiravir = c_all & not_treated_within_7_days_molnupiravir,
    c_pregnancy_molnupiravir = c_all & pregnancy_molnupiravir)  %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=everything(),
    names_to="criteria",
    values_to="n"
  )  %>%
  mutate(n = ifelse(n < 8 & n > 0, "<8", n),
         n = ifelse(n != "<8", plyr::round_any(as.numeric(n), 10), n))

write_csv(rbind(data_flowchart, data_flowchart2), fs::path(output_dir, "table_non_elig_flowchart_redacted.csv"))

print("data_flowchart saved")


# High risk patient cohorts ----
print(table(data_processed_clean$match))

high_risk_cohort_comparison_redacted <- data_processed_clean %>%
  filter(!is.na(treatment_date)) %>%
  filter(is.na(match)) %>%
  select(high_risk_group_nhsd_combined, high_risk_cohort_covid_therapeutics) %>%
  group_by(high_risk_group_nhsd_combined, high_risk_cohort_covid_therapeutics) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(n = ifelse(n < 8, NA, n),
         n = plyr::round_any(as.numeric(n), 10))

write_csv(high_risk_cohort_comparison_redacted, fs::path(output_dir, "table_non_elig_high_risk_cohort_comparison_redacted.csv"))


# Time to treatment ----
all <- data_processed_clean %>%
  mutate(tb = ifelse(is.na(tb_postest_treat), tb_postest_treat, tb_postest_treat)) %>%
  filter(!is.na(treatment_type)) %>%
  group_by(tb, treatment_type) %>%
  tally() %>%
  mutate(high_risk_cohort = "All",
         n = ifelse(n < 8, NA, n),
         n = plyr::round_any(as.numeric(n), 10)) %>%
  filter(!is.na(n))

groups <- data_processed_clean %>%
  filter(!is.na(treatment_type)) %>%
  mutate(tb = ifelse(is.na(tb_postest_treat), tb_postest_treat, tb_postest_treat)) %>%
  select(tb, downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
         hiv_aids, solid_organ_transplant, rare_neurological_conditions) %>%
  group_by(tb) %>%
  filter(!is.na(tb)) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, immunosupression, 
             hiv_aids, solid_organ_transplant, rare_neurological_conditions),
    names_to = "high_risk_cohort",
    values_to = "n"
  ) %>%
  mutate(n = ifelse(n < 8, NA, n),
         n = plyr::round_any(as.numeric(n), 10)) %>%
  filter(!is.na(n))

groups2 <- data_processed_clean %>%
  filter(!is.na(treatment_type)) %>%
  mutate(tb = ifelse(is.na(tb_postest_treat), tb_postest_treat, tb_postest_treat)) %>%
  select(tb, autism_nhsd, care_home_primis, dementia_nhsd, learning_disability_primis, serious_mental_illness_nhsd, 
         housebound_opensafely, shielded_primis) %>%
  group_by(tb) %>%
  filter(!is.na(tb)) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(autism_nhsd, care_home_primis, dementia_nhsd, learning_disability_primis, serious_mental_illness_nhsd, 
             housebound_opensafely, shielded_primis),
    names_to = "group",
    values_to = "n"
  ) %>%
  mutate(n = ifelse(n < 8, NA, n),
         n = plyr::round_any(as.numeric(n), 10),
         variable = group) %>%
  filter(!is.na(n))

groups_tte <- c("ageband", "sex", "ethnicity", "imd", "rural_urban", "region_nhs", "vaccination_status")

for (i in 1:length(groups_tte)) {
  
  group_tte <- data_processed_clean %>%
    filter(!is.na(treatment_type)) %>%
    mutate(tb = ifelse(is.na(tb_postest_treat), tb_postest_treat, tb_postest_treat)) %>%
    filter(!is.na(tb)) %>%
    select(tb, variable = groups_tte[i]) %>%
    group_by_all() %>% 
    tally() %>%
    mutate(n = ifelse(n < 8, NA, n),
           n = plyr::round_any(as.numeric(n), 10),
           group = groups_tte[i]) %>%
    filter(!is.na(n))
  
  groups2 <- rbind(groups2, group_tte)
  
  print(i)
  
}

groups2 <- groups2 %>%
  arrange(variable, group, tb)

write_csv(rbind(all, groups), fs::path(output_dir, "table_time_to_treat_redacted.csv"))
write_csv(groups2, fs::path(output_dir, "table_time_to_treat_groups_redacted.csv"))


# COVID-19 related events ----
all <- data_processed_clean %>%
  mutate(treated_status = ifelse(!is.na(treatment_type), "treated", "not treated"),
         covid_hospitalisation_outcome = ifelse(covid_hospitalisation_outcome_date > covid_test_positive_date, 1, 0),
         covid_hospitalisation_outcome = ifelse(is.na(covid_hospitalisation_outcome), 0, covid_hospitalisation_outcome),
         covid_hospitalisation_critical_care = ifelse(!is.na(covid_hospitalisation_critical_care), 1, 0)) %>%
  select(covid_positive_test_30_days_post_elig_or_treat, covid_hospitalisation_outcome, covid_hospitalisation_critical_care, 
         covid_death, any_death, treated_status) %>%
  tibble() %>%
  group_by(treated_status) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(covid_positive_test_30_days_post_elig_or_treat, covid_hospitalisation_outcome, covid_hospitalisation_critical_care,
             covid_death, any_death),
    names_to = "Outcome",
    values_to = "Count"
  ) %>%
  mutate(Count = ifelse(Count < 5, NA, Count),
         Count = plyr::round_any(as.numeric(Count), 10)) %>%
  pivot_wider(names_from = treated_status, values_from = Count) %>%
  mutate(treated_tot = data_processed_clean %>% filter(!is.na(treatment_type)) %>% nrow(),
         treated_perc = round(treated/treated_tot*100, digits = 0),
         untreated_tot = data_processed_clean %>% filter(is.na(treatment_type)) %>% nrow(),
         untreated_perc = round(`not treated`/untreated_tot*100, digits = 0)) %>%
  select(Outcome, `not treated`, untreated_perc, treated, treated_perc)

all$Outcome <- c("Positive SARS-CoV-2 test, 30 days or more since start date",
                 "Required hospitalisation with COVID-19 recorded as the primary diagnosis, one or more days after start date",
                 "COVID-19-related critical care admission, one or more days after start date",
                 "COVID-19 related death, one or more days after start date",
                 "Any death, one or more days after start date")

write_csv(all, fs::path(output_dir, "table_covid_outcomes_redacted.csv"))





