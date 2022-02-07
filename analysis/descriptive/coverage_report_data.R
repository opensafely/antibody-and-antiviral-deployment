################################################################################
#
# Description: This script produces metadata, figurs and tables to go into the
#              mabs_and_antivirvals_coverage_report.rmd 
#
# Input: /output/data/data_processed.rds
#
# Output: /output/reports/coverage/tables/report_stats.csv
#         /output/reports/coverage/tables/table_elig_treat_redacted.csv
#         /output/reports/coverage/tables/table_demo_clinc_breakdown_redacted.csv
#         /output/reports/coverage/figures/cum_treatment_plot.png
#         /output/reports/coverage/figures/cum_eligiblity_plot.png
#
# Author(s): M Green
# Date last updated: 04/02/2022
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

## Import custom user functions
source(here("analysis", "lib", "custom_functions.R"))

## Create output directory
fs::dir_create(here::here("output", "reports", "coverage", "tables"))
fs::dir_create(here::here("output", "reports", "coverage", "figures"))

## Import data
data_processed <- read_rds(here::here("output", "data", "data_processed.rds"))

## Redaction threshold
threshold = 8

# Format data ----

## Apply eligibility and exclusion criteria
data_processed_eligible <- data_processed %>%
   filter(
    # Apply eligibility criteria
    covid_test_positive == 1,
    covid_positive_previous_30_days != 1,
    (tb_postest_treat <= 5 & tb_postest_treat >= 0) | is.na(tb_postest_treat),
    !is.na(high_risk_group_nhsd),
    
    # Apply exclusion criteria
    is.na(covid_hospital_admission_date) | covid_hospital_admission_date < (elig_start - 30) & covid_hospital_admission_date > (elig_start),
    age >= 12,
    
    # Only eligible patients
    !is.na(elig_start),
  ) %>%
  mutate(eligibility_status = "Eligible") 

## Include treated patients not flagged as eligible
data_processed_treated <- data_processed %>%
  filter(!(patient_id %in% unique(data_processed_eligible)),
         !is.na(treatment_date)) %>%
  mutate(high_risk_group_nhsd = ifelse(is.na(high_risk_group_nhsd), "Not deemed eligible", high_risk_group_nhsd),
         elig_start = as.Date(ifelse(is.na(elig_start), treatment_date, elig_start), origin = "1970-01-01")) %>%
  mutate(eligibility_status = "Treated") 

data_processed_combined <- rbind(data_processed_eligible, data_processed_treated)

## Exclude patients issued more than one treatment within two weeks
dup_ids <- data_processed_combined %>%
  select(patient_id, treatment_date, sotrovimab_covid_therapeutics, molnupiravir_covid_therapeutics, casirivimab_covid_therapeutics) %>%
  filter(!is.na(treatment_date)) %>%
  mutate(sotrovimab_covid_therapeutics = as.Date(sotrovimab_covid_therapeutics, origin="1970-01-01"),
         molnupiravir_covid_therapeutics = as.Date(molnupiravir_covid_therapeutics, origin="1970-01-01"),
         casirivimab_covid_therapeutics = as.Date(casirivimab_covid_therapeutics, origin="1970-01-01"),
         sot_mol_diff = as.numeric(sotrovimab_covid_therapeutics - molnupiravir_covid_therapeutics),
         sot_cas_diff = as.numeric(sotrovimab_covid_therapeutics - casirivimab_covid_therapeutics),
         mol_cas_diff = as.numeric(molnupiravir_covid_therapeutics - casirivimab_covid_therapeutics)) %>%
  melt(id.var = "patient_id", measure.vars = c("sot_mol_diff", "sot_cas_diff", "mol_cas_diff")) %>%
  filter(!is.na(value),
         value <= 14 | value >= -14) %>%
  group_by(patient_id) %>%
  arrange(patient_id) 

data_processed_clean <- data_processed_combined %>%
  mutate(!(patient_id %in% dup_ids$patient_id))

## Formatting variables
data_processed_clean <- data_processed_clean %>%
  mutate(
    
    # Age groups
    ageband = cut(
      age,
      breaks = c(12, 30, 40, 50, 60, 70, 80, Inf),
      labels = c("12-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
      right = FALSE),
    
    #IMD
    imd = as.character(imd),
    imd = ifelse(imd %in% c("1 most deprived", 2:4, "5 least deprived"), imd, "Unknown"),
    imd = fct_case_when(
      imd == "1 most deprived" ~ "1 most deprived",
      imd == 2 ~ "2",
      imd == 3 ~ "3",
      imd == 4 ~ "4",
      imd == "5 least deprived" ~ "5 least deprived",
      imd == "Unknown" ~ "Unknown",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    # Region
    region = as.character(region_nhs),
    region = fct_case_when(
      region == "London" ~ "London",
      region == "East of England" ~ "East of England",
      region == "East Midlands" ~ "East Midlands",
      region == "North East" ~ "North East",
      region == "North West" ~ "North West",
      region == "South East" ~ "South East",
      region == "South West" ~ "South West",
      region == "West Midlands" ~ "West Midlands",
      region == "Yorkshire and the Humber" ~ "Yorkshire and the Humber",
      #TRUE ~ "Unknown"
      TRUE ~ NA_character_),
    
    # High risk cohort
    high_risk_group_nhsd = as.character(high_risk_group_nhsd),
    high_risk_group_nhsd = fct_case_when(
      high_risk_group_nhsd == "Down's syndrome" ~ "Down's syndrome",
      high_risk_group_nhsd == "Sickle cell disease" ~ "Sickle cell disease",
      high_risk_group_nhsd == "Patients with a solid cancer" ~ "Solid cancer",
      high_risk_group_nhsd == "Patients with a haematological diseases and stem cell transplant recipients" ~ "Haematological diseases and stem cell transplant recipients",
      high_risk_group_nhsd == "Patients with renal disease" ~ "Renal disease",
      high_risk_group_nhsd == "Patients with liver disease" ~ "Liver disease",
      high_risk_group_nhsd == "Patients with immune-mediated inflammatory disorders (IMID)" ~ "Immune-mediated inflammatory disorders",
      high_risk_group_nhsd == "Primary immune deficiencies" ~ "Primary immune deficiencies",
      high_risk_group_nhsd == "HIV/AIDS" ~ "HIV or AIDS",
      high_risk_group_nhsd == "Solid organ transplant recipients" ~ "Solid organ transplant recipients",
      high_risk_group_nhsd == "Rare neurological conditions" ~ "Rare neurological conditions",
      high_risk_group_nhsd == "Not deemed eligible" ~ "Not deemed eligible",
      #TRUE ~ "Unknown"
      TRUE ~ NA_character_)

  )


# Numbers for text ----
study_start <- format(as.Date(min(data_processed_clean$elig_start),format="%Y-%m-%d"), format = "%d-%b-%Y")
study_end <- format(as.Date(max(data_processed_clean$elig_start),format="%Y-%m-%d"), format = "%d-%b-%Y")
eligible_patients <- format(plyr::round_any(data_processed_clean %>% nrow(), 10), big.mark = ",", scientific = FALSE)
eligible_treated_patients <- paste(format(plyr::round_any(data_processed_clean %>% filter(!is.na(treatment_date), eligibility_status == "Eligible") %>% nrow(), 10), big.mark = ",", scientific = FALSE), 
                          " (",
                          round(data_processed_clean %>% filter(!is.na(treatment_date), eligibility_status == "Eligible") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                          "%)", sep = "")
eligible_sotrovimab <- paste(format(plyr::round_any(data_processed_clean %>% filter(treatment_type == "Sotrovimab",  eligibility_status == "Eligible") %>% nrow(), 10), big.mark = ",", scientific = FALSE),
                    " (",
                    round(data_processed_clean %>% filter(treatment_type == "Sotrovimab", eligibility_status == "Eligible") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                    "%)", sep = "")
eligible_molnupiravir <- paste(format(plyr::round_any(data_processed_clean %>% filter(treatment_type == "Molnupiravir",  eligibility_status == "Eligible") %>% nrow(), 10), big.mark = ",", scientific = FALSE),
                      " (",
                      round(data_processed_clean %>% filter(treatment_type == "Molnupiravir", eligibility_status == "Eligible") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                      "%)", sep = "")
eligible_casirivimab <- paste(format(plyr::round_any(data_processed_clean %>% filter(treatment_type == "Casirivimab",  eligibility_status == "Eligible") %>% nrow(), 10), big.mark = ",", scientific = FALSE),
                      " (",
                      round(data_processed_clean %>% filter(treatment_type == "Casirivimab", eligibility_status == "Eligible") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                      "%)", sep = "")
noneligible_treated_patients <- paste(format(plyr::round_any(data_processed_clean %>% filter(!is.na(treatment_date), eligibility_status == "Treated") %>% nrow(), 10), big.mark = ",", scientific = FALSE), 
                                   " (",
                                   round(data_processed_clean %>% filter(!is.na(treatment_date), eligibility_status == "Treated") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                                   "%)", sep = "")
noneligible_sotrovimab <- paste(format(plyr::round_any(data_processed_clean %>% filter(treatment_type == "Sotrovimab",  eligibility_status == "Treated") %>% nrow(), 10), big.mark = ",", scientific = FALSE),
                             " (",
                             round(data_processed_clean %>% filter(treatment_type == "Sotrovimab", eligibility_status == "Treated") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                             "%)", sep = "")
noneligible_molnupiravir <- paste(format(plyr::round_any(data_processed_clean %>% filter(treatment_type == "Molnupiravir",  eligibility_status == "Treated") %>% nrow(), 10), big.mark = ",", scientific = FALSE),
                               " (",
                               round(data_processed_clean %>% filter(treatment_type == "Molnupiravir", eligibility_status == "Treated") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                               "%)", sep = "")
noneligible_casirivimab <- paste(format(plyr::round_any(data_processed_clean %>% filter(treatment_type == "Casirivimab",  eligibility_status == "Treated") %>% nrow(), 10), big.mark = ",", scientific = FALSE),
                              " (",
                              round(data_processed_clean %>% filter(treatment_type == "Casirivimab", eligibility_status == "Treated") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                              "%)", sep = "")


text <- data.frame(study_start, study_end, eligible_patients, eligible_treated_patients, eligible_sotrovimab, 
                   eligible_molnupiravir, eligible_casirivimab, noneligible_treated_patients, noneligible_treated_patients, 
                   noneligible_sotrovimab, noneligible_molnupiravir, noneligible_casirivimab)

write_csv(text, here::here("output", "reports", "coverage", "tables", "report_stats.csv"))


# Plots ----

## Eligibility coverage
plot_data_coverage <- data_processed_clean %>%
  mutate(patient_id = 1) %>%
  group_by(elig_start, treatment_date, treatment_type) %>%
  summarise(count = sum(patient_id, na.rm  = T)) %>%
  arrange(elig_start, treatment_date, treatment_type) %>%
  ungroup() %>%
  arrange(elig_start, treatment_date) %>%
  complete(elig_start = seq.Date(min(elig_start), max(elig_start), by="day")) %>%
  group_by(elig_start) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  ungroup() %>%
  mutate(count = ifelse(is.na(count), 0, count),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10)) %>%
  mutate(high_risk_group_nhsd = "All")

plot_data_coverage_groups <- data_processed_clean %>%
  mutate(patient_id = 1) %>%
  group_by(elig_start, treatment_date, treatment_type, high_risk_group_nhsd) %>%
  summarise(count = sum(patient_id, na.rm  = T)) %>%
  arrange(elig_start, treatment_date, treatment_type, high_risk_group_nhsd) %>%
  ungroup() %>%
  arrange(high_risk_group_nhsd, elig_start) %>%
  group_by(high_risk_group_nhsd) %>%
  complete(elig_start = seq.Date(min(elig_start), max(elig_start), by="day")) %>%
  group_by(high_risk_group_nhsd, elig_start) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  group_by(high_risk_group_nhsd) %>%
  mutate(count = ifelse(is.na(count), 0, count),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10))

plot_order <- rbind(plot_data_coverage, plot_data_coverage_groups) %>%
  group_by(high_risk_group_nhsd) %>%
  mutate(order = max(cum_count_redacted, na.rm = T)) %>%
  arrange(desc(order)) %>%
  filter(cum_count_redacted == order) %>%
  select(high_risk_group_nhsd, order) %>%
  distinct()

coverage_plot_data <- rbind(plot_data_coverage, plot_data_coverage_groups) %>%
  mutate(high_risk_group_nhsd = factor(high_risk_group_nhsd, levels = plot_order$high_risk_group_nhsd))

write_csv(coverage_plot_data, here::here("output", "reports", "coverage", "figures", "cum_eligiblity_plot.csv"))

coverage_plot <- coverage_plot_data %>%
  ggplot(aes(x = elig_start, y = cum_count_redacted, colour = high_risk_group_nhsd, group = high_risk_group_nhsd)) +
  geom_step(size = 1) +
  theme_classic(base_size = 8) +
  scale_x_date(date_breaks = "1 week", date_labels =  "%d %b %Y") +
  labs(
    x = "Date",
    y = "Number of patients eligible for receiving treatment",
    colour = "High risk cohort",
    title = "") +
  theme(
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 10),
    legend.position = c(0.2,0.75),
    legend.background = element_rect(colour = "white"),
    legend.box.background = element_rect(colour = "black"),
    axis.line.x = element_line(colour = "black"),
    panel.grid.minor.x = element_blank(),
    legend.box.margin = margin(t = 1, l = 1, b = 1, r = 1))

ggsave(
  here::here("output", "reports", "coverage", "figures", "cum_eligiblity_plot.png"),
  coverage_plot,
  units = "cm", width = 35, height = 20
)

## Treatment coverage
plot_data_treatment <- data_processed_clean %>%
  mutate(patient_id = 1) %>%
  group_by(elig_start, treatment_date, treatment_type) %>%
  summarise(count = sum(patient_id, na.rm  = T)) %>%
  arrange(elig_start, treatment_date, treatment_type) %>%
  ungroup() %>%
  filter(!is.na(treatment_date)) %>%
  arrange(treatment_date) %>%
  complete(treatment_date = seq.Date(min(treatment_date), max(treatment_date), by="day")) %>%
  group_by(treatment_date) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  ungroup() %>%
  mutate(count = ifelse(is.na(count), 0, count),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10)) %>%
  mutate(high_risk_group_nhsd = "All")

plot_data_treatment_groups <- data_processed_clean %>%
  mutate(patient_id = 1) %>%
  group_by(elig_start, treatment_date, treatment_type, high_risk_group_nhsd) %>%
  summarise(count = sum(patient_id, na.rm  = T)) %>%
  arrange(elig_start, treatment_date, treatment_type, high_risk_group_nhsd) %>%
  ungroup() %>%
  filter(!is.na(treatment_date)) %>%
  arrange(high_risk_group_nhsd, treatment_date) %>%
  group_by(high_risk_group_nhsd) %>%
  complete(treatment_date = seq.Date(min(treatment_date), max(treatment_date), by="day")) %>%
  group_by(high_risk_group_nhsd, treatment_date) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  group_by(high_risk_group_nhsd) %>%
  mutate(count = ifelse(is.na(count), 0, count),
         cum_count = cumsum(count),
         cum_count_redacted =  plyr::round_any(cum_count, 10))

# Plot
plot_order <- rbind(plot_data_treatment, plot_data_treatment_groups) %>%
  group_by(high_risk_group_nhsd) %>%
  mutate(order = max(cum_count_redacted, na.rm = T)) %>%
  arrange(desc(order)) %>%
  filter(cum_count_redacted == order) %>%
  select(high_risk_group_nhsd, order) %>%
  distinct()

treatment_plot_data <- rbind(plot_data_treatment, plot_data_treatment_groups) %>%
  mutate(high_risk_group_nhsd = factor(high_risk_group_nhsd, levels = plot_order$high_risk_group_nhsd)) 

write_csv(treatment_plot_data, here::here("output", "reports", "coverage", "figures", "cum_treatment_plot.csv"))

treatment_plot <- treatment_plot_data %>%
  ggplot(aes(x = treatment_date, y = cum_count_redacted, colour = high_risk_group_nhsd, group = high_risk_group_nhsd)) +
  geom_step(size = 1) +
  theme_classic(base_size = 8) +
  scale_x_date(date_breaks = "1 week", date_labels =  "%d %b %Y") +
  labs(
    x = "Date",
    y = "Number of eligible patient receiving treatment",
    colour = "High risk cohort",
    title = "") +
  theme(
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 10),
    legend.position = c(0.2,0.75),
    legend.background = element_rect(colour = "white"),
    legend.box.background = element_rect(colour = "black"),
    axis.line.x = element_line(colour = "black"),
    panel.grid.minor.x = element_blank(),
    legend.box.margin = margin(t = 1, l = 1, b = 1, r = 1))

ggsave(
  here::here("output", "reports", "coverage", "figures", "cum_treatment_plot.png"),
  treatment_plot,
  units = "cm", width = 35, height = 20
)


# Tables ---

## Treatment table
eligibility_table <- data_processed_clean %>%
  select(high_risk_group_nhsd) %>%
  tbl_summary()

eligibility_table$inputs$data <- NULL

eligibility_table <- eligibility_table$table_body %>%
  separate(stat_0, c("stat_0","perc0"), sep = " ([(])") %>%
  select(`High risk cohort` = label, 
         `Number of eligible patients` = stat_0) %>%
  mutate(`Number of eligible patients` = as.numeric(gsub(",", "", `Number of eligible patients`))) %>%
  data.frame()

treatment_table <- data_processed_clean %>%
  select(high_risk_group_nhsd, treatment_type) %>%
  tbl_summary(by = treatment_type) %>%
  add_overall()

treatment_table$inputs$data <- NULL

treatment_table <- treatment_table$table_body %>%
  separate(stat_0, c("stat_0","perc0"), sep = " ([(])") %>%
  separate(stat_1, c("stat_1","perc1"), sep = " ([(])") %>%
  separate(stat_2, c("stat_2","perc2"), sep = " ([(])") %>%
  separate(stat_3, c("stat_3","perc3"), sep = " ([(])") %>%
  select(`High risk cohort` = label, 
         `Number of treated patients` = stat_0,
         `Treated with Casirivimab` = stat_1,
         `Treated with Molnupiravir` = stat_2,
         `Treated with Sotrovimab` = stat_3) %>%
  mutate(`Number of treated patients` = as.numeric(gsub(",", "", `Number of treated patients`)),
         `Treated with Casirivimab` = as.numeric(gsub(",", "", `Treated with Casirivimab`)),
         `Treated with Molnupiravir` = as.numeric(gsub(",", "", `Treated with Molnupiravir`)),
         `Treated with Sotrovimab` = as.numeric(gsub(",", "", `Treated with Sotrovimab`))) %>%
  data.frame()

table_elig_treat_redacted <- left_join(eligibility_table, treatment_table, by = "High.risk.cohort") %>%
  mutate(High.risk.cohort = factor(High.risk.cohort, 
                                     levels = c( "All", "Down's syndrome", "Sickle cell disease", "Solid cancer", 
                                                 "Haematological diseases and stem cell transplant recipients",
                                                 "Renal disease", "Liver disease",
                                                 "Immune-mediated inflammatory disorders",
                                                 "Primary immune deficiencies", "HIV or AIDS",
                                                 "Solid organ transplant recipients", "Rare neurological conditions", "Not deemed eligible"))) %>%
  # Redact values < 8
  mutate(Number.of.eligible.patients = ifelse(Number.of.eligible.patients < threshold, NA, as.numeric(Number.of.eligible.patients)),
         Number.of.treated.patients = ifelse(Number.of.treated.patients < threshold, NA, as.numeric(Number.of.treated.patients)),
         Treated.with.Casirivimab = ifelse(Treated.with.Casirivimab < threshold, NA, as.numeric(Treated.with.Casirivimab)),
         Treated.with.Molnupiravir = ifelse(Treated.with.Molnupiravir < threshold, NA, as.numeric(Treated.with.Molnupiravir)),
         Treated.with.Sotrovimab = ifelse(Treated.with.Sotrovimab < threshold, NA, as.numeric(Treated.with.Sotrovimab))
         ) %>%
  # Round to nearest 10
  mutate(Number.of.eligible.patients = plyr::round_any(Number.of.eligible.patients, 10),
         Number.of.treated.patients = plyr::round_any(Number.of.treated.patients, 10),
         Treated.with.Casirivimab = plyr::round_any(Treated.with.Casirivimab, 10),
         Treated.with.Molnupiravir = plyr::round_any(Treated.with.Molnupiravir, 10),
         Treated.with.Sotrovimab = plyr::round_any(Treated.with.Sotrovimab, 10))

write_csv(table_elig_treat_redacted, here::here("output", "reports", "coverage", "tables", "table_elig_treat_redacted.csv"))

## Clinical and demographics table
variables <- c("ageband", "sex", "ethnicity", "imd", "region")

table_demo_clinc_breakdown_base <- data_processed_clean %>%
  select(all_of(variables)) %>%
  tbl_summary()

table_demo_clinc_breakdown_base$inputs$data <- NULL

table_demo_clinc_breakdown_base <- table_demo_clinc_breakdown_base$table_body %>%
  separate(stat_0, c("stat_0","perc0"), sep = " ([(])") %>%
  select(Variable = label, 
         All = stat_0) %>%
  mutate(All = as.numeric(gsub(",", "", All))) %>%
  data.frame()

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
  select(Variable = label, 
         Treated = stat_0,
         Casirivimab = stat_1,
         Molnupiravir = stat_2,
         Sotrovimab = stat_3) %>%
  mutate(Treated = as.numeric(gsub(",", "", Treated)),
         Casirivimab = as.numeric(gsub(",", "", Casirivimab)),
         Molnupiravir = as.numeric(gsub(",", "", Molnupiravir)),
         Sotrovimab = as.numeric(gsub(",", "", Sotrovimab))) %>%

  data.frame()

table_demo_clinc_breakdown_redacted <- left_join(table_demo_clinc_breakdown_base, table_demo_clinc_breakdown, by = "Variable") %>%
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
         Casirivimab = plyr::round_any(Casirivimab, 10),
         Molnupiravir = plyr::round_any(Molnupiravir, 10),
         Sotrovimab = plyr::round_any(Sotrovimab, 10))


write_csv(table_demo_clinc_breakdown_redacted, here::here("output", "reports", "coverage", "tables", "table_demo_clinc_breakdown_redacted.csv"))





