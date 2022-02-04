################################################################################
#
# Description: This script produces metadata, figurs and tables to go into the
#              mabs_and_antivirvals_coverage_report.rmd 
#
# Input: /output/data/data_processed.rds
#
# Output: /reports/coverage/tables/report_stats.csv
#         /reports/coverage/tables/table_elig_treat_redacted.csv
#         /reports/coverage/tables/table_demo_clinc_breakdown_redacted.csv
#         /reports/coverage/figures/cum_treatment_plot.png
#         /reports/coverage/figures/cum_eligiblity_plot.png
#
# Author(s): M Green
# Date last updated: 02/02/2022
#
################################################################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('here')
library('glue')
library('gt')
library('gtsummary')
library('reshape2')

## Import custom user functions
source(here("analysis", "lib", "custom_functions.R"))

## Create output directory
fs::dir_create(here::here("reports", "coverage", "tables"))
fs::dir_create(here::here("reports", "coverage", "figures"))

## Import data
data_processed <- read_rds(here::here("output", "data", "data_processed.rds"))


# Format data ----

## Apply eligibility and exclusion criteria
data_processed_eligible <- data_processed %>%
  # Temporary fix to include treated patients not flagged as eligible due to high risk cohort 
  mutate(high_risk_group_nhsd = ifelse(is.na(high_risk_group_nhsd), "Non-digital", high_risk_group_nhsd),
         elig_start = as.Date(ifelse(is.na(elig_start) & high_risk_group_nhsd == "Non-digital", treatment_date, elig_start), origin = "1970-01-01")) %>%
  filter(
    # Apply eligibility criteria
    covid_test_positive == 1,
    covid_positive_previous_30_days != 1,
    (tb_postest_treat <= 10 & tb_postest_treat >= 0) | is.na(tb_postest_treat),
    !is.na(high_risk_group_nhsd),
    
    # Apply exclusion criteria
    is.na(covid_hospital_admission_date) | covid_hospital_admission_date < (elig_start - 30) & covid_hospital_admission_date >= (elig_start + 1),
    age >= 12,
    
    # Only eligible patients
    !is.na(elig_start),
  )

## Formatting variables
data_processed_eligible <- data_processed_eligible %>%
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
    high_risk_group_nhsd = factor(high_risk_group_nhsd, levels = c("Down's syndrome", "Sickle cell disease", "Patients with a solid cancer",
                                                                   "Patients with a haematological diseases and stem cell transplant recipients",
                                                                   "Patients with renal disease", "Patients with liver disease",
                                                                   "Patients with immune-mediated inflammatory disorders (IMID)",
                                                                   "Primary immune deficiencies", "HIV/AIDS", "Solid organ transplant recipients", 
                                                                   "Rare neurological conditions", "Non-digital"))
    
  )

## Exclude duplicated patients issued more than one treatment
dup_ids <- data_processed_eligible %>%
  select(patient_id, sotrovimab_covid_therapeutics, molnupiravir_covid_therapeutics, casirivimab_covid_therapeutics) %>%
  melt(id.var = "patient_id") %>%
  filter(!is.na(value)) %>%
  mutate(value = as.Date(value, origin="1970-01-01")) %>%
  group_by(patient_id) %>%
  summarise(count = n()) %>%
  filter(count > 1)

data_processed_clean <- data_processed_eligible %>%
  mutate(!(patient_id %in% dup_ids$patient_id))


# Numbers for text ----
study_start <- as.Date(min(data_processed_clean$elig_start),format="%Y-%m-%d")
study_end <- format(as.Date(max(data_processed_clean$elig_start),format="%Y-%m-%d"), format = "%d-%b-%Y")
eligible_patients <- format(data_processed_clean %>% nrow(), big.mark = ",", scientific = FALSE)
treated_patients <- paste(format(data_processed_clean %>% filter(!is.na(treatment_date)) %>% nrow(), big.mark = ",", scientific = FALSE), 
                          " (",
                          round(data_processed_clean %>% filter(!is.na(treatment_date)) %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                          "%)", sep = "")
sotrovimab <- paste(format(data_processed_clean %>% filter(treatment_type == "sotrovimab") %>% nrow(), big.mark = ",", scientific = FALSE),
                    " (",
                    round(data_processed_clean %>% filter(treatment_type == "sotrovimab") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                    "%)", sep = "")
molnupiravir <- paste(format(data_processed_clean %>% filter(treatment_type == "molnupiravir") %>% nrow(), big.mark = ",", scientific = FALSE),
                      " (",
                      round(data_processed_clean %>% filter(treatment_type == "molnupiravir") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                      "%)", sep = "")
casirivimab <- paste(format(data_processed_clean %>% filter(treatment_type == "casirivimab") %>% nrow(), big.mark = ",", scientific = FALSE),
                      " (",
                      round(data_processed_clean %>% filter(treatment_type == "casirivimab") %>% nrow()/nrow(data_processed_clean)*100, digits = 0),
                      "%)", sep = "")


text <- data.frame(study_start, study_end, eligible_patients, treated_patients, sotrovimab, molnupiravir, casirivimab)

write_csv(text, here::here("reports", "coverage", "tables", "report_stats.csv"))


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
         cum_count_redacted =  plyr::round_any(cum_count, 5)) %>%
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
         cum_count_redacted =  plyr::round_any(cum_count, 5))

coverage_plot <- rbind(plot_data_coverage, plot_data_coverage_groups) %>%
  mutate(high_risk_group_nhsd = factor(high_risk_group_nhsd, levels = c("All", "Down's syndrome", "Sickle cell disease", "Patients with a solid cancer",
                                                                        "Patients with a haematological diseases and stem cell transplant recipients",
                                                                        "Patients with renal disease", "Patients with liver disease",
                                                                        "Patients with immune-mediated inflammatory disorders (IMID)",
                                                                        "Primary immune deficiencies", "HIV/AIDS",
                                                                        "Solid organ transplant recipients", "Rare neurological conditions"))) %>%
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
  here::here("reports", "coverage", "figures", "cum_eligiblity_plot.png"),
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
         cum_count_redacted =  plyr::round_any(cum_count, 5)) %>%
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
         cum_count_redacted =  plyr::round_any(cum_count, 5))

# Plot
treatment_plot <- rbind(plot_data_treatment, plot_data_treatment_groups) %>%
  mutate(high_risk_group_nhsd = factor(high_risk_group_nhsd, levels = c("All", "Down's syndrome", "Sickle cell disease", "Patients with a solid cancer",
                                                                        "Patients with a haematological diseases and stem cell transplant recipients",
                                                                        "Patients with renal disease", "Patients with liver disease",
                                                                        "Patients with immune-mediated inflammatory disorders (IMID)",
                                                                        "Primary immune deficiencies", "HIV/AIDS",
                                                                        "Solid organ transplant recipients", "Rare neurological conditions"))) %>%
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
  here::here("reports", "coverage", "figures", "cum_treatment_plot.png"),
  treatment_plot,
  units = "cm", width = 35, height = 20
)


# Tables ---

## Treatment table
tbl1 <- cbind(data_processed_clean %>% summarise(Eligible = n()),
              data_processed_clean %>% filter(!is.na(treatment_date) & treatment_type == "sotrovimab") %>% summarise(sotrovimab = n()),
              data_processed_clean %>% filter(!is.na(treatment_date) & treatment_type == "molnupiravir") %>% summarise(molnupiravir = n()),
              data_processed_clean %>% filter(!is.na(treatment_date) & treatment_type == "casirivimab") %>% summarise(casirivimab = n()),
              data_processed_clean %>% filter(!is.na(treatment_date)) %>% summarise(any = n())) %>%
  mutate(`Received treatment` = paste(any, " (", round(any/Eligible*100, digits = 0), "%)", sep = ""),
         `Received sotrovimab` = paste(sotrovimab, " (", round(sotrovimab/any*100, digits = 0), "%)", sep = ""),
         `Received molnupiravir` = paste(molnupiravir, " (", round(molnupiravir/any*100, digits = 0), "%)", sep = ""),
         `Received casirivimab` = paste(casirivimab, " (", round(casirivimab/any*100, digits = 0), "%)", sep = ""),
         `High risk cohort` = "All") %>%
  select(`High risk cohort`, `Number of eligible patients, n` = Eligible, 
         `Eligible patients receiving treatment, n (%)` = `Received treatment`, 
         `Treated patients receiving sotrovimab, n (%)` = `Received sotrovimab`, 
         `Treated patients receiving molnupiravir, n (%)` = `Received molnupiravir`, 
         `Treated patients receiving casirivimab, n (%)` = `Received casirivimab`)

tbl2 <- left_join(data_processed_clean %>% group_by(high_risk_group_nhsd) %>% summarise(Eligible = n()),
                  data_processed_clean %>% group_by(high_risk_group_nhsd) %>% 
                    filter(!is.na(treatment_date) & treatment_type == "sotrovimab") %>% summarise(sotrovimab = n())) %>%
  left_join(data_processed_clean %>% group_by(high_risk_group_nhsd) %>% filter(!is.na(treatment_date) & treatment_type == "molnupiravir") %>% 
              summarise(molnupiravir = n())) %>%
  left_join(data_processed_clean %>% group_by(high_risk_group_nhsd) %>% filter(!is.na(treatment_date) & treatment_type == "casirivimab") %>% 
              summarise(casirivimab = n())) %>%
  left_join(data_processed_clean %>% group_by(high_risk_group_nhsd) %>% filter(!is.na(treatment_date)) %>% summarise(any = n())) %>%
  mutate(`Received treatment` = paste(any, " (", round(any/Eligible*100, digits = 0), "%)", sep = ""),
         `Received sotrovimab` = paste(sotrovimab, " (", round(sotrovimab/any*100, digits = 0), "%)", sep = ""),
         `Received molnupiravir` = paste(molnupiravir, " (", round(molnupiravir/any*100, digits = 0), "%)", sep = ""),
         `Received casirivimab` = paste(casirivimab, " (", round(casirivimab/any*100, digits = 0), "%)", sep = "")) %>%
  select(`High risk cohort` = high_risk_group_nhsd, `Number of eligible patients, n` = Eligible, 
         `Eligible patients receiving treatment, n (%)` = `Received treatment`, 
         `Treated patients receiving sotrovimab, n (%)` = `Received sotrovimab`, 
         `Treated patients receiving molnupiravir, n (%)` = `Received molnupiravir`, 
         `Treated patients receiving casirivimab, n (%)` = `Received casirivimab`) %>%
  ungroup()

table_elig_treat_redacted <- rbind(tbl1, tbl2) %>%
  mutate(`High risk cohort` = factor(`High risk cohort`, 
                                     levels = c( "All", "Down's syndrome", "Sickle cell disease", "Patients with a solid cancer", 
                                                 "Patients with a haematological diseases and stem cell transplant recipients",
                                                 "Patients with renal disease", "Patients with liver disease",
                                                 "Patients with immune-mediated inflammatory disorders (IMID)",
                                                 "Primary immune deficiencies", "HIV/AIDS",
                                                 "Solid organ transplant recipients", "Rare neurological conditions", "Non-digital"))) 

write_csv(table_elig_treat_redacted, here::here("reports", "coverage", "tables", "table_elig_treat_redacted.csv"))

## Clinical and demographics table
variables <- c("ageband", "sex", "ethnicity", "imd", "region")

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

gt::gtsave(as_gt(table_demo_clinc_breakdown), here::here("reports", "coverage", "tables", "table_demo_clinc_breakdown_redacted.html"))





