################################################################################
#
# Description: This script contains custom functions for:
#             - Factorising variables
#             - Calculating weekly counts of codes
#
# Author(s): M Green
# Date last updated: 05/01/2022
#
################################################################################


# Factorise ----
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# Calculate weekly counts ----
calculate_weekly_counts <- function(x) {
  
  # Read in data
  data_extract <- read_csv(
    here::here("output", "data", x))
  
  # Event counts
  event_counts <- data_extract %>%
    summarise(anti_infective_agent_events = sum(anti_infective_agent, na.rm = T),
              sotrovimab_events = sum(sotrovimab, na.rm = T),
              ronapreve_events = sum(ronapreve, na.rm = T),
              molnupiravir_events = sum(molnupiravir, na.rm = T),
              antiviral_therapy_events = sum(antiviral_therapy, na.rm = T),
              population = n())
  
  # Patient counts
  patient_counts <- data_extract %>%
    mutate(anti_infective_agent = ifelse(anti_infective_agent > 0, 1, 0),
           antiviral_therapy = ifelse(antiviral_therapy > 0, 1, 0),
           sotrovimab = ifelse(sotrovimab > 0, 1, 0),
           ronapreve = ifelse(ronapreve > 0, 1, 0),
           molnupiravir = ifelse(molnupiravir > 0, 1, 0)) %>%
    summarise(anti_infective_agent_count = sum(anti_infective_agent, na.rm = T),
              antiviral_therapy_count = sum(antiviral_therapy, na.rm = T),
              sotrovimab_count = sum(sotrovimab, na.rm = T),
              ronapreve_count = sum(ronapreve, na.rm = T),
              molnupiravir_count = sum(molnupiravir, na.rm = T)) %>%
    cbind(data_extract %>%
      select(sotrovimab_code, ronapreve_code, molnupiravir_code) %>%
      gather() %>%
      filter(!is.na(value)) %>%
      mutate(code = paste(key, value, sep = "_")) %>%
      count(code) %>%
      pivot_wider(names_from = code, values_from = n))
  
  # Combine
  weekly_counts <- cbind(event_counts, patient_counts) %>%
    mutate(date = as.Date(substr(x, 7, 16), format = "%Y-%m-%d"))

}


