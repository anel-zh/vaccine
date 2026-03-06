# ==============================================================================
# PROJECT: Vaccination Prediction
# FILE: processing_utils.R
# ROLE: Data transformation and summary functions (e.g., LOCF logic)
# AUTHOR: Anel Zhunussova and Poornima Kumar 
# DATE CREATED: 09 February 2026 
# LAST UPDATED: 09 February 2026 by AZ
# ==============================================================================

# HELPER: Rename scales for LOCF plotting
pretty_scale_name <- function(x) {
  case_when(
    str_detect(x, "promis_") ~ str_to_title(str_replace(x, "promis_", "PROMIS: ")),
    str_detect(x, "coping_") ~ str_to_title(str_replace(x, "coping_", "Coping: ")),
    str_detect(x, "apathy_") ~ str_to_title(str_replace(x, "apathy_", "Apathy: ")),
    x == "pandemic_stress" ~ "Pandemic Stress",
    TRUE ~ str_to_title(str_replace_all(x, "_", " "))
  )
}

# 1. LOCF SUMMARY GENERATOR (Script 02) ----------------------------------------
# Transforms the raw lag_table into summary data frames for QC plotting
prep_imputation_summaries <- function(lag_table) {
  
  total_N <- n_distinct(lag_table$record_id)
  all_scales <- lag_table %>%
    distinct(variable) %>%
    mutate(pretty_name = pretty_scale_name(variable)) %>%
    pull(pretty_name)
  
  # Calculate participant-level stats
  part_stats <- lag_table %>%
    group_by(record_id) %>%
    summarise(n_imputed = sum(lag_steps > 0), .groups = "drop") %>%
    mutate(status = ifelse(n_imputed == 0, "No LOCF applied", "LOCF applied"))
  
  # Incidence plot data
  incidence_plot_data <- part_stats %>%
    count(status) %>%
    mutate(pct = n / sum(n)) %>%
    mutate(status = factor(status, levels = c("No LOCF applied", "LOCF applied")))
  
  # Imputation burden data
  imputed_only_plot_data <- part_stats %>% filter(n_imputed > 0)
  
  # Lag categories (2 weeks = 1 step, etc.)
  lag_levels <- c("2 weeks", "1 month", ">1 month")
  
  locf_plot_data <- lag_table %>%
    mutate(pretty_name = pretty_scale_name(variable)) %>%
    filter(lag_steps > 0) %>%
    mutate(lag_category = case_when(
      lag_steps == 1 ~ lag_levels[1],
      lag_steps == 2 ~ lag_levels[2],
      lag_steps >= 3 ~ lag_levels[3]
    )) %>%
    mutate(lag_category = factor(lag_category, levels = lag_levels)) %>%
    count(pretty_name, lag_category, name = "count") %>%
    mutate(percent_of_total = count / total_N) %>%
    complete(pretty_name = all_scales,
             lag_category = factor(lag_levels, levels = lag_levels),
             fill = list(count = 0, percent_of_total = 0))
  
  # Sort order for plot
  sort_order <- locf_plot_data %>%
    group_by(pretty_name) %>%
    summarise(total_imp = sum(percent_of_total), .groups = "drop") %>%
    arrange(total_imp) %>%
    pull(pretty_name)
  
  return(list(
    incidence_plot_data = incidence_plot_data,
    imputed_only_plot_data = imputed_only_plot_data,
    locf_plot_data = locf_plot_data,
    sort_order = sort_order
  ))
}

# 2. COHEN'S D CALCULATION (Script 03) -----------------------------------------
# Calculating Cohen's D for all predictors against the outcome
calc_all_cohens_d <- function(data, vars_to_test, outcome_col) {
  data %>%
    select(all_of(outcome_col), all_of(vars_to_test)) %>%
    mutate(across(where(is.factor) & !all_of(outcome_col), as.numeric)) %>%
    pivot_longer(cols = -all_of(outcome_col), names_to = "Variable", values_to = "Score") %>%
    group_by(Variable) %>%
    summarise(
      m1 = mean(Score[!!sym(outcome_col) == "Intention (Yes)"], na.rm = TRUE),
      s1 = sd(Score[!!sym(outcome_col) == "Intention (Yes)"], na.rm = TRUE),
      n1 = sum(!!sym(outcome_col) == "Intention (Yes)", na.rm = TRUE),
      m2 = mean(Score[!!sym(outcome_col) == "Hesitancy (No)"], na.rm = TRUE),
      s2 = sd(Score[!!sym(outcome_col) == "Hesitancy (No)"], na.rm = TRUE),
      n2 = sum(!!sym(outcome_col) == "Hesitancy (No)", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      s_pooled = sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2)),
      cohen_d = (m2 - m1) / s_pooled, 
      se_d = sqrt((n1 + n2) / (n1 * n2) + cohen_d^2 / (2 * (n1 + n2))),
      lower = cohen_d - 1.96 * se_d,
      upper = cohen_d + 1.96 * se_d
    ) %>%
    filter(!is.na(cohen_d))
}
message(">>> Processing utilities loaded.")
