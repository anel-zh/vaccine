# ==============================================================================
# PROJECT: Vaccination Prediction
# FILE: visualization_utils.R
# ROLE: All plotting themes, colors, and specialized visualization functions
# AUTHOR: Anel Zhunussova and Poornima Kumar 
# DATE CREATED: 09 February 2026 
# LAST UPDATED: 09 February 2026 by AZ
# ==============================================================================

# 1. GLOBAL STYLES & OVERALL HELPERS -----------------------------------------------------

style_cols <- list(
  intention = "#708090",  # Slate Grey
  hesitancy = "#bb2849",  # Crimson
  train     = "lightblue",
  test      = "darkblue",
  
  # LOCF Specific Colors
  locf_none    = "#aaaaaa",
  locf_applied = "#2F79B5",
  lag_steps    = c("2 weeks" = "#DBEAF2", "1 month" = "#86BDDA", ">1 month" = "#4E9AC6")
)

# Standard theme used across all project figures
fig_theme <- function(base_size = 12, rotate_x = TRUE, aspect = 1) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text  = element_text(size = 14, color = "black"),
      axis.line  = element_line(color = "black", linewidth = 0.4),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(2.2, "mm"),
      axis.title.x = element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = element_text(margin = ggplot2::margin(r = 8)),
      plot.margin  = ggplot2::margin(8, 10, 8, 10),
      aspect.ratio = aspect
    ) +
    (if (rotate_x) theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,
                                                    margin = ggplot2::margin(t = 4))) else theme())
}

# Helper to plot a grid of variables based on type
plot_var_grid <- function(data, vars, ncol = 3, title = "Grid") {
  plots <- map(vars, ~{
    # Check if variable is in levels_map (categorical) or not (numeric)
    if (.x %in% names(levels_map)) {
      plot_uni_bar(data, .x, make_pretty(.x))
    } else if (.x == "age") {
      plot_uni_hist_no_perc(data, .x, make_pretty(.x), x_lab = "Years", binwidth = 5)
    } else {
      plot_uni_hist(data, .x, make_pretty(.x))
    }
  })
  
  wrap_plots(plots, ncol = ncol, guides = "collect") +
    plot_annotation(title = sprintf("%s (n = %d)", title, nrow(data)),
                    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))) &
    theme(legend.position = "bottom", legend.title = element_blank())
}
# Helper to plot ANY list of plots in a pretty grid (Generic version of your EDA helper)
plot_custom_grid <- function(plot_list, ncol = 3, title = "Grid") {
  # This version takes a LIST OF PLOTS instead of a list of variable names
  patchwork::wrap_plots(plot_list, ncol = ncol, guides = "collect") +
    patchwork::plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    ) &
    theme(legend.position = "bottom", legend.title = element_blank())
}
# Export helper
# # Saves a list of plots into a multi-page PDF report
# save_report_pdf <- function(plot_list,full_path, width = 8, height = 6) {
#   # We construct the full path using the directory defined in config
# 
#   pdf(full_path, width = width, height = height)
#   # We use walk from purrr to print each plot to a new page
#   purrr::walk(plot_list, print)
#   dev.off()
# 
#   message(">>> Report saved: ", full_path)
# }


save_report_pdf <- function(plot_list, full_path, width = 16, height = 11) {
  if (length(plot_list) == 0) return(message(">>> Error: Plot list is empty."))
  
  pdf(full_path, width = width, height = height)
  purrr::walk(plot_list, ~{
    try(print(.x))
  })
  
  dev.off()
  message(">>> Report saved successfully: ", full_path)
}
# 2. DATA SELECTION QC (Script 02) ---------------------------------------------

# Timeline of response collection with percentage labels
plot_timeline <- function(data) {
  total_n <- nrow(data)
  data %>%
    mutate(month_floor = floor_date(as_date(date_complete), "month")) %>%
    ggplot(aes(x = month_floor)) +
    geom_bar(fill = style_cols$intention, alpha = 0.5, color = "black", linewidth = 0.5) +
    stat_count(geom = "text", 
               aes(label = scales::percent(after_stat(count) / total_n, accuracy = 0.1)),
               vjust = -0.5, size = 3, fontface = "bold") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = sprintf("Data Collection Period (n = %d)", nrow(data)), x = "Month of Response", y = "N Participants") +
    fig_theme()
}

# Checking temporal bias in vaccination intent
plot_outcome_temporal_bias <- function(data) {
  summary_df <- data %>%
    mutate(month = floor_date(as_date(date_complete), "month")) %>%
    group_by(month) %>%
    summarise(n_total = n(), prop_yes = mean(take_vaccination == 0), .groups = "drop") %>%
    mutate(ci = binom::binom.confint(round(prop_yes * n_total), n_total, methods = "wilson"))
  
  ggplot(summary_df, aes(x = month, y = prop_yes)) +
    geom_ribbon(aes(ymin = ci$lower, ymax = ci$upper), fill = style_cols$intention, alpha = 0.2) +
    geom_line(linewidth = 0.8, color = style_cols$intention) +
    geom_point(size = 2.5, color = style_cols$intention) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.5, 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(title = sprintf("Temporal Stability of Vaccination Intent (n = %d)", nrow(data)), x = "Date", y = "% Intenders (Yes)") +
    fig_theme()
}

# Incidence of LOCF imputation at participant level
plot_imputation_incidence <- function(incidence_data) {
  ggplot(incidence_data, aes(x = status, y = n, fill = status)) +
    geom_col(width = 0.65, color = "white", alpha = 0.5) +
    geom_text(aes(label = sprintf("%d (%.1f%%)", n, 100 * pct)), vjust = -0.4, fontface = "bold") +
    scale_fill_manual(values = c("No LOCF applied" = style_cols$locf_none, 
                                 "LOCF applied" = style_cols$locf_applied)) +
    labs(title = "Incidence of LOCF Imputation", x = NULL, y = "N Participants") +
    fig_theme(rotate_x = FALSE)
}

# Distribution of how many predictors were imputed per participant
plot_imputation_burden <- function(imputed_only_data) {
  ggplot(imputed_only_data, aes(x = n_imputed)) +
    geom_histogram(
      binwidth = 1, boundary = -0.5, closed = "left",
      fill = style_cols$locf_applied,
      color = "white", alpha = 0.5, linewidth = 0.5
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                       expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.06)),
                       breaks = scales::pretty_breaks(n = 5)) +
    labs(
      title = "Distribution of LOCF imputation burden",
      x = "Number of LOCF-imputed predictors",
      y = "Number of participants"
    ) +
    fig_theme(rotate_x = FALSE, aspect = 1)
}

# Extent and temporal lag of LOCF observations
plot_lag_by_scale <- function(locf_plot_data, sort_order) {
  ggplot(locf_plot_data, aes(x = percent_of_total, y = factor(pretty_name, levels = sort_order), fill = lag_category)) +
    geom_col(width = 1, color = "black", linewidth = 0.25, alpha = 0.5) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = style_cols$lag_steps) +
    labs(title = "Temporal Lag of Imputed Observations", x = "% Observations Imputed", y = NULL, fill = NULL) +
    fig_theme(rotate_x = FALSE) + theme(legend.position = "top")
}

# 3. EDA & DISTRIBUTIONS (Script 03) -------------------------------------------

# Stacked histogram for numeric variables
plot_uni_hist <- function(data, var, title, x_lab = "Score", binwidth = 1, show_y = FALSE) {
  total_n <- data %>% filter(!is.na(.data[[var]])) %>% nrow()
  data %>%
    filter(!is.na(.data[[var]])) %>%
    ggplot(aes(x = .data[[var]])) +
    geom_histogram(aes(fill = take_vaccination_lbl), position = "stack", binwidth = binwidth, color = "black", alpha = 0.7, linewidth = 0.25) +
    stat_bin(binwidth = binwidth, geom = "text", aes(label = scales::percent(after_stat(count) / total_n, accuracy = 0.1)), vjust = -0.5, size = 2.5, fontface = "bold") +
    scale_fill_manual(values = c(style_cols$intention, style_cols$hesitancy)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = x_lab, y = if(show_y) "Count" else NULL) +
    fig_theme(rotate_x = FALSE) + theme(legend.position = "none")
}

# Stacked histogram without percentage labels (e.g., for Age)
plot_uni_hist_no_perc <- function(data, var, title, x_lab = "Score", binwidth = 1, show_y = FALSE) {
  data %>%
    filter(!is.na(.data[[var]])) %>%
    ggplot(aes(x = .data[[var]], fill = take_vaccination_lbl)) +
    geom_histogram(position = "stack", binwidth = binwidth, color = "black", alpha = 0.7, linewidth = 0.25) +
    scale_fill_manual(values = c(style_cols$intention, style_cols$hesitancy)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, x = x_lab, y = if(show_y) "Count" else NULL) +
    fig_theme(rotate_x = FALSE) + theme(legend.position = "none")
}

# Stacked bar chart for categorical variables
plot_uni_bar <- function(data, var, title, show_y = FALSE) {
  total_n <- data %>% filter(!is.na(.data[[var]])) %>% nrow()
  data %>%
    filter(!is.na(.data[[var]])) %>%
    ggplot(aes(x = .data[[var]])) +
    geom_bar(aes(fill = take_vaccination_lbl), position = "stack", color = "black", alpha = 0.7, width = 0.5, linewidth = 0.25) +
    stat_count(geom = "text", aes(label = scales::percent(after_stat(count) / total_n, accuracy = 0.1)), vjust = -0.5, size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c(style_cols$intention, style_cols$hesitancy)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
    scale_x_discrete(drop = FALSE, labels = function(x) str_wrap(x, width = 15)) +
    labs(title = title, x = NULL, y = if(show_y) "Count" else NULL) +
    fig_theme(rotate_x = TRUE) + theme(legend.position = "none")
}



# PSYCHOLOGICAL PROFILES 
# Raincloud plot for group comparisons with Bayesian/Wilcoxon stats
plot_rainclouds <- function(data, group_var, metric_vars, prefix_to_remove = "", 
                            fill_colors = c(style_cols$intention, style_cols$hesitancy),
                            title = "Psychological Profiles", y_label = "Score",
                            test_type = "bayesian") {
  
  group_sym <- sym(group_var)
  
  # Preparing long format data
  long_data <- data %>%
    select(!!group_sym, all_of(metric_vars)) %>%
    pivot_longer(cols = all_of(metric_vars), names_to = "Scale", values_to = "Score") %>%
    mutate(
      Scale_Clean = gsub(prefix_to_remove, "", Scale),
      Scale_Clean = tools::toTitleCase(gsub("_", " ", Scale_Clean)),
      !!group_sym := as.factor(!!group_sym)
    ) %>%
    filter(!is.na(Score))
  
  # Calculating statistics for annotations
  stats_df <- long_data %>%
    group_by(Scale_Clean) %>%
    do({
      if (test_type == "bayesian") {
        bf_obj <- BayesFactor::ttestBF(formula = Score ~ !!group_sym, data = as.data.frame(.))
        bf_val <- BayesFactor::extractBF(bf_obj)$bf
        data.frame(stat_val = bf_val, y_max = max(.$Score, na.rm = TRUE))
      } else {
        p_val <- wilcox.test(Score ~ get(group_var), data = .)$p.value
        data.frame(stat_val = p_val, y_max = max(.$Score, na.rm = TRUE))
      }
    }) %>%
    mutate(
      display_label = if(test_type == "bayesian") {
        paste0("BF10 = ", ifelse(stat_val > 1000, formatC(stat_val, format = "e", digits = 1), round(stat_val, 2)))
      } else {
        paste0("p = ", round(stat_val, 3))
      },
      y_bracket = y_max * 1.25,
      y_label_pos = y_max * 1.35
    )
  
  # Creating plot
  ggplot(long_data, aes(x = !!group_sym, y = Score, fill = !!group_sym)) +
    geom_jitter(aes(color = !!group_sym), position = position_jitter(width = 0.15), size = 0.8, alpha = 0.4) +
    geom_violin(trim = FALSE, alpha = 0.3, color = NA, width = 0.8) +
    geom_boxplot(width = 0.15, color = "black", alpha = 0, outlier.shape = NA) +
    facet_wrap(~Scale_Clean, nrow = 1, strip.position = "bottom") + 
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = fill_colors) +
    labs(title = title, y = y_label, x = NULL) +
    fig_theme(rotate_x = FALSE) +
    theme(
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      strip.background = element_blank(), strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing = unit(0.5, "lines"), legend.position = "bottom"
    ) +
    geom_segment(data = stats_df, aes(x = 1, xend = 2, y = y_bracket, yend = y_bracket), inherit.aes = FALSE, color = "grey30") +
    geom_text(data = stats_df, aes(x = 1.5, y = y_label_pos, label = display_label), inherit.aes = FALSE, size = 3, fontface = "bold") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.35)))
}

# OVERALL EDA
# Ridge plot for scale distributions
make_ridge_plot <- function(data, vars, title) {
  plot_data <- data %>%
    select(all_of(vars), take_vaccination_lbl) %>%
    pivot_longer(cols = -take_vaccination_lbl, names_to = "Scale", values_to = "Score") %>%
    mutate(Scale = str_replace_all(Scale, "_", " ") %>% str_to_title())
  
  ggplot(plot_data, aes(x = Score, y = Scale, fill = take_vaccination_lbl)) +
    geom_density_ridges(scale = 1.5, rel_min_height = 0.01, alpha = 0.5, color = "white") +
    scale_fill_manual(values = c(style_cols$intention, style_cols$hesitancy)) +
    labs(title = title, x = NULL, y = NULL) +
    fig_theme(rotate_x = FALSE, aspect = NULL) +
    theme(legend.position = "none")
}


# Spearman correlation heatmap by category
plot_category_cor <- function(data, vars, title) {
  cor_data <- data %>%
    dplyr::select(starts_with("take_vaccination"), all_of(vars)) %>%
    mutate(across(everything(), as.numeric))
  
  colnames(cor_data) <- make_pretty(colnames(cor_data))
  
  cor_res <- Hmisc::rcorr(as.matrix(cor_data), type = "spearman")
  p <- patchwork::wrap_elements(full = ~ {
    corrplot::corrplot(cor_res$r, method = "color", type = "lower", order = "hclust", hclust.method = "ward.D2",
                       tl.col = "black", tl.cex = 0.8, p.mat = cor_res$P, sig.level = 0.05, insig = "blank",
                       diag = FALSE, col = colorRampPalette(c("darkblue","white","darkred"))(200),
                       title = paste0("Spearman: ", title), mar = c(0,0,1,0))})
  return(p)
}



# Cluster dendrogram to show redundant predictors
plot_dendrogram <- function(data, vars, title = "Cluster dendrogram of predictors") {
  cor_data <- data %>% 
    select(all_of(vars)) %>% 
    mutate(across(everything(), as.numeric))
  cor_mat <- cor(cor_data, method = "spearman", use = "complete.obs")
  dist_matrix <- as.dist(1 - abs(cor_mat))
  hc_fit <- hclust(dist_matrix, method = "ward.D2")
  main_title <- title
    p <- patchwork::wrap_elements(full = ~ {
    plot(hc_fit, hang = -1, main = main_title, xlab = "", sub = "", cex = 0.7)
    abline(h = 0.3, col = "red", lty = 2)})
  
  return(p)
}

# Cohen's D
plot_cohens_d_summary <- function(data, vars, group_var) {
  df_eff <- calc_all_cohens_d(data, vars, group_var) %>%
    mutate(
      Variable_Clean = make_pretty(Variable),
      Category = case_when(
        Variable %in% demo_vars    ~ "Sociodemographic",
        Variable %in% context_vars ~ "Pandemic context",
        TRUE                       ~ "Psychological"
      ),
      Direction = ifelse(cohen_d > 0, "Higher in Hesitant", "Higher in Intenders")
    ) %>%
    arrange(Category, cohen_d) %>%
    mutate(Variable_Clean = factor(Variable_Clean, levels = unique(Variable_Clean)))
  
  ggplot(df_eff, aes(x = cohen_d, y = Variable_Clean, color = Direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3) +
    geom_point(size = 2.5) +
    scale_color_manual(values = c("Higher in Intenders" = style_cols$intention, 
                                  "Higher in Hesitant" = style_cols$hesitancy)) +
    facet_grid(Category ~ ., scales = "free_y", space = "free_y") +
    labs(title = "Predictors of vaccination hesitancy (univariate)", 
         x = "Standardized effect size (Cohen's d)", y = NULL) +
    fig_theme(rotate_x = FALSE, aspect = NULL) +
    theme(legend.position = "bottom")
}

# 4. MODEL SPLIT QC (Script 04) ------------------------------------------------

# Compares Training vs Testing distributions for a specific variable
plot_split_compare <- function(data, var, type = "categorical") {
  split_cols <- c("Train" = style_cols$train, "Test" = style_cols$test)
  
  if (type == "continuous") {
    ggplot(data, aes(x = .data[[var]], fill = split, color = split)) +
      geom_density(alpha = 0.3, adjust = 1.1, linewidth = 0.6) +
      scale_fill_manual(values = split_cols) +
      scale_color_manual(values = split_cols) +
      labs(x = make_pretty(var), y = "Density") +
      fig_theme(rotate_x = FALSE) + theme(legend.position = "none")
    
  } else {
    # Categorical logic with percentages
    sum_df <- data %>%
      count(split, !!sym(var)) %>%
      group_by(split) %>%
      mutate(p = n / sum(n)) %>%
      ungroup()
    
    ggplot(sum_df, aes(x = !!sym(var), y = p, fill = split)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.5) +
      geom_text(aes(label = scales::percent(p, accuracy = 1)),
                position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5, fontface = "bold") +
      scale_fill_manual(values = split_cols) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                         expand = expansion(mult = c(0, 0.15))) +
      labs(x = make_pretty(var), y = "Proportion") +
      fig_theme(rotate_x = TRUE) + theme(legend.position = "none")
  }
}

# 5. MODEL SELECTION PLOTS -----------------------------------------------------

# Visualizing model stability across the 5 outer folds
plot_cv_stability <- function(cv_results_df) {
  plot_data <- cv_results_df %>%
    tidyr::pivot_longer(
      cols = any_of(c("AUC", "R2_McFadden", "R2_Tjur", "F1_Hesitancy", "F1_Intention", "Accuracy")), 
      names_to = "Metric", 
      values_to = "Value"
    ) %>%
    mutate(Metric = case_when(
      Metric == "AUC"           ~ "AUC",
      Metric == "R2_McFadden"   ~ "McFadden R2",
      Metric == "R2_Tjur"       ~ "Tjur R2",
      Metric == "F1_Hesitancy"  ~ "F1 (Hesitancy)",
      Metric == "F1_Intention"  ~ "F1 (Intention)",
      Metric == "Accuracy"      ~ "Accuracy",
      TRUE ~ Metric
    )) %>%
    mutate(Metric = factor(Metric, levels = c("AUC", "Tjur R2", "McFadden R2", 
                                              "F1 (Hesitancy)", "F1 (Intention)", "Accuracy")))
    
  ggplot(plot_data, aes(x = Model, y = Value, fill = Model)) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.6, size = 1.5) +
    facet_wrap(~Metric, scales = "free_y", ncol = 3, axes = "all", axis.labels = "all") +
    scale_fill_manual(values = c("EN" = "#8dd3c7", "RF" = "#bebada", "LOG" = "#ffffb3")) +
    labs(
      title = "Model performance stability across outer folds",
      x = NULL, 
      y = "Score Value"
    ) +
    fig_theme(rotate_x = FALSE) + 
    theme(
      legend.position = "none",
      strip.background = element_blank(),           
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing = unit(1, "lines")
    )
}

# 6. INTERPRETATION PLOTS ------------------------------------------------------

# Standardized beta coefficients
plot_model_coefs <- function(coef_df, title_suffix = "") {
  df_plot <- coef_df %>% mutate(Variable_Clean = make_pretty(Variable))
  
  ggplot(df_plot, aes(x = reorder(Variable_Clean, abs(Coefficient)), y = Coefficient, fill = Coefficient > 0)) +
    geom_col(width = 0.7) + coord_flip() +
    scale_fill_manual(values = c("TRUE" = style_cols$hesitancy, "FALSE" = style_cols$intention), 
                      labels = c("Increases Intention", "Increases Hesitancy"), name = "Direction") +
    geom_text(aes(label = sprintf("%.3f", Coefficient), hjust = ifelse(Coefficient > 0, -0.1, 1.1)), size = 3.5) +
    labs(title = paste("Top Predictors", title_suffix), x = NULL, y = "Standardized Beta") +
    fig_theme(rotate_x = FALSE) + theme(legend.position = "bottom")
}

# Standardized beta coefficients (bootstrapped)
plot_model_coefs_boot <- function(boot_df, title_suffix = "", boot_N = 10000) {
  # Filter for stable predictors only
  df_plot <- boot_df %>%
    filter(Stable == "Yes") %>%
    mutate(
      Variable_Clean = make_pretty(Variable),
      Direction = ifelse(Mean_Beta > 0, "Increases Hesitancy", "Increases Intention")
    )
  
  ggplot(df_plot, aes(x = reorder(Variable_Clean, Mean_Beta), y = Mean_Beta, color = Direction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.3, linewidth = 0.8) +
    geom_point(size = 3) +
    geom_text(aes(label = sprintf("%.3f", Mean_Beta)), 
              vjust = -1.2,          
              size = 3.5,            
              fontface = "bold",      
              show.legend = FALSE) +  
    coord_flip() +
    
    # Styling
    scale_color_manual(values = c("Increases Intention" = style_cols$intention, 
                                  "Increases Hesitancy" = style_cols$hesitancy)) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) + # Add room for labels
    labs(
      title = paste("Robust Predictors", title_suffix),
      subtitle = paste0("Mean standardized beta and 95% bootstrap CI (", boot_N, " iterations)"),
      x = NULL,
      y = "Standardized Beta"
    ) +
    fig_theme(rotate_x = FALSE) + 
    theme(legend.position = "bottom", legend.title = element_blank())
}


# EN interactions 
plot_en_interactions <- function(m2_coefs, raw_df, target_var, weights_vector) {
   interaction_terms <- m2_coefs %>% 
     filter(grepl("^int_", Variable))
   
  if (nrow(interaction_terms) == 0) return(NULL)
  
  plot_list <- list()
  for (i in 1:nrow(interaction_terms)) {
    int_name <- interaction_terms$Variable[i]
    vars <- strsplit(gsub("^int_", "", int_name), "_x_")[[1]]
    
    # Map back to raw factor names if necessary
    find_orig <- function(v) {
      for (nom in nominal_vars) { if (startsWith(v, nom)) return(nom) }
      return(v)
    }
    v1 <- find_orig(vars[1]); v2 <- find_orig(vars[2])
    
    fit_glm <- glm(as.formula(paste0(target_var, " ~ `", v1, "` * `", v2, "`")), 
                   data = raw_df, weights = weights_vector, family = quasibinomial(link = "logit"))
    
    plot_list[[int_name]] <- marginaleffects::plot_predictions(fit_glm, condition = c(v1, v2), type = "response") +
      labs(title = paste("Effect of", make_pretty(v1), ":", make_pretty(v2)),
           subtitle = paste("EN Beta:", round(interaction_terms$Coefficient[i], 3)),
           y = "Prob(Hesitancy)", x = make_pretty(v1), color = make_pretty(v2), fill = make_pretty(v2)) +
      fig_theme(rotate_x = FALSE)
  }
  return(plot_list)
}

# 7. SHAP & CLUSTERING PLOTS ------------------------------------------------------
# SHAP importance summary
plot_shap_summary <- function(shp_obj, top_vars, title_suffix = "") {
  colnames(shp_obj$X) <- make_pretty(colnames(shp_obj$X))
  colnames(shp_obj$S) <- make_pretty(colnames(shp_obj$S))
  clean_top_vars <- make_pretty(top_vars)
  
  # Beeswarm
  p_bee <- shapviz::sv_importance(shp_obj[, clean_top_vars], kind = "beeswarm", 
                                  max_display = length(top_vars), size = 1.8) + 
    fig_theme(aspect = NULL) + labs(title = paste("SHAP Directionality", title_suffix))
  
  # Bar with SE and Numeric Labels
  shap_stats <- as.data.frame(shp_obj$S) %>%
    dplyr::select(any_of(clean_top_vars)) %>%
    pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value") %>%
    mutate(Abs_Value = abs(Value)) %>%
    group_by(Feature) %>%
    summarise(mean_val = mean(Abs_Value), se_val = sd(Abs_Value) / sqrt(n()))
  
  p_bar <- ggplot(shap_stats, aes(x = reorder(Feature, mean_val), y = mean_val)) +
    geom_col(fill = "grey70", width = 0.7) +
    geom_errorbar(aes(ymin = mean_val - se_val, ymax = mean_val + se_val), width = 0.2) +
    geom_text(aes(label = sprintf("%.3f", mean_val)), hjust = -0.5, size = 3.5, fontface = "bold") +
    coord_flip() + labs(title = "SHAP Importance", x = NULL, y = "mean(|SHAP|)") +
    expand_limits(y = max(shap_stats$mean_val) * 1.3) + # Extra room for labels
    fig_theme(rotate_x = FALSE)
  
  return(p_bee / p_bar)
}

# Clustering Signature Plot
plot_phenotype_profiles <- function(clust_obj) {
  # Prepare Profile Data (Z-Scores)
  profile_data <- clust_obj$features %>%
    mutate(Phenotype = clust_obj$data$Phenotype) %>%
    group_by(Phenotype) %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(-Phenotype, names_to = "Variable", values_to = "Z_Score") %>%
    mutate(Variable_Clean = make_pretty(Variable))
  
  # Profile Plot
  p_profile <- ggplot(profile_data, aes(x = Variable_Clean, y = Z_Score, group = Phenotype, color = Phenotype)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_line(linewidth = 1.2) + geom_point(size = 3) +
    coord_flip() + scale_color_brewer(palette = "Set1") +
    labs(title = "Phenotype Signature", y = "Standard Deviations (Z-Score)", x = NULL) +
    fig_theme()
  
  # Outcome Plot
  outcome_stats <- clust_obj$data %>%
    group_by(Phenotype) %>%
    summarise(n = n(), n_h = sum(Outcome == "No"), pct = (n_h/n)*100) %>%
    rowwise() %>% 
    mutate(low = prop.test(n_h, n)$conf.int[1]*100, up = prop.test(n_h, n)$conf.int[2]*100)
  
  p_outcome <- ggplot(outcome_stats, aes(x = Phenotype, y = pct, fill = Phenotype)) +
    geom_col(alpha = 0.7, width = 0.6) +
    geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
    geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -2, fontface = "bold", size = 4) +
    scale_fill_brewer(palette = "Set1") +
    labs(y = "% Hesitant", x = "Phenotype") +
    ylim(0, 115) + # Increased limit to prevent text cutoff
    theme_classic() + fig_theme(rotate_x = FALSE)
  
  return(p_profile / p_outcome + plot_layout(heights = c(2, 1)))
}

plot_cluster_validation <- function(clust_obj, title_suffix = "") {
  
  # Significance test: Chi-Square for Outcome
  # Is the distribution of Hesitancy significantly different?
  contingency_table <- table(clust_obj$data$Phenotype, clust_obj$data$Outcome)
  chi_test <- chisq.test(contingency_table)
  p_val <- format.pval(chi_test$p.value, digits = 3)
  
  # t-SNE visualization 
  set.seed(123)
  # Remove duplicate rows before t-SNE if any exist
  unique_data <- !duplicated(clust_obj$features)
  tsne_out <- Rtsne(as.matrix(clust_obj$features[unique_data, ]), dims = 2, perplexity = 30)
  
  tsne_df <- data.frame(
    X = tsne_out$Y[,1],
    Y = tsne_out$Y[,2],
    Phenotype = clust_obj$data$Phenotype[unique_data]
  )
  
  p_cloud <- ggplot(tsne_df, aes(x = X, y = Y, color = Phenotype)) +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_brewer(palette = "Set1") +
    labs(title = paste("t-SNE Cluster Separation", title_suffix),
         subtitle = paste("Chi-Square p-value (Outcome) =", p_val),
         x = "t-SNE Dimension 1", y = "t-SNE Dimension 2") +
    theme_minimal() + fig_theme(aspect = 1)
  
  return(list(plot = p_cloud, stats = chi_test))
}
message(">>> Full visualization library (visualization_utils.R) loaded.")