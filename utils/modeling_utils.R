# ==============================================================================
# PROJECT: Vaccination Prediction
# FILE: modeling_utils.R
# ROLE: Core functions for data prep, partitioning, and model training
# AUTHOR: Anel Zhunussova and Poornima Kumar 
# DATE CREATED: 09 February 2026 
# LAST UPDATED: 09 February 2026 by AZ
# ==============================================================================

# 1. DATA PREPARATION ----------------------------------------------------------

# Handles standard factor conversion and removes non-predictor metadata
prepare_model_data_base <- function(df, target_var, nominal_list) {
  df %>%
    # Remove columns that shouldn't be predictors (intermittent QC vars)
    dplyr::select(-any_of(c("record_id", "date_complete", "timepoint", "month", "date_was_recovered")), 
           -matches("_lbl$")) %>% 
    mutate(
      # Ensure target is a factor with specific labels
      !!sym(target_var) := factor(!!sym(target_var), levels = c(0, 1), labels = c("Yes", "No")),
      # Apply factor types to categorical predictors
      across(all_of(nominal_list), as.factor),
      # Ensure everything else is numeric
      across(-all_of(c(target_var, nominal_list)), as.numeric))
}

# 2. MODEL FITTING MECHANISMS ----------------------------------------------------------

# Scaling and Dummification (Inner Loop)
prepare_fold_data <- function(train_df, test_df, target_var = "take_vaccination") {
  y_train <- train_df[[target_var]]
  y_test  <- test_df[[target_var]]
  
  x_train_raw <- train_df %>% dplyr::select(-all_of(target_var))
  x_test_raw  <- test_df %>% dplyr::select(-all_of(target_var))
  # Creating dummies for predictors only 
  dummies_proc <- caret::dummyVars(~ ., data = x_train_raw, fullRank = TRUE)
  x_train <- predict(dummies_proc, newdata = x_train_raw)
  x_test  <- predict(dummies_proc, newdata = x_test_raw)
  # Scaling & Centering
  preproc_vals <- caret::preProcess(x_train, method = c("center", "scale"))
  return(list(
    x_train = predict(preproc_vals, x_train), y_train = y_train,
    x_test  = predict(preproc_vals, x_test),  y_test  = y_test,
    preproc = preproc_vals, dummies = dummies_proc
  ))
}

# --- FITTING HELPERS ---

# Elastic Net Helper
fit_en_helper <- function(x, y, wts) {
  lambda_seq <- 10^seq(5, -5, length = 500)
  alpha_seq  <- seq(0, 1, by = 0.1)
  
  # Pre-generate fold IDs for internal 5-fold CV so Alpha search stays independent of the RNG state
  set.seed(999) 
  fold_ids <- sample(rep(1:5, length.out = nrow(x)))
  
  best_mse <- Inf; best_params <- list()
  for (a in alpha_seq) {
    cv_fit <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = a, 
                                weights = wts, lambda = lambda_seq, 
                                nfolds = 5, foldid = fold_ids)
    
    if (min(cv_fit$cvm) < best_mse) {
      best_mse <- min(cv_fit$cvm)
      best_params <- list(alpha = a, lambda = cv_fit$lambda.min)
    }
  }
  model <- glmnet::glmnet(x, y, family = "binomial", alpha = best_params$alpha, lambda = best_params$lambda, weights = wts)
  return(list(model = model, params = best_params))
}

# Random Forest Helper 
# Custom RF allows caret to tune 'ntree' which is not standard in caret's default RF
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), 
                                  class = rep("numeric", 2), 
                                  label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {
  data.frame(mtry = floor(sqrt(ncol(x))), ntree = 1000)
}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest::randomForest(x, y, mtry = param$mtry, ntree = param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) predict(modelFit, newdata)
customRF$prob    <- function(modelFit, newdata, preProc = NULL, submodels = NULL) predict(modelFit, newdata, type = "prob")
customRF$sort    <- function(x) x[order(x[,1]),]
customRF$levels  <- function(x) x$classes

fit_rf_helper <- function(x, y) {
  tunegrid_rf <- expand.grid(.mtry = c(1:sqrt(ncol(x))), 
                             .ntree = seq(100, 1000, by = 100)) 
  
  ctrl <- caret::trainControl(method = "cv", number = 5, classProbs = TRUE, 
                              summaryFunction = caret::twoClassSummary, sampling = "down")
  
  caret::train(x = x, y = y, method = customRF, tuneGrid = tunegrid_rf, 
               trControl = ctrl, metric = "ROC")
}

# Logistic Regression Helper
fit_log_helper <- function(x, y, wts) {
  # Stitch x and y back into a dataframe
  tmp_df <- as.data.frame(x)
  tmp_df$target_y <- y
  
  # quasibinomial to prevent "non-integer success" warnings from weights
  stats::glm(target_y ~ ., data = tmp_df, family = "quasibinomial", weights = wts)
}


# Standardized Metric Extraction
calculate_metrics <- function(y_true, y_prob, model_obj = NULL) {
  y_pred <- factor(ifelse(y_prob > 0.5, "No", "Yes"), levels = c("Yes", "No"))
  
  # AUC
  roc_obj <- pROC::roc(y_true, y_prob, levels = c("Yes", "No"), direction = "<", quiet = TRUE)

  # McFadden's R2 (Deviance Explained)
  # Because this is pulled from model_obj$dev.ratio, it represents how well the model explained the Training data for that fold.
  r2_mcfadden <- NA
  if (!is.null(model_obj)) {
    if (inherits(model_obj, "glmnet")) {
      # For Elastic Net
      r2_mcfadden <- model_obj$dev.ratio 
    } else if (inherits(model_obj, "glm")) {
      # For Logistic Regression: 1 - (Residual Deviance / Null Deviance)
      r2_mcfadden <- 1 - (model_obj$deviance / model_obj$null.deviance)
    }
    # For Random Forest stays NA
  }
  
  # Tjur's R2 (Coefficient of Discrimination)
  # Compares avg prob of 'No' group vs 'Yes' group.
  # Because this is calculated using y_prob and y_true, it represents how well the model separated the groups in the Testing (outer fold) data.
  levs <- levels(y_true)
  r2_tjur <- abs(mean(y_prob[y_true == levs[2]]) - mean(y_prob[y_true == levs[1]]))
  
  data.frame(
    AUC      = as.numeric(pROC::auc(roc_obj)),
    R2_McFadden = r2_mcfadden,
    R2_Tjur  = r2_tjur,
    F1_Hesitancy = MLmetrics::F1_Score(y_true = y_true, y_pred = y_pred, positive = "No"),
    F1_Intention    = MLmetrics::F1_Score(y_true = y_true, y_pred = y_pred, positive = "Yes"),
    Accuracy = MLmetrics::Accuracy(y_true = y_true, y_pred = y_pred)
  )
}


# Bootstrap model
fit_boot_stability <- function(x, y, wts, alpha = 0, n_boot = 1000) {
  message(sprintf(">>> Starting Bootstrap Stability (%d iterations)...", n_boot))
  boot_coefs_list <- list()
  # Progress bar is helpful for 10,000 runs
  pb <- txtProgressBar(min = 0, max = n_boot, style = 3)
  for (b in 1:n_boot) {
    # Resample with replacement
    idx      <- sample(1:nrow(x), replace = TRUE)
    x_boot   <- x[idx, ]
    y_boot   <- y[idx]
    wts_boot <- wts[idx]
    # Re-tune Lambda ONLY (Alpha is fixed based on Nested CV result)
    # We use a simpler 5-fold CV inside the bootstrap to save time
    cv_boot <- glmnet::cv.glmnet(x_boot, y_boot, family = "binomial", 
                                 alpha = alpha, weights = wts_boot, nfolds = 5)
    # Store coefficients at lambda.min
    boot_coefs_list[[b]] <- as.matrix(coef(cv_boot, s = "lambda.min"))
    setTxtProgressBar(pb, b)
  }
  close(pb)
  # Aggregate Results
  boot_matrix <- do.call(cbind, boot_coefs_list)
  summary_df <- data.frame(
    Variable  = rownames(boot_matrix),
    Mean_Beta = rowMeans(boot_matrix),
    Lower_CI  = apply(boot_matrix, 1, quantile, probs = 0.025),
    Upper_CI  = apply(boot_matrix, 1, quantile, probs = 0.975),
    Selection_Freq = apply(boot_matrix, 1, function(row) mean(row != 0))
  ) %>%
    filter(Variable != "(Intercept)") %>%
    mutate(Stable = ifelse(Lower_CI * Upper_CI > 0, "Yes", "No")) %>%
    arrange(desc(abs(Mean_Beta)))
  
  return(summary_df)
}

# 3. INTERPRETATION & HIERARCHICAL MODELING ------------------------------------

# Extract predictors with coefficients above a threshold
get_top_predictors <- function(model, params, threshold = 0.05) {
  coefs <- coef(model, s = params$lambda) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rename(Coefficient = 1) %>% 
    rownames_to_column("Variable") %>%
    filter(Variable != "(Intercept)", abs(Coefficient) >= threshold) %>%
    arrange(desc(abs(Coefficient)))
  
  return(coefs)
}



# Systematic interaction discovery using glinternet
run_hierarchical_en <- function(x_train, y_train) {
  # glinternet needs 0/1 numeric outcome
  y_numeric <- ifelse(y_train == "No", 1, 0)
  
  # Since data is scaled/dummified, all are treated as type="continuous" (numLevels=1)
  num_levels <- rep(1, ncol(x_train))
  
  set.seed(123)
  # Hierarchical Lasso (glinternet)
  cv_fit <- glinternet::glinternet.cv(x_train, y_numeric, 
                                      numLevels = num_levels, 
                                      family = "binomial", 
                                      nFolds = 5)
  
  # Extract indices of interactions found at the best lambda
  cf <- coef(cv_fit)
  inter_idx <- cf$interactions$contcont
  
  # If no interactions found, return NULL
  if (is.null(inter_idx)) return(NULL)
  
  # Map indices back to variable names
  var_names <- colnames(x_train)
  inter_list <- data.frame(
    var1 = var_names[inter_idx[, 1]],
    var2 = var_names[inter_idx[, 2]],
    stringsAsFactors = FALSE
  )
  
  return(inter_list)
}



# Generate interaction terms based on discovery list
apply_top_interactions <- function(x_matrix, interaction_list) {
  new_df <- as.data.frame(x_matrix)
  if(!is.null(interaction_list) && nrow(interaction_list) > 0) {
    for(i in 1:nrow(interaction_list)) {
      v1 <- interaction_list$var1[i]; v2 <- interaction_list$var2[i]
      inter_name <- paste0("int_", v1, "_x_", v2)
      new_df[[inter_name]] <- x_matrix[, v1] * x_matrix[, v2]
    }
  }
  return(as.matrix(new_df))
}

# 4. SHAP & CLUSTERING ---------------------------------------------------------
# SHAP Calculation
calculate_shaps <- function(model_res, x_test) {
  p_fun <- function(obj, X) as.numeric(predict(obj, newx = as.matrix(X), s = model_res$params$lambda, type = "response"))
  shaps <- kernelshap::kernelshap(model_res$model, as.matrix(x_test), pred_fun = p_fun)
  return(shapviz::shapviz(shaps))
}

# Clustering Engine
run_phenotype_clustering <- function(x_data, y_data, top_vars, k = 3) {
  # Filter continuous vars only (exclude dummies)
  full_vars_df <- as.data.frame(x_data) %>% dplyr::select(any_of(top_vars))
  to_exclude <- grep("\\.1|\\.2", names(full_vars_df), value = TRUE)
  cluster_df <- full_vars_df[, !names(full_vars_df) %in% to_exclude]
  
  # Elbow/Silhouette 
  p_elbow <- factoextra::fviz_nbclust(cluster_df, kmeans, method = "wss") + theme_minimal()
  p_sil   <- factoextra::fviz_nbclust(cluster_df, kmeans, method = "silhouette") + theme_minimal()
  
  # Run K-means
  set.seed(123)
  km_res <- kmeans(cluster_df, centers = k, nstart = 25)
  
  # Bootstrapping (Stability)
  boot_res <- fpc::clusterboot(cluster_df, B = 1000, bootmethod = "boot",
                               clustermethod = fpc::kmeansCBI, krange = k, seed = 123, count = FALSE, nstart = 25)
  
  return(list(
    data = data.frame(Phenotype = as.factor(km_res$cluster), Outcome = y_data),
    features = cluster_df,
    validation_plots = p_elbow + p_sil,
    stability = boot_res$bootmean
  ))
}

message(">>> Modeling utilities (modeling_utils.R) loaded.")