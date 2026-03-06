# ==============================================================================
# PROJECT: Vaccination Prediction
# FILE: load_packages.R
# ROLE: Automated package installation and loading
# AUTHOR: Anel Zhunussova and Poornima Kumar 
# DATE CREATED: 09 February 2026 
# LAST UPDATED: 09 February 2026 by AZ
# ==============================================================================

# List of all packages used across the project scripts
pkgs <- c(
  # Core Data Manipulation
  "tidyverse", "lubridate", "readxl", "stringr", "scales", "dplyr", "tidyr", "purrr", "tools", "tictoc", "openxlsx",
  
  # Visualization
  "ggplot2", "patchwork", "ggridges", "corrplot", "binom", "shapviz", "gridGraphics", 
  
  # Statistics & Modeling
  "Hmisc", "BayesFactor", "caret", "glmnet", "randomForest", "pROC", "MLmetrics", 
  "kernelshap", "factoextra", "fpc", "Rtsne", "glinternet", "marginaleffects",
  "mediation", "DALEX", "ingredients"
)

# Function to check if packages are installed; if not, install them.
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    message(paste0(">>> Installing package: ", p))
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# Run the installation/loading loop
invisible(lapply(pkgs, install_if_missing))

message(">>> All required packages loaded successfully (n = ", length(pkgs), ").")