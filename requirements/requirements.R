# ==============================================================================
# MODULE: SYSTEM REQUIREMENTS
# DESCRIPTION: Ensures all R packages and LaTeX dependencies are installed.
# ==============================================================================

REQUIRED_PACKAGES <- c("shiny", "bslib", "DT", "dplyr", "readxl", "openxlsx", "purrr",
                       "tidyr", "rmarkdown", "shinyjs", "tinytex", "RColorBrewer")

run_setup <- function() {
  # --- 1. Identify and Install Missing R Packages ---
  # Check system library for existing packages to avoid redundant installations.
  new_packages <- REQUIRED_PACKAGES[!(REQUIRED_PACKAGES %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  
  # --- 2. TinyTeX Installation Logic ---
  # Only install TinyTeX if it is not already detected in the system path.
  # Removing 'force = TRUE' ensures that once installed, the environment remains stable,
  # significantly speeding up the application startup time on cloud servers.
  if (!tinytex::is_tinytex()) {
    tryCatch({
      tinytex::install_tinytex()
    }, error = function(e) {
      # Log the error message if installation fails
      message("TinyTeX installation encountered an issue: ", e$message)
    })
  }
}