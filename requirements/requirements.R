REQUIRED_PACKAGES <- c("shiny", "bslib", "DT", "dplyr", "readxl", "openxlsx", "tidyr", "rmarkdown", "shinyjs", "tinytex", "RColorBrewer")

run_setup <- function() {
  # Identify which packages are not yet installed on the system
  new_packages <- REQUIRED_PACKAGES[!(REQUIRED_PACKAGES %in% installed.packages()[,"Package"])]
  
  # Install missing packages if any are found
  if(length(new_packages)) install.packages(new_packages)
  
  # Ensure TinyTeX is installed for PDF generation immediately during setup
  if (!tinytex::is_tinytex()) {
    tinytex::install_tinytex(force = TRUE)
  }  
}