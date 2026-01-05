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

check_requirements <- function() {
  # Identify which packages are not yet installed on the system
  missing_packages <- REQUIRED_PACKAGES[!(REQUIRED_PACKAGES %in% installed.packages()[,"Package"])]
  
  # Install missing packages if any are found
  if(length(missing_packages)) {
    observe({
      showNotification(
        "Please follow setup instructions and install all packages before proceeding.",
        duration = NULL,
        type = "error",
        id = "missing_packages"
      )
    })
  } 
}