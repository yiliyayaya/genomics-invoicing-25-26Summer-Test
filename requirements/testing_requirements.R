REQUIRED_TESTING_PACKAGES <- c("testthat", "shinytest2", "devtools")

run_testing_setup <- function() {
  new_packages <- REQUIRED_TESTING_PACKAGES[!(REQUIRED_TESTING_PACKAGES %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}