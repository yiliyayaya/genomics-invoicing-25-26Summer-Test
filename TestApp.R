source("src/server.R")
source("src/ui.R")
source("requirements/requirements.R")
# ==============================================================================
# SECTION 1: PACKAGE MANAGEMENT & SETUP
# ==============================================================================

# Check and install required packages
run_setup()

# Load packages
library(shiny)        # Core framework for the web app
library(bslib)        # For modern Bootstrap themes and styling
library(DT)           # For interactive data tables
library(dplyr)        # For data manipulation (filter, select, mutate)
library(readxl)       # For reading Excel files
library(openxlsx)     # For creating and formatting Excel exports
library(tidyr)        # For data tidying (e.g., replace_na)
library(rmarkdown)    # For generating PDF reports
library(shinyjs)      # For JavaScript operations (hiding/disabling inputs, onclick events)
library(tinytex)      # Helper for compiling LaTeX to PDF
library(RColorBrewer) # For professional color palettes

ui <- main_ui_features()

server <- function(input, output, session) {
  # --- Startup Notification ---
  # Specific ID used for custom CSS yellow background
  observe({
    showNotification(
      "Please use the most up to date master spreadsheet in the download link at the top left hand side of this tool",
      duration = 60,
      type = "default",
      id = "startup_notice"
    )
  })
  
  # --- Reactive Values ---
  values <- reactiveValues(
    data = NULL,
    platform_select = "All", 
    cart = data.frame(
      Cart_ID = character(),
      Product_Code = character(),
      Name = character(),
      Description = character(), 
      Type = character(),
      Category = character(), 
      Base_Ref = numeric(),
      Add_Ref = numeric(),
      Is_Constant = logical(),
      Unit_Price = numeric(),
      Quantity = numeric(),
      Disc_Pct = numeric(),
      Disc_Amt = numeric(),
      Final_Total = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  main_server_logic(input, output, session, values)
  main_output_logic(input, output, values$cart)
}

# Run the Application
shinyApp(ui, server)