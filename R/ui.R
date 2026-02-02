source("R/ui-files/charges-ui.R", local=TRUE)
source("R/ui-files/final-quote-ui.R", local=TRUE)
source("R/ui-files/item-ui.R", local=TRUE)
source("R/ui-files/application-protocol-ui.R", local=TRUE)

main_ui_features <- function() {
  # Main function to initialize app UI structure
  
  # Define a custom theme using Bootstrap 5 variables (WEHI branding colors)
  custom_theme <- bs_theme(
    version = 5, 
    primary = "#225EA8", 
    secondary = "#41B6C4",
    success = "#41B6C4", 
    info = "#1D91C0",
    base_font = font_google("Roboto"),
    "navbar-bg" = "#225EA8" 
  )
  
  app_interface <- page_sidebar(
    title = "WEHI Genomics Invoicing",
    theme = custom_theme,
    useShinyjs(), # Enable JavaScript capabilities
    
    # Custom CSS for UI enhancements
    tags$head(
      tags$style(HTML("
      /* Position all notifications in the bottom right corner */
      #shiny-notification-panel {
        top: unset !important;
        bottom: 20px !important;
        left: unset !important;
        right: 20px !important;
        width: 450px !important;
      }
      /* Specifically target the startup notification by unique ID to turn it light yellow */
      #shiny-notification-startup_notice {
        background-color: #FFF9C4 !important;
        color: #333 !important;
        border: 1px solid #FDD835 !important;
        font-weight: bold !important;
        opacity: 1 !important;
      }
      /* Layout for application vertical button stack */
      .application-btn-container {
        display: flex;
        flex-direction: column;
        gap: 4px !important;
        padding: 10px 15px !important;
        height: 40%;
        overflow: auto;
      }
      .application-action-btn {
        width: 60% !important; 
        text-align: left;
        font-weight: bold;
        border: none;
        padding: 8px 25px !important;
        border-radius: 4px !important;
        transition: transform 0.1s, opacity 0.2s;
        box-shadow: 1px 1px 3px rgba(0,0,0,0.1);
      }
      .application-action-btn:hover {
        transform: scale(1.01);
        opacity: 0.9;
      }
      /* Layout for protocol vertical button stack */
      .protocol-btn-container {
        display: flex;
        flex-direction: column;
        gap: 4px !important;
        padding: 10px 15px !important;
        height: 40%;
        overflow: auto;
      }
      .protocol-action-btn {
        width: 60% !important;
        text-align: left;
        font-weight: bold;
        border: none;
        padding: 8px 25px !important;
        border-radius: 4px !important;
        transition: transform 0.1s, opacity 0.2s;
        box-shadow: 1px 1px 3px rgba(0,0,0,0.1);
      }
      .protocol-action-btn:hover {
        transform: scale(1.01);
        opacity: 0.9;
      }
    "))
    ),
    
    # --- Sidebar Configuration ---
    sidebar = sidebar(
      width = 350,
      title = NULL,
      
      # Download Template Button (For users without the source file)
      div(style = "margin-bottom: 5px;",
          downloadButton("dl_template", "Download Template (.xlsx)", class = "btn-outline-primary w-100 btn-sm")
      ),
      # Global Input: File Upload (Always Visible)
      fileInput("master_sheet", "1. Upload Master Spreadsheet (.xlsx)", accept = ".xlsx"),
      # Tab 2 Specific: Filter Items
      item_sidebar_filters(),
      # Tab 3 Specific: Filter Services
      charges_sidebar_filters(),
      # Tab 4 Specific: Project Configuration & Metadata
      quote_config_panel()
    ),
    
    # --- Main Content Area (Tabs) ---
    navset_card_underline(
      id = "nav_tabs",
      
      # Tab 1: Select application
      structure_application_protocol_page(),
      # Tab 2: Catalog for Consumables
      structure_items_page(),
      # Tab 3: Catalog for Services
      structure_charges_page(),
      # Tab 4: Final Quote Review
      structure_quote_page()
    )
  )
  
  return(app_interface)
}