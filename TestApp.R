# ==============================================================================
# SECTION 1: PACKAGE MANAGEMENT & SETUP
# ==============================================================================

# Define the list of required packages for the application
packages <- c("shiny", "bslib", "DT", "dplyr", "readxl", "openxlsx", "tidyr", "rmarkdown", "shinyjs", "tinytex")

# Identify which packages are not yet installed on the system
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install missing packages if any are found
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(shiny)      # Core framework for the web app
library(bslib)      # For modern Bootstrap themes and styling
library(DT)         # For interactive data tables
library(dplyr)      # For data manipulation (filter, select, mutate)
library(readxl)     # For reading Excel files
library(openxlsx)   # For creating and formatting Excel exports
library(tidyr)      # For data tidying (e.g., replace_na)
library(rmarkdown)  # For generating PDF reports
library(shinyjs)    # For JavaScript operations (hiding/disabling inputs, onclick events)
library(tinytex)    # Helper for compiling LaTeX to PDF

# ==============================================================================
# SECTION 2: BACKEND LOGIC (Data Processing)
# ==============================================================================

# Function: process_pricing_logic
# Purpose: Reads the uploaded Excel file and processes pricing rules based on fixed row order.
# Input: file_path (location of the uploaded .xlsx file)
process_pricing_logic <- function(file_path) {
  
  # --- Step 1: Process Configuration (Sheet 3) ---
  # Reads the multiplier configurations.
  # Logic relies on ROW ORDER, not label names.
  raw_config <- read_excel(file_path, sheet = 3, col_names = TRUE)
  
  # Select only relevant columns: Type (Col 1) and Amount (Col 3)
  df_config <- raw_config %>%
    select(1, 3) %>% 
    setNames(c("Type", "Amount")) %>%
    mutate(Amount = as.numeric(Amount))
  
  # --- LOGIC A: PRICE_LIST (Consumables/Items) ---
  # Strategy: Filter for "PRICE_LIST" and use row indices.
  # Row 1 -> Internal Multiplier
  # Row 2 -> External Multiplier
  pl_rows <- df_config %>% filter(Type == "PRICE_LIST")
  
  val_item_int <- if(nrow(pl_rows) >= 1) pl_rows$Amount[1] else 1.1 # Default 1.1 if missing
  val_item_ext <- if(nrow(pl_rows) >= 2) pl_rows$Amount[2] else 1.3 # Default 1.3 if missing
  
  item_logic <- list(
    "Internal" = val_item_int,
    "External" = val_item_ext
  )
  
  # --- LOGIC B: PROCESSING (Services) ---
  # Strategy: Filter for "PROCESSING" and use cumulative product (cumprod).
  # Row 1 -> Internal
  # Row 2 -> Ext. Collaborative
  # Row 3 -> Ext. RSA
  # Row 4 -> Commercial
  proc_rows <- df_config %>% 
    filter(Type == "PROCESSING") %>%
    mutate(Cumulative = cumprod(Amount)) # Multipliers stack on top of each other
  
  # Helper function to safely retrieve cumulative value by row index
  get_proc_val <- function(idx) {
    if(nrow(proc_rows) >= idx) return(proc_rows$Cumulative[idx]) else return(1)
  }
  
  processing_logic <- list(
    "Internal"          = get_proc_val(1), # Corresponds to Row 1
    "Ext.Collaborative" = get_proc_val(2), # Corresponds to Row 2
    "Ext.RSA"           = get_proc_val(3), # Corresponds to Row 3
    "Commercial"        = get_proc_val(4)  # Corresponds to Row 4
  )
  
  # --- Step 2: Process Items Catalog (Sheet 1) ---
  # Reads the list of consumable items.
  raw_items <- read_excel(file_path, sheet = 1, col_names = TRUE)
  
  df_items <- raw_items %>%
    select(1:8) %>% 
    setNames(c("Product_Code", "Brand", "Item", "Category", "Description", "Base_Cost", "Add_Cost", "Is_Constant")) %>%
    mutate(
      Base_Cost = as.numeric(Base_Cost),
      Add_Cost = as.numeric(replace_na(Add_Cost, 0)),
      Is_Constant = as.character(Is_Constant),
      # Normalize boolean text to logical TRUE/FALSE
      Is_Constant = case_when(
        toupper(Is_Constant) %in% c("TRUE", "T", "YES", "Y", "1") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(!is.na(Base_Cost)) %>%
    mutate(across(where(is.character), as.character)) 
  
  # Get Platform and Item pairs
  item_platform_needed_cols <- c("Brand", "Item", "Platform")
  df_platform_item <- raw_items[, item_platform_needed_cols]
  df_platform_item$Platform[is.na(df_platform_item$Platform)] <- "ALL_PLATFORMS"
  df_platform_item <- df_platform_item %>% 
    setNames(c("Brand", "Item", "Platform_String")) %>%
    mutate(Brand = as.character(Brand),
           Item = as.character(Item),
           Platform_String = as.character(Platform_String),
           Platform = strsplit(Platform_String, split = ";", fixed = TRUE)) %>%
    select(-Platform_String)
    
  
  # --- Step 3: Process Services Catalog (Sheet 2) ---
  # Reads the list of processing services.
  raw_services <- read_excel(file_path, sheet = 2, col_names = TRUE)
  
  df_services <- raw_services %>%
    select(1, 3, 4, 5) %>% 
    setNames(c("Group", "Service", "Description", "Base_Price")) %>%
    mutate(Base_Price = as.numeric(Base_Price)) %>%
    filter(!is.na(Base_Price)) %>%
    mutate(across(where(is.character), as.character))
  
  # Get Platform and Service pairs
  process_platform_needed_col <- c("Service", "Platform")
  df_platform_process <- raw_services[, process_platform_needed_col]
  df_platform_process <- df_platform_process %>% 
    setNames(c("Service", "Platform_String")) %>%
    mutate(Service = as.character(Service),
           Platform_String = as.character(Platform_String),
           Platform = strsplit(Platform_String, ";", fixed = TRUE)) %>%
    select(-Platform_String)
  
  # --- Step 4: Processing Discounts (Sheet 4) ---
  # Reads the list of discounts
  raw_discounts <- read_excel(file_path, sheet = 4, col_names = TRUE)
  df_discounts <- raw_discounts %>%
    setNames(c("Supplier", "Label", "Amount", "End_Date")) %>%
    mutate(
      Supplier = as.character(Supplier),
      Label = as.character(Label),
      Amount = as.numeric(Amount),
      End_Date = as.Date(End_Date)
    ) %>%
    filter(!is.na(Amount)) %>%
    mutate(End_Date = replace_na(End_Date, Sys.Date())) %>%
    filter(End_Date >= Sys.Date()) %>%
    mutate(Display_Text = paste(Supplier, Label, Amount, sep = " | ")) %>%
    select(-End_Date)
  
  # Return structured list containing all processed data and logic maps
  return(list(items = df_items, services = df_services, logic_proc = processing_logic, logic_item = item_logic, 
              platform_item = df_platform_item, platform_proc = df_platform_process, supplier_discount = df_discounts))
}

# ==============================================================================
# SECTION 3: UI DESIGN (User Interface)
# ==============================================================================

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

# Page Layout: Sidebar layout with navigation tabs
ui <- page_sidebar(
  title = "WEHI Genomics Invoicing",
  theme = custom_theme,
  useShinyjs(), # Enable JavaScript capabilities
  
  # --- Sidebar Configuration ---
  sidebar = sidebar(
    width = 350,
    title = NULL,
    
    # 0. Helper: Download Template Button (For users without the source file)
    # Using margin-bottom: 0px to ensure it sits directly above the file input
    div(style = "margin-bottom: 5px;",
        downloadButton("dl_template", "Download Template (.xlsx)", class = "btn-outline-primary w-100 btn-sm")
    ),
    
    # 1. Global Input: File Upload (Always Visible)
    fileInput("master_sheet", "1. Upload Master Spreadsheet (.xlsx)", accept = ".xlsx"),
    
    # 2. Tab 2 Specific: Filter Items
    conditionalPanel(
      condition = "input.nav_tabs == 'tab_items'",
      h5("Filter Items"),
      selectInput("filter_brand", "Filter Brand", choices = "All", selectize = TRUE), 
      selectInput("filter_category", "Filter Category", choices = "All"),
      div(class = "text-muted small mb-2", "Tip: Select items in the table, then click 'Add'."),
      actionButton("add_items_btn", "Add Selected to Quote", class = "btn-success w-100")
    ),
    
    # 3. Tab 3 Specific: Filter Services
    conditionalPanel(
      condition = "input.nav_tabs == 'tab_processing'",
      h5("Filter Services"),
      selectInput("filter_group", "Filter Group", choices = "All"),
      br(),
      actionButton("add_proc_btn", "Add Selected to Quote", class = "btn-success w-100")
    ),
    
    # 4. Tab 4 Specific: Project Configuration & Metadata
    # This section contains the Project Type selector and Multiplier Table
    conditionalPanel(
      condition = "input.nav_tabs == 'tab_quote'",
      h5("Quote Configuration"),
      
      # Project Type Selector (Required for final calculation)
      selectInput("project_type", "2. Project Type", 
                  choices = c("", "Internal", "Ext.Collaborative", "Ext.RSA", "Commercial")),
      
      # Display table for current active multipliers (Visible only if Project Type is selected)
      conditionalPanel(
        condition = "input.project_type !== ''",
        div(style = "margin-bottom: 10px; padding-bottom: 0px;", 
            strong("Current Multipliers:"),
            tableOutput("multiplier_table") 
        )
      ),
      hr(),
      
      selectInput("supplier_discount_select", "Supplier Discounts (Optional)", choices = c(""), selectize=TRUE),
      actionButton("apply_supp_disc_btn", "Apply Supplier Discount"),
      
      numericInput("percent_discount_input", "Custom Discount (as % between 0-100)", value = 0, min = 0, max = 100),
      actionButton("apply_percent_discount", "Apply Custom % Discount"),
      
      numericInput("amount_discount_input", "Custom Discount (as $)", value = 0, min = 0),
      actionButton("apply_amount_discount", "Apply Custom $ Discount"),
      
      hr(),
      h5("Project Metadata"),
      textInput("meta_date", "Date (DD/MM/YYYY)", value = format(Sys.Date(), "%d/%m/%Y")),
      textInput("quote_id", "Quote ID", value = paste0("Q-", format(Sys.Date(), "%Y%m%d"))),
      textInput("meta_proj_id", "Project ID", placeholder = "e.g., P-12345"),
      textInput("meta_title", "Project Title", placeholder = "e.g., Single Cell Analysis"),
      textInput("meta_platform", "Platform", placeholder = "e.g., 10x Genomics"),
      numericInput("meta_samples", "Total # samples", value = 1, min = 1),
      numericInput("meta_batches", "# batches", value = 1, min = 1),
      textInput("meta_samples_batch", "# samples per batch", placeholder = "e.g., 4"),
      textInput("meta_aimed_cells", "Aimed # cells per sample", placeholder = "e.g., 5000")
    )
  ),
  
  # --- Main Content Area (Tabs) ---
  navset_card_underline(
    id = "nav_tabs",
    
    # Tab 1: Select platform for filter
    nav_panel(
      title = "1. Select Platform",
      value = "tab_platform",
      card (
        card_header("Select Platform"),
        radioButtons("platform_select", "Platform Selection", choices = "All"),
      )
    ),
    
    # Tab 2: Catalog for Consumables
    nav_panel(
      title = "2. Select Items",
      value = "tab_items",
      card(
        card_header("Items Catalog (Consumables)"),
        DTOutput("table_items_catalog")
      )
    ),
    
    # Tab 3: Catalog for Services
    nav_panel(
      title = "3. Select Processing",
      value = "tab_processing",
      card(
        card_header("Processing Services Catalog"),
        DTOutput("table_proc_catalog")
      )
    ),
    
    # Tab 4: Final Quote Review
    nav_panel(
      title = "4. Final Quote",
      value = "tab_quote",
      card(
        full_screen = TRUE, 
        card_header("Review & Edit Quote"),
        card_body(
          # Conditional Panel: Only show table if Project Type is selected
          conditionalPanel(
            condition = "input.project_type !== ''",
            
            # Action buttons for the quote table
            div(class = "d-flex justify-content-between align-items-center",
                div(actionButton("remove_row_btn", "Remove Selected Row", class = "btn-danger btn-sm", icon = icon("trash"))),
                div(downloadButton("dl_excel", "Download .xlsx", class = "btn-secondary btn-sm"),
                    downloadButton("dl_pdf", "Download PDF Invoice", class = "btn-primary btn-sm"))
            ),
            hr(),
            DTOutput("table_final_quote") # Editable Data Table
          ),
          
          # Warning Message: Shown if no project type is selected
          conditionalPanel(
            condition = "input.project_type == ''",
            div(class = "alert alert-warning", 
                "Please select a 'Project Type' in the sidebar to generate the quote.")
          )
        ),
        card_footer(
          div(style = "text-align: right; font-size: 1.2rem; font-weight: bold;",
              textOutput("grand_total_display") # Display grand total text
          )
        )
      )
    )
  )
)

# ==============================================================================
# SECTION 4: SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # --- Helper Function: Latex Escape ---
  # Sanitizes text inputs to prevent LaTeX errors during PDF generation.
  escape_latex <- function(x) {
    if (is.null(x) || is.na(x)) return("NA")
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x) 
    x <- gsub("([#$%&_{}])", "\\\\\\1", x)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
    x <- gsub("~", "\\\\textasciitilde{}", x)
    return(x)
  }
  
  # Check availability of tinytex for PDF generation
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    showNotification("Error: 'tinytex' package is missing.", type = "error", duration = NULL)
  }
  
  # --- Reactive Values ---
  # 'values' stores the application state.
  values <- reactiveValues(
    data = NULL, # Stores the data loaded from the Excel file
    # Stores the "Shopping Cart". 
    # Note: Includes Base Cost data to allow recalculation when Project Type changes.
    cart = data.frame(
      Cart_ID = character(),
      Product_Code = character(),
      Name = character(),
      Description = character(), 
      Type = character(),
      Category = character(), 
      
      # Pricing Calculation Fields
      Base_Ref = numeric(),  # Stores Base Cost/Price
      Add_Ref = numeric(),   # Stores Additional Cost
      Is_Constant = logical(),
      
      # Display Fields
      Unit_Price = numeric(),
      Quantity = numeric(),
      Disc_Pct = numeric(),
      Disc_Amt = numeric(),
      Final_Total = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  # --- Event: File Upload ---
  observeEvent(input$master_sheet, {
    req(input$master_sheet)
    tryCatch({
      # Execute the data processing logic on the uploaded file
      values$data <- process_pricing_logic(input$master_sheet$datapath)
      
      # Populate sidebar filters based on the loaded data
      brands_sorted <- sort(unique(values$data$items$Brand))
      cats_sorted <- sort(unique(values$data$items$Category))
      groups_sorted <- sort(unique(values$data$services$Group))
      supplier_discount_labels <- sort(unique(values$data$supplier_discount$Display_Text))
      
      platform_sorted <- sort(unique(c(unlist(values$data$platform_item$Platform, use.names = FALSE), 
                                       unlist(values$data$platform_proc$Platform, use.names = FALSE))))
      platform_sorted <- platform_sorted[!(platform_sorted %in% c("ALL_PLATFORMS"))]
      
      updateSelectInput(session, "filter_brand", choices = c("All", brands_sorted))
      updateSelectInput(session, "filter_category", choices = c("All", cats_sorted))
      updateSelectInput(session, "filter_group", choices = c("All", groups_sorted))
      updateRadioButtons(session, "platform_select", choices = c("All", platform_sorted))
      updateSelectInput(session, "supplier_discount_select", choices = c("", supplier_discount_labels))
      
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # --- Output: Multiplier Table ---
  # Shows users which multipliers are currently applied based on Project Type.
  output$multiplier_table <- renderTable({
    req(values$data, input$project_type)
    if(input$project_type == "") return(NULL)
    
    cat <- input$project_type
    proc_mult <- round(values$data$logic_proc[[cat]], 3)
    rate_type <- if(cat == "Internal") "Internal" else "External"
    item_mult <- values$data$logic_item[[rate_type]]
    
    data.frame(
      Category = c("Items (Consumables)", "Processing Services"),
      Multiplier = c(paste0("x", item_mult), paste0("x", proc_mult))
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # --- Logic: Calculate Item Prices (Tab 1 Display) ---
  # Calculates both Internal and External prices for initial display.
  output$table_items_catalog <- renderDT({
    req(values$data)
    df <- values$data$items
    
    if(input$filter_brand != "All") df <- df %>% filter(Brand == input$filter_brand)
    if(input$filter_category != "All") df <- df %>% filter(Category == input$filter_category)
    if(input$platform_select != "All") {
      common_items <- values$data$platform_item %>% 
        rowwise() %>%
        filter(any(Platform %in% input$platform_select) | any(Platform == "ALL_PLATFORMS")) %>%
        ungroup()
      df <- df %>% semi_join(common_items, by=c("Item", "Brand"))
    } 
    
    # Retrieve Multipliers
    m_int <- values$data$logic_item[["Internal"]]
    m_ext <- values$data$logic_item[["External"]]
    
    # Calculate columns for display
    df_display <- df %>% 
      mutate(
        Price_Internal = ifelse(Is_Constant, Base_Cost + Add_Cost, (Base_Cost * m_int) + Add_Cost),
        Price_External = ifelse(Is_Constant, Base_Cost + Add_Cost, (Base_Cost * m_ext) + Add_Cost)
      ) %>%
      select(Product_Code, Brand, Item, Description, Price_Internal, Price_External)
    
    datatable(
      df_display,
      selection = "multiple",
      options = list(pageLength = 10),
      colnames = c("Code", "Brand", "Item", "Description", "Internal Price", "External Price")
    ) %>%
      formatCurrency(c("Price_Internal", "Price_External"))
  })
  
  # --- Event: Add Items to Cart ---
  observeEvent(input$add_items_btn, {
    if (is.null(input$table_items_catalog_rows_selected)) {
      showNotification("Please select items first!", type = "warning")
      return()
    }
    
    # Re-fetch the filtered dataframe to ensure indices match
    df_full <- values$data$items
    if(input$filter_brand != "All") df_full <- df_full %>% filter(Brand == input$filter_brand)
    if(input$filter_category != "All") df_full <- df_full %>% filter(Category == input$filter_category)
    
    selected_indices <- input$table_items_catalog_rows_selected
    items_to_add <- df_full[selected_indices, ]
    
    # Prevent adding duplicates
    existing_codes <- values$cart$Product_Code
    items_to_add_unique <- items_to_add %>% filter(!Product_Code %in% existing_codes)
    
    if (nrow(items_to_add_unique) == 0) return()
    
    # Structure data for the cart (Store Base Costs for dynamic recalculation)
    new_entries <- items_to_add_unique %>%
      mutate(
        Cart_ID = paste0("I-", as.numeric(Sys.time()), "-", row_number()),
        Type = "Item",
        Quantity = 1, Disc_Pct = 0, Disc_Amt = 0,
        Base_Ref = Base_Cost,
        Add_Ref = Add_Cost,
        # Initialize Unit_Price to 0 until Project Type is selected
        Unit_Price = 0, Final_Total = 0
      ) %>%
      select(Cart_ID, Product_Code, Name = Item, Description, Type, Category, 
             Base_Ref, Add_Ref, Is_Constant, Unit_Price, Quantity, Disc_Pct, Disc_Amt, Final_Total)
    
    values$cart <- bind_rows(values$cart, new_entries) %>% as.data.frame()
    showNotification(paste(nrow(new_entries), "new items added."), type = "message")
  })
  
  # --- Logic: Calculate Service Prices (Tab 2 Display) ---
  # Calculates 4 specific columns for display based on Sheet 3 Logic.
  output$table_proc_catalog <- renderDT({
    req(values$data)
    df <- values$data$services
    if(input$filter_group != "All") df <- df %>% filter(Group == input$filter_group)
    if(input$platform_select != "All") {
      common_services <- values$data$platform_proc %>% 
        rowwise() %>%
        filter(any(Platform %in% input$platform_select) | any(Platform == "ALL_PLATFORMS")) %>%
        ungroup()
      df <- df[df$Service %in% common_services$Service, ]
    }
    
    # Retrieve Multipliers from processed logic
    m_int <- values$data$logic_proc[["Internal"]]
    m_col <- values$data$logic_proc[["Ext.Collaborative"]]
    m_rsa <- values$data$logic_proc[["Ext.RSA"]]
    m_com <- values$data$logic_proc[["Commercial"]]
    
    df_display <- df %>% 
      mutate(
        P_Int = Base_Price * m_int,
        P_Col = Base_Price * m_col,
        P_RSA = Base_Price * m_rsa,
        P_Com = Base_Price * m_com
      ) %>%
      select(Group, Service, Description, P_Int, P_Col, P_RSA, P_Com)
    
    datatable(
      df_display,
      selection = "multiple", options = list(pageLength = 10, scrollX = TRUE),
      colnames = c("Group", "Service", "Description", "Internal", "Ext. Collab", "Ext. RSA", "Commercial")
    ) %>%
      formatCurrency(c("P_Int", "P_Col", "P_RSA", "P_Com"))
  })
  
  # --- Event: Add Services to Cart ---
  observeEvent(input$add_proc_btn, {
    if (is.null(input$table_proc_catalog_rows_selected)) {
      showNotification("Please select services first!", type = "warning")
      return()
    }
    
    df_full <- values$data$services
    if(input$filter_group != "All") df_full <- df_full %>% filter(Group == input$filter_group)
    
    selected_indices <- input$table_proc_catalog_rows_selected
    items_to_add <- df_full[selected_indices, ]
    
    existing_names <- values$cart$Name
    items_to_add_unique <- items_to_add %>% filter(!Service %in% existing_names)
    
    if (nrow(items_to_add_unique) == 0) return()
    
    new_entries <- items_to_add_unique %>%
      mutate(
        Cart_ID = paste0("P-", as.numeric(Sys.time()), "-", row_number()),
        Type = "Processing",
        Category = Group, 
        Quantity = 1, Disc_Pct = 0, Disc_Amt = 0,
        Product_Code = "SVC",
        Base_Ref = Base_Price,
        Add_Ref = 0,
        Is_Constant = FALSE,
        Unit_Price = 0, Final_Total = 0
      ) %>%
      select(Cart_ID, Product_Code, Name = Service, Description, Type, Category, 
             Base_Ref, Add_Ref, Is_Constant, Unit_Price, Quantity, Disc_Pct, Disc_Amt, Final_Total)
    
    values$cart <- bind_rows(values$cart, new_entries) %>% as.data.frame()
    showNotification(paste(nrow(new_entries), "new services added."), type = "message")
  })
  
  # --- Observer: Recalculate Cart when Project Type Changes ---
  # This logic ensures that when the user selects a project type in Tab 3,
  # all items in the cart are updated with the correct pricing.
  observeEvent(input$project_type, {
    req(values$data, nrow(values$cart) > 0)
    
    # If selection is empty, we cannot calculate
    if(input$project_type == "") return()
    
    ptype <- input$project_type
    
    # Determine Multipliers based on new project type
    rate_type_item <- if(ptype == "Internal") "Internal" else "External"
    mult_item <- values$data$logic_item[[rate_type_item]]
    mult_proc <- values$data$logic_proc[[ptype]]
    
    if(is.null(mult_proc)) mult_proc <- 1 # Safety fallback
    
    # Apply logic row by row (vectorized)
    values$cart <- values$cart %>%
      mutate(
        Unit_Price = case_when(
          Type == "Item" & Is_Constant ~ Base_Ref + Add_Ref,
          Type == "Item" & !Is_Constant ~ (Base_Ref * mult_item) + Add_Ref,
          Type == "Processing" ~ Base_Ref * mult_proc,
          TRUE ~ 0
        ),
        # Recalculate Totals maintaining quantity and discount
        Gross = Unit_Price * Quantity,
        Disc_Amt = Gross * (Disc_Pct / 100),
        Final_Total = Gross - Disc_Amt
      ) %>%
      select(-Gross) # Remove temp column
  })
  
  # --- Output: Final Quote Table ---
  output$table_final_quote <- renderDT({
    req(input$project_type) # Only show if project type selected
    
    display_df <- values$cart %>% 
      select(Product_Code, Name, Description, Type, Unit_Price, Quantity, Disc_Pct, Disc_Amt, Final_Total) %>%
      as.data.frame() 
    
    datatable(display_df,
              selection = "multiple",
              # Allow editing of Quantity (col 5), Discount % (col 6), and Discount $ (col 7)
              # Disabled: Code(0), Name(1), Desc(2), Type(3), Unit_Price(4), Total(8)
              editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4, 8))),
              options = list(
                pageLength = 25, 
                dom = 't',
                scrollX = TRUE 
              ),
              rownames = FALSE,
              colnames = c("Code", "Name", "Description", "Type", "Unit Price", "Quantity", "Discount %", "Discount $", "Total")) %>%
      formatCurrency(c("Unit_Price", "Disc_Amt", "Final_Total")) %>%
      formatRound("Disc_Pct", 2)
  }, server = FALSE) 
  
  # --- Event: Edit Quote Table Cells ---
  # Dynamic recalculation when user edits Quantity or Discount fields.
  observeEvent(input$table_final_quote_cell_edit, {
    info <- input$table_final_quote_cell_edit
    row_idx <- info$row
    col_idx <- info$col 
    new_val <- as.numeric(info$value)
    
    current_row <- values$cart[row_idx, ]
    price <- current_row$Unit_Price
    qty   <- current_row$Quantity
    
    # Case: Quantity Edited (Index 5)
    if (col_idx == 5) {
      qty <- new_val
      values$cart$Quantity[row_idx] <- qty
      pct <- values$cart$Disc_Pct[row_idx]
      gross <- price * qty
      disc_amt <- gross * (pct / 100)
      values$cart$Disc_Amt[row_idx] <- disc_amt
      values$cart$Final_Total[row_idx] <- gross - disc_amt
      
      # Case: Discount Percent Edited (Index 6)
    } else if (col_idx == 6) { 
      pct <- new_val
      values$cart$Disc_Pct[row_idx] <- pct
      gross <- price * qty
      disc_amt <- gross * (pct / 100)
      values$cart$Disc_Amt[row_idx] <- disc_amt
      values$cart$Final_Total[row_idx] <- gross - disc_amt
      
      # Case: Discount Amount Edited (Index 7)
    } else if (col_idx == 7) { 
      amt <- new_val
      values$cart$Disc_Amt[row_idx] <- amt
      gross <- price * qty
      pct <- if(gross > 0) (amt / gross) * 100 else 0
      values$cart$Disc_Pct[row_idx] <- pct
      values$cart$Final_Total[row_idx] <- gross - amt
    }
  })
  
  # --- Observer: Apply Supplier Discount ---
  observeEvent(input$apply_supp_disc_btn, {
    req(input$supplier_discount_select, input$table_final_quote_rows_selected)
    if(input$supplier_discount_select == "") {
      return()
    }
    discount_data <- values$data$supplier_discount %>% filter(Display_Text == input$supplier_discount_select)
    discount_pct <- discount_data$Amount[1] * 100
    
    for (row_idx in input$table_final_quote_rows_selected) {
      current_row <- values$cart[row_idx, ]
      
      values$cart$Disc_Pct[row_idx] <- discount_pct
      pre_discount_total <- current_row$Unit_Price * current_row$Quantity
      discount_amt <- pre_discount_total * (discount_pct / 100)
      values$cart$Disc_Amt[row_idx] <- discount_amt
      values$cart$Final_Total[row_idx] <- pre_discount_total - discount_amt
    }
  })
  
  # --- Observer: Apply Custom Discounts ---
  # Apply % discount
  observeEvent(input$apply_percent_discount, {
    req(input$table_final_quote_rows_selected, input$percent_discount_input)
    if(input$percent_discount_input < 0 | input$percent_discount_input > 100) {
      showNotification("Input Error: % Discount must be numeric between 0 and 100.", type = "warning")
      return()
    }
    
    discount_pct <- input$percent_discount_input
    
    for (row_idx in input$table_final_quote_rows_selected) {
      current_row <- values$cart[row_idx, ]
      values$cart$Disc_Pct[row_idx] <- discount_pct
      pre_discount_total <- current_row$Unit_Price * current_row$Quantity
      discount_amt <- pre_discount_total * (discount_pct / 100)
      values$cart$Disc_Amt[row_idx] <- discount_amt
      values$cart$Final_Total[row_idx] <- pre_discount_total - discount_amt
    }
  })
  
  # Apply $ discount
  observeEvent(input$apply_amount_discount, {
    req(input$table_final_quote_rows_selected, input$amount_discount_input)
    if(input$amount_discount_input < 0) {
      showNotification("Input Error: $ Discount must non-negative.", type = "warning")
      return()
    }
    
    discount_amt <- input$amount_discount_input

    for (row_idx in input$table_final_quote_rows_selected) {
      current_row <- values$cart[row_idx, ]
      values$cart$Disc_Amt[row_idx] <- discount_amt
      pre_discount_total <- current_row$Unit_Price * current_row$Quantity
      if(pre_discount_total < discount_amt) {
        discount_pct <-  100
        values$cart$Disc_Amt[row_idx] <- pre_discount_total
        values$cart$Disc_Pct[row_idx] <- discount_pct
        values$cart$Final_Total[row_idx] <- 0
      } else {
        discount_pct <- (discount_amt / pre_discount_total) * 100
        values$cart$Disc_Pct[row_idx] <- discount_pct
        values$cart$Final_Total[row_idx] <- pre_discount_total - discount_amt  
      }
    }
  })
  
  # --- Event: Remove Row ---
  observeEvent(input$remove_row_btn, {
    req(input$table_final_quote_rows_selected)
    idx <- input$table_final_quote_rows_selected
    values$cart <- values$cart[-idx, ]
  })
  
  # --- Output: Grand Total Calculation ---
  output$grand_total_display <- renderText({
    if(nrow(values$cart) == 0 || input$project_type == "") return("Total (Per Batch): $0.00")
    total <- sum(values$cart$Final_Total, na.rm = TRUE)
    batches <- input$meta_batches
    paste0("Total (Per Batch): $", formatC(total, format="f", digits=2, big.mark=","), 
           " | Total Project (", batches, " batches): $", formatC(total * batches, format="f", digits=2, big.mark=","))
  })
  
  # ============================================================================
  # SECTION 5: EXPORT HANDLERS (Excel & PDF)
  # ============================================================================
  
  # --- Handler: Template Download ---
  # Allows users to download the template file directly from the app
  output$dl_template <- downloadHandler(
    filename = "master_spreadsheet_25_26_summer.xlsx",
    content = function(file) {
      # Copies the file from the deployment bundle to the user's download stream
      file.copy("master_spreadsheet_25_26_summer.xlsx", file)
    }
  )
  
  # --- Handler: Excel Download ---
  output$dl_excel <- downloadHandler(
    filename = function() { paste0(input$quote_id, "_Quote.xlsx") },
    content = function(file) {
      df_cart <- values$cart
      
      # Prepare data table for export
      df_table <- df_cart %>%
        mutate(Total_Amount_AUD = Unit_Price * Quantity) %>%
        select(Name, Unit_Price, Description, Quantity, Total_Amount_AUD) 
      
      # Calculate totals for the report
      raw_total_batch <- sum(df_table$Total_Amount_AUD, na.rm = TRUE)
      discount_total_batch <- sum(df_cart$Disc_Amt, na.rm = TRUE)
      net_total_batch <- raw_total_batch - discount_total_batch
      
      batches <- as.numeric(input$meta_batches)
      project_total <- net_total_batch * batches
      batch_label <- ifelse(batches > 1, "batches", "batch")
      
      # Create Workbook using 'openxlsx'
      wb <- createWorkbook()
      addWorksheet(wb, "Invoice")
      showGridLines(wb, "Invoice", showGridLines = FALSE) 
      
      # Define Formatting Styles
      style_header_title <- createStyle(fontSize = 14, fontColour = "#225EA8", textDecoration = "bold")
      style_italic <- createStyle(textDecoration = "italic")
      style_bold <- createStyle(textDecoration = "bold")
      style_currency <- createStyle(numFmt = "$#,##0.00")
      style_line_bold <- createStyle(border = "bottom", borderColour = "black", borderStyle = "medium")
      style_line_thin <- createStyle(border = "bottom", borderColour = "black", borderStyle = "thin")
      style_grand_total <- createStyle(fontSize = 12, textDecoration = "bold")
      
      # Write Headers
      writeData(wb, "Invoice", input$meta_title, startRow = 1, startCol = 1)
      addStyle(wb, "Invoice", style_header_title, rows = 1, cols = 1)
      writeData(wb, "Invoice", "Project based cost estimate", startRow = 2, startCol = 1)
      addStyle(wb, "Invoice", style_italic, rows = 2, cols = 1)
      
      # Write Metadata Field Logic
      meta_fields <- list(
        c("Date:", input$meta_date),
        c("Quote ID:", input$quote_id),
        c("Project ID:", input$meta_proj_id),
        c("Project Title:", input$meta_title),
        c("Project type:", input$project_type),
        c("Platform:", input$meta_platform),
        c("Total # samples:", input$meta_samples),
        c("# batches:", input$meta_batches),
        c("# samples per batch:", input$meta_samples_batch),
        c("Aimed # cells per sample:", input$meta_aimed_cells)
      )
      
      row_ptr <- 4
      # Manually write metadata rows with styling
      writeData(wb, "Invoice", paste(meta_fields[[1]][1], meta_fields[[1]][2]), startRow = row_ptr, startCol = 1); addStyle(wb, "Invoice", style_bold, rows = row_ptr, cols = 1); row_ptr <- row_ptr + 1
      writeData(wb, "Invoice", paste(meta_fields[[2]][1], meta_fields[[2]][2]), startRow = row_ptr, startCol = 1); addStyle(wb, "Invoice", style_bold, rows = row_ptr, cols = 1); row_ptr <- row_ptr + 2
      
      for(i in 3:length(meta_fields)) {
        writeData(wb, "Invoice", paste(meta_fields[[i]][1], meta_fields[[i]][2]), startRow = row_ptr, startCol = 1)
        addStyle(wb, "Invoice", style_bold, rows = row_ptr, cols = 1)
        row_ptr <- row_ptr + 1
      }
      
      # Write Itemized Table
      start_tbl_row <- row_ptr + 2
      writeData(wb, "Invoice", "Cost per batch", startRow = start_tbl_row, startCol = 1)
      addStyle(wb, "Invoice", createStyle(fontSize=12, textDecoration="bold"), rows=start_tbl_row, cols=1)
      addStyle(wb, "Invoice", style_line_bold, rows = start_tbl_row, cols = 1:5, stack = TRUE)
      
      headers <- c("Item", "Amount", "Description", "Quantity", "Total Amount [AUD]")
      writeData(wb, "Invoice", t(headers), startRow = start_tbl_row + 1, startCol = 1, colNames = FALSE)
      addStyle(wb, "Invoice", style_bold, rows = start_tbl_row + 1, cols = 1:5)
      addStyle(wb, "Invoice", style_line_thin, rows = start_tbl_row + 1, cols = 1:5, stack = TRUE)
      
      if(nrow(df_table) > 0) {
        writeData(wb, "Invoice", df_table, startRow = start_tbl_row + 2, startCol = 1, colNames = FALSE)
        data_rows <- (start_tbl_row + 2):(start_tbl_row + 1 + nrow(df_table))
        addStyle(wb, "Invoice", style_currency, rows = data_rows, cols = 2, stack = TRUE)
        addStyle(wb, "Invoice", style_currency, rows = data_rows, cols = 5, stack = TRUE)
        last_row <- max(data_rows)
      } else {
        last_row <- start_tbl_row + 2
      }
      
      addStyle(wb, "Invoice", style_line_bold, rows = last_row, cols = 1:5, stack = TRUE)
      
      # Write Total Summary
      row_sum_start <- last_row + 1
      writeData(wb, "Invoice", "Total (per batch)", startRow = row_sum_start, startCol = 1)
      writeData(wb, "Invoice", raw_total_batch, startRow = row_sum_start, startCol = 5)
      addStyle(wb, "Invoice", style_bold, rows = row_sum_start, cols = 1)
      addStyle(wb, "Invoice", style_currency, rows = row_sum_start, cols = 5)
      
      writeData(wb, "Invoice", "Discount", startRow = row_sum_start + 1, startCol = 1)
      writeData(wb, "Invoice", discount_total_batch, startRow = row_sum_start + 1, startCol = 5)
      addStyle(wb, "Invoice", style_currency, rows = row_sum_start + 1, cols = 5)
      addStyle(wb, "Invoice", style_line_thin, rows = row_sum_start + 1, cols = 1:5, stack = TRUE)
      
      writeData(wb, "Invoice", "Discount total (per batch)", startRow = row_sum_start + 2, startCol = 1)
      writeData(wb, "Invoice", net_total_batch, startRow = row_sum_start + 2, startCol = 5)
      addStyle(wb, "Invoice", style_bold, rows = row_sum_start + 2, cols = 1)
      addStyle(wb, "Invoice", style_grand_total, rows = row_sum_start + 2, cols = 5)
      addStyle(wb, "Invoice", style_currency, rows = row_sum_start + 2, cols = 5)
      
      row_grand <- row_sum_start + 4 
      grand_label <- paste0("Total Project cost (", batches, " ", batch_label, ")")
      writeData(wb, "Invoice", grand_label, startRow = row_grand, startCol = 1)
      writeData(wb, "Invoice", project_total, startRow = row_grand, startCol = 5)
      addStyle(wb, "Invoice", style_grand_total, rows = row_grand, cols = 1)
      addStyle(wb, "Invoice", style_grand_total, rows = row_grand, cols = 5)
      addStyle(wb, "Invoice", style_currency, rows = row_grand, cols = 5)
      
      # Write Footer text
      row_footer <- row_grand + 3
      footer_text <- c(
        "All prices are in AUD and exclude GST.",
        "Cost estimated and quotes are valid for 4 weeks.",
        "Purchase orders and additional price enquires should be directed to Daniela Zalcenstein (zalcenstein.d@wehi.edu.au)."
      )
      writeData(wb, "Invoice", footer_text[1], startRow = row_footer, startCol = 1)
      writeData(wb, "Invoice", footer_text[2], startRow = row_footer + 1, startCol = 1)
      writeData(wb, "Invoice", footer_text[3], startRow = row_footer + 2, startCol = 1)
      addStyle(wb, "Invoice", createStyle(fontSize = 9), rows = row_footer:(row_footer+2), cols = 1)
      
      setColWidths(wb, "Invoice", cols = 1:5, widths = c(30, 15, 30, 10, 20))
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # --- Event: PDF Download Notification ---
  # Replaced the modal with a persistent 'warning' notification.
  # type="warning" sets the color to yellow/orange.
  # duration=60 ensures it stays 60s until closed by the user.
  shinyjs::onclick("dl_pdf", {
    showNotification(
      "Note: The first PDF generation may take a few minutes. Subsequent PDF downloads will be instantaneous.",
      type = "warning",
      duration = 60,
      closeButton = TRUE
    )
  })
  
  # --- Handler: PDF Download ---
  output$dl_pdf <- downloadHandler(
    filename = function() { paste0(input$quote_id, "_Invoice.pdf") },
    content = function(file) {
      req(requireNamespace("tinytex", quietly = TRUE))
      if (!tinytex::is_tinytex()) tinytex::install_tinytex(force = TRUE)
      
      df_invoice <- values$cart
      
      # Recalculate totals for the PDF context
      raw_total_batch <- sum(df_invoice$Unit_Price * df_invoice$Quantity, na.rm = TRUE)
      discount_total_batch <- sum(df_invoice$Disc_Amt, na.rm = TRUE)
      net_total_batch <- raw_total_batch - discount_total_batch
      
      batches <- as.numeric(input$meta_batches)
      project_total <- net_total_batch * batches
      batch_label <- ifelse(batches > 1, "batches", "batch")
      
      # --- Define RMarkdown Template Internally ---
      # This block constructs the .Rmd content as a string directly in code.
      bt <- strrep("`", 3) # Creates backticks for code blocks
      chunk_header <- paste0(bt, "{r setup, include=FALSE}")
      chunk_table  <- paste0(bt, "{r table, results='asis'}")
      chunk_end    <- bt
      
      rmd_content <- paste(
        "---",
        "output: pdf_document",
        "params:", # Define parameters to be passed from Shiny
        "  date: NA",
        "  quote_id: NA",
        "  proj_id: NA",
        "  title: NA",
        "  proj_type: NA",
        "  platform: NA",
        "  n_samples: NA",
        "  n_batches: NA",
        "  samp_per_batch: NA",
        "  aimed_cells: NA",
        "  cart: NA",
        "  str_raw_total: NA",
        "  str_disc_total: NA",
        "  str_net_total: NA",
        "  str_proj_total: NA",
        "  batch_label: NA",
        "geometry: margin=0.8cm", 
        "header-includes:",
        "    - \\usepackage{booktabs}",
        "    - \\usepackage{colortbl}",
        "    - \\usepackage{xcolor}",
        "    - \\usepackage{float}",
        "---",
        "",
        chunk_header,
        "knitr::opts_chunk$set(echo = FALSE)",
        "library(knitr)",
        "library(dplyr)",
        chunk_end,
        "",
        "# **\\textcolor[HTML]{225EA8}{`r params$title`}**",
        "",
        "*Project based cost estimate*",
        "",
        "\\vspace{1em}",
        "",
        "**Date:** `r params$date`  ",
        "**Quote ID:** `r params$quote_id`",
        "",
        "\\vspace{1em}",
        "",
        "**Project ID:** `r params$proj_id`  ",
        "**Project Title:** `r params$title`  ",
        "**Project Type:** `r params$proj_type`  ",
        "**Platform:** `r params$platform`  ",
        "**Total # samples:** `r params$n_samples`  ",
        "**# batches:** `r params$n_batches`  ",
        "**# samples per batch:** `r params$samp_per_batch`  ",
        "**Aimed # cells per sample:** `r params$aimed_cells`",
        "",
        "\\vspace{2em}",
        "",
        "### Cost per batch",
        "",
        chunk_table,
        "tbl_data <- params$cart %>%",
        "  mutate(",
        "    Amount = sprintf(\"$%.2f\", Unit_Price),",
        "    Total_AUD = sprintf(\"$%.2f\", Unit_Price * Quantity),",
        "    Description = ifelse(is.na(Description), \"\", Description)",
        "  ) %>%",
        "  select(Name, Amount, Description, Quantity, Total_AUD)",
        "",
        # Create Latex Table using Kable
        "k <- kable(tbl_data, format = 'latex', col.names = c(\"Item\", \"Amount\", \"Description\", \"Quantity\", \"Total Amount [AUD]\"), align = c(\"l\", \"r\", \"l\", \"c\", \"r\"), booktabs = TRUE)",
        "k <- gsub(\"\\\\\\\\toprule\", \"\", k)",
        "k <- gsub(\"\\\\\\\\bottomrule\", \"\", k)",
        "cat(k)",
        chunk_end,
        "",
        "\\vspace{1em}",
        "",
        # Manually create summary table in Latex
        "\\begin{flushright}",
        "\\begin{tabular}{lr}",
        "Total (per batch) & `r params$str_raw_total` \\\\",
        "Discount & `r params$str_disc_total` \\\\",
        "\\hline",
        "\\textbf{Discount total (per batch)} & \\textbf{`r params$str_net_total`} \\\\",
        "\\vspace{1em} & \\\\",
        "\\textbf{Total Project cost (`r params$n_batches` `r params$batch_label`)} & \\textbf{`r params$str_proj_total`}",
        "\\end{tabular}",
        "\\end{flushright}",
        "",
        "\\vfill",
        "",
        "\\footnotesize",
        "All prices are in AUD and exclude GST.  ",
        "Cost estimated and quotes are valid for 4 weeks.  ",
        "Purchase orders and additional price enquires should be directed to Daniela Zalcenstein (zalcenstein.d@wehi.edu.au).",
        sep = "\n"
      )
      
      temp_rmd <- file.path(tempdir(), "invoice.Rmd")
      writeLines(rmd_content, temp_rmd)
      
      # Pass data to the RMarkdown Params
      params_list <- list(
        date = escape_latex(input$meta_date),
        quote_id = escape_latex(input$quote_id),
        proj_id = escape_latex(input$meta_proj_id),
        title = escape_latex(input$meta_title),
        proj_type = escape_latex(input$project_type),
        platform = escape_latex(input$meta_platform),
        n_samples = input$meta_samples,
        n_batches = batches,
        samp_per_batch = escape_latex(input$meta_samples_batch),
        aimed_cells = escape_latex(input$meta_aimed_cells),
        cart = df_invoice,
        str_raw_total = sprintf("\\$%.2f", raw_total_batch), 
        str_disc_total = sprintf("\\$%.2f", discount_total_batch),
        str_net_total = sprintf("\\$%.2f", net_total_batch),
        str_proj_total = sprintf("\\$%.2f", project_total),
        batch_label = batch_label
      )
      
      # Render the PDF
      tryCatch({
        rmarkdown::render(temp_rmd, output_file = file, params = params_list, envir = new.env(parent = globalenv()))
      }, error = function(e) {
        showNotification(paste("PDF Generation Failed:", e$message), type = "error", duration = NULL)
        print(e)
      })
    }
  )
}

# Run the Application
shinyApp(ui, server)