source("src/server.R")
source("src/ui.R")
source("requirements/requirements.R")
# ==============================================================================
# SECTION 1: PACKAGE MANAGEMENT & SETUP
# ==============================================================================

# This is kept here for demonstration purposes, this should be removed after
# it is hosted on WEHI servers and replaced with the check_requirements function
run_setup()

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
library(RColorBrewer) # For professional color palettes

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
      /* Layout for platform vertical button stack with tight spacing and increased length */
      .platform-btn-container {
        display: flex;
        flex-direction: column;
        gap: 4px !important;
        padding: 10px 15px !important;
      }
      .platform-action-btn {
        width: 800px !important; /* Increased length of the buttons as requested */
        text-align: left;
        font-weight: bold;
        border: none;
        padding: 8px 25px !important;
        border-radius: 4px !important;
        transition: transform 0.1s, opacity 0.2s;
        box-shadow: 1px 1px 3px rgba(0,0,0,0.1);
      }
      .platform-action-btn:hover {
        transform: scale(1.01);
        opacity: 0.9;
      }
    "))
  ),
  
  # --- Sidebar Configuration ---
  sidebar = sidebar(
    width = 350,
    title = NULL,
    
    # 0. Helper: Download Template Button (For users without the source file)
    div(style = "margin-bottom: 5px;",
        downloadButton("dl_template", "Download Template (.xlsx)", class = "btn-outline-primary w-100 btn-sm")
    ),
    
    # 1. Global Input: File Upload (Always Visible)
    fileInput("master_sheet", "1. Upload Master Spreadsheet (.xlsx)", accept = ".xlsx"),
    
    # 2. Tab 2 Specific: Filter Items
    conditionalPanel(
      condition = "input.nav_tabs == 'tab_items'",
      h5("Filter Items"),
      selectInput("filter_protocol", "Filter Protocol", choices = "All", selectize = TRUE), 
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
    conditionalPanel(
      condition = "input.nav_tabs == 'tab_quote'",
      h5("Quote Configuration"),
      
      selectInput("project_type", "2. Project Type", 
                  choices = c("", "Internal", "Ext.Collaborative", "Ext.RSA", "Commercial")),
      
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
        # Vertical button stack generated dynamically
        uiOutput("platform_button_ui")
      )
    ),
    
    # Tab 2: Catalog for Consumables
    nav_panel(
      title = "2. Select Items",
      value = "tab_items",
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span("Items Catalog (Consumables)"),
              actionButton("add_items_btn_top", "Add Selected to Quote", class = "btn-success btn-sm")
          )
        ),
        DTOutput("table_items_catalog")
      )
    ),
    
    # Tab 3: Catalog for Services
    nav_panel(
      title = "3. Select Processing",
      value = "tab_processing",
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span("Processing Services Catalog"),
              actionButton("add_proc_btn_top", "Add Selected to Quote", class = "btn-success btn-sm")
          )
        ),
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
          conditionalPanel(
            condition = "input.project_type !== ''",
            
            div(class = "d-flex justify-content-between align-items-center",
                div(actionButton("remove_row_btn", "Remove Selected Row", class = "btn-danger btn-sm", icon = icon("trash"))),
                div(downloadButton("dl_excel", "Download .xlsx", class = "btn-secondary btn-sm"),
                    downloadButton("dl_pdf", "Download PDF Invoice", class = "btn-primary btn-sm"))
            ),
            hr(),
            DTOutput("table_final_quote") 
          ),
          
          conditionalPanel(
            condition = "input.project_type == ''",
            div(class = "alert alert-warning", 
                "Please select a 'Project Type' in the sidebar to generate the quote.")
          )
        ),
        card_footer(
          div(style = "text-align: right; font-size: 1.2rem; font-weight: bold;",
              textOutput("grand_total_display") 
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
  
  # --- Background Warm-up for PDF Engine ---
  observe({
    try({
      temp_warmup <- file.path(tempdir(), "warmup.Rmd")
      writeLines("---\noutput: pdf_document\n---\nInit", temp_warmup)
      rmarkdown::render(temp_warmup, output_file = file.path(tempdir(), "warmup.pdf"), quiet = TRUE)
    }, silent = TRUE)
  })
  
  # --- Helper Function: Latex Escape ---
  escape_latex <- function(x) {
    if (is.null(x) || is.na(x)) return("NA")
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x) 
    x <- gsub("([#$%&_{}])", "\\\\\\1", x)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
    x <- gsub("~", "\\\\textasciitilde{}", x)
    return(x)
  }
  
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
  
  # --- Event: File Upload ---
  observeEvent(input$master_sheet, {
    req(input$master_sheet)
    tryCatch({
      values$data <- process_pricing_logic(input$master_sheet$datapath)
      
      protocols_sorted <- sort(unique(values$data$items$Protocol))
      cats_sorted <- sort(unique(values$data$items$Category))
      groups_sorted <- sort(unique(values$data$services$Group))
      supplier_discount_labels <- sort(unique(values$data$supplier_discount$Display_Text))
      
      updateSelectInput(session, "filter_protocol", choices = c("All", protocols_sorted))
      updateSelectInput(session, "filter_category", choices = c("All", cats_sorted))
      updateSelectInput(session, "filter_group", choices = c("All", groups_sorted))
      updateSelectInput(session, "supplier_discount_select", choices = c("", supplier_discount_labels))
      
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
      print(e)
    })
  })
  
  # --- UI Output: Platform Button Stack ---
  # Generates a vertical list of rectangular buttons with light-to-dark blue gradient
  output$platform_button_ui <- renderUI({
    req(values$data)
    
    platforms <- sort(unique(c(unlist(values$data$platform_item$Platform, use.names = FALSE), 
                               unlist(values$data$platform_proc$Platform, use.names = FALSE))))
    platforms <- platforms[!(platforms %in% c("ALL_PLATFORMS"))]
    choices <- c("All", platforms)
    
    # Use professional Blue palette for the gradient
    n_colors <- length(choices)
    pal <- colorRampPalette(brewer.pal(9, "Blues"))(n_colors + 1)
    
    div(class = "platform-btn-container",
        lapply(seq_along(choices), function(i) {
          btn_id <- paste0("btn_platform_", gsub("[^a-zA-Z0-9]", "_", choices[i]))
          onclick_code <- sprintf("Shiny.setInputValue('btn_platform_click', '%s', {priority: 'event'});", choices[i])
          
          # Gradient progresses through shades of blue
          bg_color <- pal[i]
          text_color <- ifelse(i > n_colors/2 + 1, "white", "black")
          
          tags$button(
            choices[i],
            id = btn_id,
            class = "platform-action-btn",
            style = sprintf("background-color: %s; color: %s;", bg_color, text_color),
            onclick = onclick_code
          )
        })
    )
  })
  
  # --- Observer: Handle Platform Button Clicks ---
  # If 'All' is selected, filter is bypassed and user moves to next step
  observeEvent(input$btn_platform_click, {
    values$platform_select <- input$btn_platform_click
    nav_select(id = "nav_tabs", selected = "tab_items")
  })
  
  # --- Output: Multiplier Table ---
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
  output$table_items_catalog <- renderDT({
    req(values$data)
    df <- values$data$items
    
    if(input$filter_protocol != "All") df <- df %>% filter(Protocol == input$filter_protocol)
    if(input$filter_category != "All") df <- df %>% filter(Category == input$filter_category)
    
    # Filter only if a specific platform (not 'All') is selected
    if(values$platform_select != "All") {
      common_items <- values$data$platform_item %>% 
        rowwise() %>%
        filter(any(Platform %in% values$platform_select) | any(Platform == "ALL_PLATFORMS")) %>%
        ungroup()
      df <- df %>% semi_join(common_items, by=c("Item", "Protocol"))
    } 
    
    m_int <- values$data$logic_item[["Internal"]]
    m_ext <- values$data$logic_item[["External"]]
    
    df_display <- df %>% 
      mutate(
        Price_Internal = ifelse(Is_Constant, Base_Cost + Add_Cost, (Base_Cost * m_int) + Add_Cost),
        Price_External = ifelse(Is_Constant, Base_Cost + Add_Cost, (Base_Cost * m_ext) + Add_Cost)
      ) %>%
      select(Product_Code, Protocol, Item, Description, Price_Internal, Price_External)
    
    datatable(
      df_display,
      selection = "multiple",
      options = list(pageLength = 10),
      colnames = c("Code", "Protocol", "Item", "Description", "Internal Price", "External Price")
    ) %>%
      formatCurrency(c("Price_Internal", "Price_External"))
  })
  
  # --- Event: Add Items to Cart ---
  observe({
    input$add_items_btn
    input$add_items_btn_top
    
    isolate({
      if (is.null(input$table_items_catalog_rows_selected)) return()
      
      df_full <- values$data$items
      if(input$filter_protocol != "All") df_full <- df_full %>% filter(Protocol == input$filter_protocol)
      if(input$filter_category != "All") df_full <- df_full %>% filter(Category == input$filter_category)
      
      if(values$platform_select != "All") {
        common_items <- values$data$platform_item %>% 
          rowwise() %>%
          filter(any(Platform %in% values$platform_select) | any(Platform == "ALL_PLATFORMS")) %>%
          ungroup()
        df_full <- df_full %>% semi_join(common_items, by=c("Item", "Protocol"))
      } 
      
      selected_indices <- input$table_items_catalog_rows_selected
      items_to_add <- df_full[selected_indices, ]
      existing_codes <- values$cart$Product_Code
      items_to_add_unique <- items_to_add %>% filter(!Product_Code %in% existing_codes)
      
      if (nrow(items_to_add_unique) == 0) return()
      
      new_entries <- items_to_add_unique %>%
        mutate(
          Cart_ID = paste0("I-", as.numeric(Sys.time()), "-", row_number()),
          Type = "Item",
          Quantity = 1, Disc_Pct = 0, Disc_Amt = 0,
          Base_Ref = Base_Cost,
          Add_Ref = Add_Cost,
          Unit_Price = 0, Final_Total = 0
        ) %>%
        select(Cart_ID, Product_Code, Name = Item, Description, Type, Category, 
               Base_Ref, Add_Ref, Is_Constant, Unit_Price, Quantity, Disc_Pct, Disc_Amt, Final_Total)
      
      values$cart <- bind_rows(values$cart, new_entries) %>% as.data.frame()
      showNotification(paste(nrow(new_entries), "new items added."), type = "message")
    })
  })
  
  # --- Logic: Calculate Service Prices (Tab 2 Display) ---
  output$table_proc_catalog <- renderDT({
    req(values$data)
    df <- values$data$services
    if(input$filter_group != "All") df <- df %>% filter(Group == input$filter_group)
    
    # Filter only if a specific platform (not 'All') is selected
    if(values$platform_select != "All") {
      common_services <- values$data$platform_proc %>% 
        rowwise() %>%
        filter(any(Platform %in% values$platform_select) | any(Platform == "ALL_PLATFORMS")) %>%
        ungroup()
      df <- df[df$Service %in% common_services$Service, ]
    }
    
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
  observe({
    input$add_proc_btn
    input$add_proc_btn_top
    
    isolate({
      if (is.null(input$table_proc_catalog_rows_selected)) return()
      
      df_full <- values$data$services
      if(input$filter_group != "All") df_full <- df_full %>% filter(Group == input$filter_group)
      
      if(values$platform_select != "All") {
        common_services <- values$data$platform_proc %>% 
          rowwise() %>%
          filter(any(Platform %in% values$platform_select) | any(Platform == "ALL_PLATFORMS")) %>%
          ungroup()
        df_full <- df_full[df_full$Service %in% common_services$Service, ]
      }
      
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
  })
  
  # --- Observer: Recalculate Cart when Project Type Changes ---
  observeEvent(input$project_type, {
    req(values$data, nrow(values$cart) > 0)
    if(input$project_type == "") return()
    
    ptype <- input$project_type
    rate_type_item <- if(ptype == "Internal") "Internal" else "External"
    mult_item <- values$data$logic_item[[rate_type_item]]
    mult_proc <- values$data$logic_proc[[ptype]]
    
    if(is.null(mult_proc)) mult_proc <- 1 
    
    values$cart <- values$cart %>%
      mutate(
        Unit_Price = case_when(
          Type == "Item" & Is_Constant ~ Base_Ref + Add_Ref,
          Type == "Item" & !Is_Constant ~ (Base_Ref * mult_item) + Add_Ref,
          Type == "Processing" ~ Base_Ref * mult_proc,
          TRUE ~ 0
        ),
        Gross = Unit_Price * Quantity,
        Disc_Amt = Gross * (Disc_Pct / 100),
        Final_Total = Gross - Disc_Amt
      ) %>%
      select(-Gross) 
  })
  
  # --- Output: Final Quote Table ---
  output$table_final_quote <- renderDT({
    req(input$project_type) 
    
    display_df <- values$cart %>% 
      select(Product_Code, Name, Description, Type, Unit_Price, Quantity, Disc_Pct, Disc_Amt, Final_Total) %>%
      as.data.frame() 
    
    datatable(display_df,
              selection = "multiple",
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
  observeEvent(input$table_final_quote_cell_edit, {
    info <- input$table_final_quote_cell_edit
    row_idx <- info$row
    col_idx <- info$col 
    new_val <- as.numeric(info$value)
    
    current_row <- values$cart[row_idx, ]
    price <- current_row$Unit_Price
    qty   <- current_row$Quantity
    
    if (col_idx == 5) {
      qty <- new_val
      values$cart$Quantity[row_idx] <- qty
      pct <- values$cart$Disc_Pct[row_idx]
      gross <- price * qty
      disc_amt <- gross * (pct / 100)
      values$cart$Disc_Amt[row_idx] <- disc_amt
      values$cart$Final_Total[row_idx] <- gross - disc_amt
    } else if (col_idx == 6) { 
      pct <- new_val
      values$cart$Disc_Pct[row_idx] <- pct
      gross <- price * qty
      disc_amt <- gross * (pct / 100)
      values$cart$Disc_Amt[row_idx] <- disc_amt
      values$cart$Final_Total[row_idx] <- gross - disc_amt
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
  output$dl_template <- downloadHandler(
    filename = "master_spreadsheet_25_26_summer.xlsx",
    content = function(file) {
      file.copy("master_spreadsheet_25_26_summer.xlsx", file)
    }
  )
  
  # --- Handler: Excel Download ---
  output$dl_excel <- downloadHandler(
    filename = function() { paste0(input$quote_id, "_Quote.xlsx") },
    content = function(file) {
      df_cart <- values$cart
      df_table <- df_cart %>%
        mutate(Total_Amount_AUD = Unit_Price * Quantity) %>%
        select(Name, Unit_Price, Description, Quantity, Total_Amount_AUD) 
      
      raw_total_batch <- sum(df_table$Total_Amount_AUD, na.rm = TRUE)
      discount_total_batch <- sum(df_cart$Disc_Amt, na.rm = TRUE)
      net_total_batch <- raw_total_batch - discount_total_batch
      
      batches <- as.numeric(input$meta_batches)
      project_total <- net_total_batch * batches
      batch_label <- ifelse(batches > 1, "batches", "batch")
      
      wb <- createWorkbook()
      addWorksheet(wb, "Invoice")
      showGridLines(wb, "Invoice", showGridLines = FALSE) 
      
      style_header_title <- createStyle(fontSize = 14, fontColour = "#225EA8", textDecoration = "bold")
      style_italic <- createStyle(textDecoration = "italic")
      style_bold <- createStyle(textDecoration = "bold")
      style_currency <- createStyle(numFmt = "$#,##0.00")
      style_line_bold <- createStyle(border = "bottom", borderColour = "black", borderStyle = "medium")
      style_line_thin <- createStyle(border = "bottom", borderColour = "black", borderStyle = "thin")
      style_grand_total <- createStyle(fontSize = 12, textDecoration = "bold")
      
      writeData(wb, "Invoice", input$meta_title, startRow = 1, startCol = 1)
      addStyle(wb, "Invoice", style_header_title, rows = 1, cols = 1)
      writeData(wb, "Invoice", "Project based cost estimate", startRow = 2, startCol = 1)
      addStyle(wb, "Invoice", style_italic, rows = 2, cols = 1)
      
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
      writeData(wb, "Invoice", paste(meta_fields[[1]][1], meta_fields[[1]][2]), startRow = row_ptr, startCol = 1); addStyle(wb, "Invoice", style_bold, rows = row_ptr, cols = 1); row_ptr <- row_ptr + 1
      writeData(wb, "Invoice", paste(meta_fields[[2]][1], meta_fields[[2]][2]), startRow = row_ptr, startCol = 1); addStyle(wb, "Invoice", style_bold, rows = row_ptr, cols = 1); row_ptr <- row_ptr + 2
      
      for(i in 3:length(meta_fields)) {
        writeData(wb, "Invoice", paste(meta_fields[[i]][1], meta_fields[[i]][2]), startRow = row_ptr, startCol = 1)
        addStyle(wb, "Invoice", style_bold, rows = row_ptr, cols = 1)
        row_ptr <- row_ptr + 1
      }
      
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
  
  # --- Handler: PDF Download ---
  output$dl_pdf <- downloadHandler(
    filename = function() { paste0(input$quote_id, "_Invoice.pdf") },
    content = function(file) {
      req(requireNamespace("tinytex", quietly = TRUE))
      if (!tinytex::is_tinytex()) tinytex::install_tinytex(force = TRUE)
      
      df_invoice <- values$cart
      raw_total_batch <- sum(df_invoice$Unit_Price * df_invoice$Quantity, na.rm = TRUE)
      discount_total_batch <- sum(df_invoice$Disc_Amt, na.rm = TRUE)
      net_total_batch <- raw_total_batch - discount_total_batch
      
      batches <- as.numeric(input$meta_batches)
      project_total <- net_total_batch * batches
      batch_label <- ifelse(batches > 1, "batches", "batch")
      
      bt <- strrep("`", 3) 
      chunk_header <- paste0(bt, "{r setup, include=FALSE}")
      chunk_table  <- paste0(bt, "{r table, results='asis'}")
      chunk_end    <- bt
      
      rmd_content <- paste(
        "---",
        "output: pdf_document",
        "params:",
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
        "k <- kable(tbl_data, format = 'latex', col.names = c(\"Item\", \"Amount\", \"Description\", \"Quantity\", \"Total Amount [AUD]\"), align = c(\"l\", \"r\", \"l\", \"c\", \"r\"), booktabs = TRUE)",
        "k <- gsub(\"\\\\\\\\toprule\", \"\", k)",
        "k <- gsub(\"\\\\\\\\bottomrule\", \"\", k)",
        "cat(k)",
        chunk_end,
        "",
        "\\vspace{1em}",
        "",
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