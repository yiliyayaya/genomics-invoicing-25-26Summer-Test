structure_quote_page <- function() {
  # Layout structure of main content of Final Quote page.
  
  quote_panel <- nav_panel(
    title = "4. Final Quote",
    value = "tab_quote",
    card(
      full_screen = TRUE, 
      card_header(
        div(class = "d-flex flex-column",
            span("Review & Edit Quote", style = "font-weight: bold; margin-bottom: 10px;"),
            div(class = "d-flex justify-content-between align-items-center",
                div(actionButton("remove_row_btn", "Remove Selected Row", class = "btn-danger btn-sm", icon = icon("trash"))),
                div(class = "d-flex gap-2",
                    downloadButton("dl_excel", "Download .xlsx", class = "btn-secondary btn-sm"),
                    downloadButton("dl_pdf", "Download PDF Invoice", class = "btn-primary btn-sm")
                )
            )
        )
      ),
      card_body(
        # Check if project configuration is complete
        conditionalPanel(
          condition = "input.item_surcharge_type !== '' && input.services_surcharge_type !== ''",
          DTOutput("table_final_quote") 
        ),
        
        conditionalPanel(
          condition = "input.item_surcharge_type == '' || input.services_surcharge_type == ''",
          div(class = "alert alert-warning", 
              "Please configure Surcharges in Tab 1 to generate the quote.")
        )
      ),
      card_footer(
        div(style = "text-align: right; font-size: 1.2rem; font-weight: bold;",
            textOutput("grand_total_display") 
        )
      )
    )
  )
  
  return(quote_panel)
}

quote_config_panel <- function() {
  # Layout sidebar for Surcharges (Tab 1) and Discounts/Metadata (Tab 4).
  
  # --- Sidebar Content for Tab 1 (Surcharge Configuration) ---
  config_tab1 <- conditionalPanel(
    condition = "input.nav_tabs == 'tab_application'",
    # Use label styling to match the fileInput header size and weight
    # Reduced margin-bottom to bring inputs closer
    tags$label("2. Select Quote Configuration", class = "control-label", style = "font-weight: bold; margin-bottom: 0px; margin-top: 10px;"),
    
    selectInput("item_surcharge_type", "Items Surcharge", choices = c("")),
    selectInput("services_surcharge_type", "Services Surcharge", choices = c("")),
    
    div(style = "margin-bottom: 10px; padding-bottom: 0px;", 
        strong("Current Multipliers:"),
        tableOutput("multiplier_table") 
    )
  )
  
  # --- Sidebar Content for Tab 4 (Discounts & Metadata) ---
  config_tab4 <- conditionalPanel(
    condition = "input.nav_tabs == 'tab_quote'",
    h5("Quote Discounts & Metadata"),
    
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
  
  # Return both panels wrapped in a tagList to render them sequentially in the sidebar
  return(tagList(config_tab1, config_tab4))
}