structure_quote_page <- function() {
  # Layout structure of main content of Final Quote page.
  
  quote_panel <- nav_panel(
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
  
  return(quote_panel)
}

quote_config_panel <- function() {
  # Layout sidebar discounts and quote configuration structure of Final Quote page.
  
  config_panel <- conditionalPanel(
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
  
  return(config_panel)
}