source("src/server-files/charges-select.R", local=TRUE)
source("src/server-files/data-processing.R", local=TRUE)
source("src/server-files/final-quote.R", local=TRUE)
source("src/server-files/item-select.R", local=TRUE)
source("src/server-files/output.R", local=TRUE)
source("src/server-files/application-protocol-select.R", local=TRUE)

main_server_logic <- function(input, output, session, values) {
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
  observe({ setup_pdf_engine(tempdir()) })
  
  # --- Event: File Upload ---
  observeEvent(input$master_sheet, {
    req(input$master_sheet)
    
    tryCatch({
      values$data <- process_pricing_logic(input$master_sheet$datapath)
      populate_select_lists(session, values$data)
      
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # --- UI Output: Application/Platform Button Stack ---
  # Generates a vertical list of rectangular buttons with light-to-dark blue gradient
  output$application_button_ui <- renderUI({
    req(values$data$application_protocol_item)
    
    applications <- sort(unique(c(unlist(values$data$application_protocol_item$Application, use.names = FALSE), 
                                  unlist(values$data$application_protocol_proc$Application, use.names = FALSE))))
    applications <- applications[!(applications %in% c("ALL_APPLICATIONS"))]
    application_choices <- c("All", applications)
    
    create_application_select(application_choices)
  })
  
  output$protocol_button_ui <- renderUI({
    req(values$data$application_protocol_item, values$application_select)
    selected_application <- values$application_select
    
    # Filter by applications for possible protocols
    if(selected_application != "All") {
      items_data <- values$data$application_protocol_item %>% 
        rowwise() %>%
        filter(selected_application %in% Application)
      services_data <- values$data$application_protocol_proc %>% 
        rowwise() %>% 
        filter(selected_application %in% Application)
    } else {
      items_data <- values$data$application_protocol_item
      services_data <- values$data$application_protocol_proc
    }
    
    # Create list of protocols to select
    protocols <- sort(unique(c(unlist(items_data$Protocol, use.names = FALSE), 
                               unlist(services_data$Protocol, use.names = FALSE))))
    protocols <- protocols[!(protocols %in% c("ALL_PROTOCOLS"))]
    protocol_choices <- c("All", protocols)
    
    create_protocol_select(protocol_choices)
  })
  
  
  # --- Observer: Handle Application and Protocol Button Clicks ---
  # If 'All' is selected, filter is bypassed and user moves to next step
  observeEvent(input$btn_application_click, {
    req(input$btn_application_click)
    values$application_select <- input$btn_application_click
  })
  
  observeEvent(input$btn_protocol_click, {
    req(values$application_select, input$btn_protocol_click)
    values$protocol_select <- input$btn_protocol_click
    nav_select(id = "nav_tabs", selected = "tab_items")
  })
  
  # --- Output: Multiplier Table ---
  output$multiplier_table <- renderTable({ create_mult_ref_table(values$data, input$project_type) }, 
                                         striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # --- Logic: Calculate Item Prices (Tab 1 Display) ---
  output$table_items_catalog <- renderDT({ create_items_datatable(input, values) })
  
  # --- Event: Add Items to Cart ---
  observe({
    input$add_items_btn
    input$add_items_btn_top
    
    isolate({
      update_cart_items(input, values)
    })
  })
  
  # --- Logic: Calculate Service Prices (Tab 2 Display) ---
  output$table_proc_catalog <- renderDT({ create_services_datatable(input, values) })
  
  # --- Event: Add Services to Cart ---
  observe({
    input$add_proc_btn
    input$add_proc_btn_top
    
    isolate({
      update_cart_services(input, values)
    })
  })
  
  # --- Observer: Recalculate Cart when Project Type Changes ---
  observeEvent(input$project_type, { recalculate_cart(input, values) })
  
  # --- Output: Final Quote Table ---
  output$table_final_quote <- renderDT({ create_quote_datatable(input, values$cart) }, server = FALSE) 
  
  # --- Event: Edit Quote Table Cells ---
  observeEvent(input$table_final_quote_cell_edit, { update_quote_table(input$table_final_quote_cell_edit, values) })
  
  # --- Observer: Apply Supplier Discount ---
  observeEvent(input$apply_supp_disc_btn, { apply_supplier_discount(input, values) })
  
  # --- Observer: Apply Custom Discounts ---
  observeEvent(input$apply_percent_discount, { apply_percentage_discount(input, values) })
  
  observeEvent(input$apply_amount_discount, { apply_amount_discount(input, values) })
  
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
}

populate_select_lists <- function(session, data_list) {
  req(session, data)
  
  item_brand_sorted <- sort(unique(data_list$items$Brand))
  cats_sorted <- sort(unique(data_list$items$Category))
  groups_sorted <- sort(unique(data_list$services$Group))
  supplier_discount_labels <- sort(unique(data_list$supplier_discount$Display_Text))
  
  updateSelectInput(session, "filter_item_brand", choices = c("All", item_brand_sorted))
  updateSelectInput(session, "filter_category", choices = c("All", cats_sorted))
  updateSelectInput(session, "filter_group", choices = c("All", groups_sorted))
  updateSelectInput(session, "supplier_discount_select", choices = c("", supplier_discount_labels))
}