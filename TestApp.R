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

ui <- main_ui_features()

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
  observe({ setup_pdf_engine(tempdir()) })
  
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
      
      populate_select_lists(session, values$data)
      
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
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
    
    create_platform_select(choices)
  })
  
  # --- Observer: Handle Platform Button Clicks ---
  # If 'All' is selected, filter is bypassed and user moves to next step
  observeEvent(input$btn_platform_click, {
    values$platform_select <- input$btn_platform_click
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