# ==============================================================================
# MODULE: OUTPUT LOGIC
# DESCRIPTION: Manages file generation and download handlers for Excel and PDF.
# ==============================================================================

# --- Constants: Metadata Indices ---
TITLE_IDX <- 1
DATE_IDX <- 2
QUOTE_ID_IDX <- 3

# --- Constants: Column Indices ---
ITEM_LABEL_COL <- 1
FIRST_COL <- 1
LAST_COL <- 7 # Includes Discount % and Discount $ columns

# --- Constants: Excel Row Layout ---
TITLE_SECTION <- 1
DATE_SECTION <- 4
QUOTEID_SECTION <- 5
METADATA_SECTION <- 7
PER_BATCH_HEADING <- 15
MAIN_SECTION_HEADER <- 16

main_output_logic <- function(input, output, values) {
  # Core logic for handling spreadsheet templates, Excel quotes, and PDF invoices.
  #
  # Arguments:
  #   input      - List of input values from the Shiny server.
  #   output     - List of output values from the Shiny server.
  #   values     - ReactiveValues object containing the live cart data.
  
  # --- PDF Engine Initialization ---
  # Pre-warms the PDF rendering engine to reduce latency for the first user request.
  observe({ setup_pdf_engine(tempdir()) })
  
  # --- Handler: Template Download ---
  # Allows users to download the original master spreadsheet template.
  output$dl_template <- downloadHandler(
    filename = "master_spreadsheet.xlsx",
    content = function(file) {
      file.copy("master_spreadsheet.xlsx", file)
    }
  )
  
  # --- Handler: Excel Quote Download ---
  # Generates a formatted .xlsx quote based on the current cart and metadata.
  output$dl_excel <- downloadHandler(
    filename = function() { paste0(input$quote_id, "_Quote.xlsx") },
    content = function(file) {
      # Access values$cart inside content to capture the latest edits
      current_cart <- values$cart
      
      # Aggregate metadata into a list for processing
      meta_data = list(
        title = input$meta_title,
        date = input$meta_date,
        quote_id = input$quote_id,
        project_id = input$meta_proj_id,
        project_type = input$project_type,
        platform = input$meta_platform,
        n_samples = input$meta_samples,
        n_batches = input$meta_batches,
        n_samples_per_batch = input$meta_samples_batch,
        n_aimed_cells_per_sample = input$meta_aimed_cells
      )
      
      # Define labels for the metadata section in Excel
      meta_labels <- c(
        "Title", "Date:", "Quote ID:", "Project ID:", "Project Title:", "Project type:",
        "Platform:", "Total # samples:", "# batches:", "# samples per batch:", "Aimed # cells per sample:"
      )
      
      generate_excel_quote(meta_data, meta_labels, current_cart, file)
    }
  )
  
  # --- Handler: PDF Invoice Download ---
  # Generates a professional PDF invoice using LaTeX.
  output$dl_pdf <- downloadHandler(
    filename = function() { paste0(input$quote_id, "_Invoice.pdf") },
    content = function(file) {
      req(requireNamespace("tinytex", quietly = TRUE))
      # Access values$cart inside content to capture the latest edits
      current_cart <- values$cart
      generate_pdf_quote(input, current_cart, file)
    }
  )
}

setup_pdf_engine <- function(temp_path) {
  # Initializes the RMarkdown rendering engine in a temporary directory.
  # This helps verify the environment is ready before a user attempts a download.
  try({
    temp_warmup <- file.path(temp_path, "warmup.Rmd")
    writeLines("---\noutput: pdf_document\n---\nInit", temp_warmup)
    rmarkdown::render(temp_warmup, output_file = file.path(temp_path, "warmup.pdf"), quiet = TRUE)
  }, silent = TRUE)
}

generate_excel_quote <- function(meta_data, meta_label, cart_data, file) {
  # Constructs and formats the Excel workbook using the 'openxlsx' package.
  
  req(cart_data)
  
  # --- Define Excel Styles ---
  style_header_title <- createStyle(fontSize = 14, fontColour = "#225EA8", textDecoration = "bold")
  style_italic <- createStyle(textDecoration = "italic")
  style_bold <- createStyle(textDecoration = "bold")
  style_currency <- createStyle(numFmt = "$#,##0.00")
  style_line_bold <- createStyle(border = "bottom", borderColour = "black", borderStyle = "medium")
  style_line_thin <- createStyle(border = "bottom", borderColour = "black", borderStyle = "thin")
  style_grand_total <- createStyle(fontSize = 12, textDecoration = "bold")
  
  # Prepare data table for export using Final_Total which already accounts for discounts
  df_table <- cart_data %>%
    select(Name, Unit_Price, Description, Quantity, Disc_Pct, Disc_Amt, Final_Total)
  
  # Calculate Totals
  raw_total_batch <- sum(cart_data$Unit_Price * cart_data$Quantity, na.rm = TRUE)
  discount_total_batch <- sum(cart_data$Disc_Amt, na.rm = TRUE)
  net_total_batch <- sum(cart_data$Final_Total, na.rm = TRUE)
  
  batches <- as.numeric(meta_data$n_batches)
  project_total <- net_total_batch * batches
  batch_label <- ifelse(batches > 1, "batches", "batch")
  
  # Initialize Workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Invoice")
  showGridLines(wb, "Invoice", showGridLines = FALSE)
  
  # --- Write Metadata Section ---
  write_data_to_excel(wb, "Invoice", meta_data$title, start_row = TITLE_SECTION, start_col = FIRST_COL, text_style = style_header_title)
  write_data_to_excel(wb, "Invoice", "Project based cost estimate", start_row = TITLE_SECTION + 1, start_col = FIRST_COL, text_style = style_italic)
  
  write_data_to_excel(wb, "Invoice", paste(meta_label[[DATE_IDX]], meta_data[[DATE_IDX]]), start_row = DATE_SECTION, start_col = FIRST_COL, text_style = style_bold)
  write_data_to_excel(wb, "Invoice", paste(meta_label[[QUOTE_ID_IDX]], meta_data[[QUOTE_ID_IDX]]), start_row = QUOTEID_SECTION, start_col = FIRST_COL, text_style = style_bold)
  
  for(i in 4:length(meta_data)) {
    write_data_to_excel(wb, "Invoice", paste(meta_label[[i]], meta_data[[i]]), start_row = METADATA_SECTION + (i-4), start_col = FIRST_COL, text_style = style_bold)
  }
  
  # --- Write Main Data Table ---
  write_data_to_excel(wb, "Invoice", "Cost per batch", start_row = PER_BATCH_HEADING, start_col = FIRST_COL, text_style = createStyle(fontSize=12, textDecoration="bold"))
  addStyle(wb, "Invoice", style_line_bold, rows = MAIN_SECTION_HEADER, cols = FIRST_COL:LAST_COL, stack = TRUE)
  
  headers <- c("Item", "Amount", "Description", "Quantity", "Discount %", "Discount $", "Total Amount [AUD]")
  writeData(wb, "Invoice", t(headers), startRow = MAIN_SECTION_HEADER, startCol = ITEM_LABEL_COL, colNames = FALSE)
  addStyle(wb, "Invoice", style_bold, rows = MAIN_SECTION_HEADER, cols = FIRST_COL:LAST_COL)
  addStyle(wb, "Invoice", style_line_thin, rows = MAIN_SECTION_HEADER, cols = FIRST_COL:LAST_COL, stack = TRUE)
  
  if(nrow(df_table) > 0) {
    writeData(wb, "Invoice", df_table, startRow = MAIN_SECTION_HEADER + 1, startCol = ITEM_LABEL_COL, colNames = FALSE)
    data_rows <- (MAIN_SECTION_HEADER + 1):(MAIN_SECTION_HEADER + nrow(df_table))
    
    # Apply currency formatting
    addStyle(wb, "Invoice", style_currency, rows = data_rows, cols = 2, stack = TRUE) # Unit Price
    addStyle(wb, "Invoice", style_currency, rows = data_rows, cols = 6, stack = TRUE) # Discount Amt
    addStyle(wb, "Invoice", style_currency, rows = data_rows, cols = 7, stack = TRUE) # Total
    last_row <- max(data_rows)
  } else {
    last_row <- MAIN_SECTION_HEADER + 1
  }
  addStyle(wb, "Invoice", style_line_bold, rows = last_row, cols = FIRST_COL:LAST_COL, stack = TRUE)
  
  # --- Write Summary Totals ---
  row_sum_start <- last_row + 1
  write_data_to_excel(wb, "Invoice", "Total (per batch)", start_row = row_sum_start, start_col = FIRST_COL, text_style = style_bold)
  write_data_to_excel(wb, "Invoice", raw_total_batch, start_row = row_sum_start, start_col = LAST_COL, text_style = style_currency)
  
  writeData(wb, "Invoice", "Discount", startRow = row_sum_start + 1, startCol = FIRST_COL)
  write_data_to_excel(wb, "Invoice", discount_total_batch, start_row = row_sum_start + 1, start_col = LAST_COL, text_style = style_currency)
  addStyle(wb, "Invoice", style_line_thin, rows = row_sum_start + 1, cols = FIRST_COL:LAST_COL, stack = TRUE)
  
  write_data_to_excel(wb, "Invoice", "Net Total (per batch)", start_row = row_sum_start + 2, start_col = FIRST_COL, text_style = style_bold)
  write_data_to_excel(wb, "Invoice", net_total_batch, start_row = row_sum_start + 2, start_col = LAST_COL, text_style = style_grand_total, text_style2 = style_currency)
  
  # --- Write Grand Total ---
  row_grand <- row_sum_start + 4 
  grand_label <- paste0("Total Project cost (", batches, " ", batch_label, ")")
  write_data_to_excel(wb, "Invoice", grand_label, start_row = row_grand, start_col = FIRST_COL, text_style = style_grand_total)
  write_data_to_excel(wb, "Invoice", project_total, start_row = row_grand, start_col = LAST_COL, text_style = style_grand_total, text_style2 = style_currency)
  
  # --- Footer ---
  row_footer <- row_grand + 2
  footer_text <- c(
    "All prices are in AUD and exclude GST.",
    "Cost estimated and quotes are valid for 4 weeks.",
    "Purchase orders and additional price enquiries should be directed to Daniela Zalcenstein (zalcenstein.d@wehi.edu.au)."
  )
  for(i in 1:3) {
    write_data_to_excel(wb, "Invoice", footer_text[i], start_row = row_footer + i, start_col = FIRST_COL, text_style = createStyle(fontSize = 9))
  }
  
  # Adjust column widths for readability
  setColWidths(wb, "Invoice", cols = FIRST_COL:LAST_COL, widths = c(30, 15, 30, 10, 15, 15, 20))
  saveWorkbook(wb, file, overwrite = TRUE)
}

write_data_to_excel <- function(wb_object, target_sheet, text, start_row, start_col, end_col = NULL, text_style = NULL, text_style2 = NULL) {
  # Helper function to write content and apply up to two styles to a cell or range.
  
  writeData(wb = wb_object, sheet = target_sheet, x = text, startRow = start_row, startCol = start_col)
  
  if(is.null(end_col)) {
    end_col_idx <- start_col
  } else {
    end_col_idx <- end_col
  }
  
  if(!is.null(text_style)) {
    addStyle(wb = wb_object, sheet = target_sheet, style = text_style, rows = start_row, cols = start_col:end_col_idx)
  }
  if(!is.null(text_style2)) {
    addStyle(wb = wb_object, sheet = target_sheet, style = text_style2, rows = start_row, cols = start_col:end_col_idx)
  }
}

generate_pdf_quote <- function(input, cart_data, file) {
  # Generates a PDF invoice.
  # Utilizes 'pdflatex' and standard 'helvet' fonts to ensure compatibility with Linux servers (shinyapps.io).
  
  # --- Calculate Totals using net values ---
  raw_total_batch <- sum(cart_data$Unit_Price * cart_data$Quantity, na.rm = TRUE)
  discount_total_batch <- sum(cart_data$Disc_Amt, na.rm = TRUE)
  net_total_batch <- raw_total_batch - discount_total_batch
  
  batches <- as.numeric(input$meta_batches)
  project_total <- net_total_batch * batches
  batch_label <- ifelse(batches > 1, "batches", "batch")
  
  # --- Construct RMarkdown Content ---
  bt <- strrep("`", 3) 
  chunk_header <- paste0(bt, "{r setup, include=FALSE}")
  chunk_table  <- paste0(bt, "{r table, results='asis'}")
  chunk_end    <- bt
  
  # Defines the RMarkdown structure with LaTeX header includes.
  rmd_content <- paste(
    "---",
    "output:",
    "  pdf_document:",
    "    latex_engine: pdflatex", 
    "params:",
    "  date: NA", "  quote_id: NA", "  proj_id: NA", "  title: NA", "  proj_type: NA",
    "  platform: NA", "  n_samples: NA", "  n_batches: NA", "  samp_per_batch: NA",
    "  aimed_cells: NA", "  cart: NA", "  str_raw_total: NA", "  str_disc_total: NA",
    "  str_net_total: NA", "  str_proj_total: NA", "  batch_label: NA",
    "geometry: margin=0.8cm", 
    "header-includes:",
    "    - \\usepackage{booktabs}",
    "    - \\usepackage{colortbl}",
    "    - \\usepackage{xcolor}",
    "    - \\usepackage{float}",
    "    - \\usepackage{graphicx}",
    "    - \\usepackage{tikz}",
    "    - \\usepackage[scaled]{helvet}",  
    "    - \\renewcommand{\\familydefault}{\\sfdefault}",
    "    - \\newcommand{\\headerfont}{\\sffamily}",
    "    - \\definecolor{darkgrey}{RGB}{77,77,77}",
    "---",
    "",
    "\\begin{center}",
    "  \\begin{tikzpicture}[remember picture]",
    "    \\node[inner sep=0pt, outer sep=0pt] (img) at (0,0) {\\makebox[\\textwidth][c]{\\includegraphics[width=0.95\\paperwidth]{pdf_header.png}}};",
    "    \\node[anchor=north east, align=left, fill=white, inner sep=3pt, xshift=1.65cm, yshift=-0.45cm] at (img.north east) {",
    "      \\begin{minipage}{9.5cm}",
    "        \\raggedright",
    "        \\headerfont\\fontsize{9}{10}\\selectfont\\color{darkgrey}",
    "        \\textbf{The Walter and Eliza Hall Institute of Medical Research} \\\\",
    "        ABN 12 004 251 423 \\\\[0.6em]",
    "        1G Royal Parade Parkville Victoria 3052 Australia \\\\",
    "        T +61 3 9345 2555 F +61 3 9347 0852 \\\\",
    "        \\textbf{www.wehi.edu.au}",
    "      \\end{minipage}",
    "    };",
    "  \\end{tikzpicture}",
    "\\end{center}",
    "",
    "\\vspace{-2em}",
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
    "\\vspace{0.5em}",
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
    "    Total_AUD = sprintf(\"$%.2f\", Final_Total),",
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
    "\\textbf{Net Total (per batch)} & \\textbf{`r params$str_net_total`} \\\\",
    "\\vspace{1em} & \\\\",
    "\\textbf{Total Project cost (`r params$n_batches` `r params$batch_label`)} & \\textbf{`r params$str_proj_total`}",
    "\\end{tabular}",
    "\\end{flushright}",
    "",
    "\\vfill",
    "",
    "\\headerfont\\fontsize{9}{11}\\selectfont\\color{darkgrey}\\bfseries\\noindent",
    "All prices are in AUD and exclude GST. \\\\",
    "Cost estimated and quotes are valid for 4 weeks. \\\\",
    "Purchase orders and additional price enquiries should be directed to Daniela Zalcenstein (zalcenstein.d@wehi.edu.au).",
    sep = "\n"
  )
  
  # Prepare the temporary Rmd file
  temp_rmd <- file.path(tempdir(), "invoice.Rmd")
  
  # Ensure the header image is available in the temp directory
  if (file.exists("pdf_header.png")) {
    file.copy("pdf_header.png", file.path(tempdir(), "pdf_header.png"), overwrite = TRUE)
  }
  
  writeLines(rmd_content, temp_rmd)
  
  # Prepare parameters for the RMarkdown template
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
    cart = cart_data,
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

escape_latex <- function(x) {
  # Sanitizes input strings to escape special LaTeX characters.
  
  if (is.null(x) || is.na(x)) return("NA")
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x) 
  x <- gsub("([#$%&_{}])", "\\\\\\1", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("~", "\\textasciitilde{}", x)
  return(x)
}