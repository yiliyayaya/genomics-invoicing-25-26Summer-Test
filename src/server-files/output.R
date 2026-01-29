# Constants
# Metadata Indices
TITLE_IDX <- 1
DATE_IDX <- 2
QUOTE_ID_IDX <- 3
# Column Indices
ITEM_LABEL_COL <- 1
FIRST_COL <- 1
LAST_COL <- 7 # Updated to accommodate Discount % and Discount $ columns
# Excel Row indices
TITLE_SECTION <- 1
DATE_SECTION <- 4
QUOTEID_SECTION <- 5
METADATA_SECTION <- 7
PER_BATCH_HEADING <- 15
MAIN_SECTION_HEADER <- 16

main_output_logic <- function(input, output, cart_data) {
  # Manages the core output logic for spreadsheet templates, Excel quotes, and PDF invoices.
  #
  # Arguments: 
  # input(list) - List of input values from Shiny server function
  # output(list) - List of output values from Shiny server function 
  # cart_data(dataframe) - Dataframe containing selected items/charges from cart
  
  # --- Background Warm-up for PDF Engine ---
  observe({ setup_pdf_engine(tempdir()) })
  
  # --- Handler: Template Download ---
  output$dl_template <- downloadHandler(
    filename = "master_spreadsheet.xlsx",
    content = function(file) {
      file.copy("master_spreadsheet.xlsx", file)
    }
  )
  
  # --- Handler: Excel Download ---
  output$dl_excel <- downloadHandler(
    filename = function() { paste0(input$quote_id, "_Quote.xlsx") },
    content = function(file) {
      meta_data = list(
        title = input$meta_title,
        date = input$meta_date,
        quote_id = input$quote_id,
        project_id = input$meta_proj_id,
        project_type = input$project_type,
        platform = input$platform,
        n_samples = input$meta_samples,
        n_batches = input$meta_batches,
        n_samples_per_batch = input$meta_samples_batch,
        n_aimed_cells_per_sample = input$meta_aimed_cells
      )
      meta_labels <- c(
        "Title", "Date:", "Quote ID:", "Project ID:", "Project Title:", "Project type:",
        "Platform:", "Total # samples:", "# batches:", "# samples per batch:", "Aimed # cells per sample:"
      )
      
      generate_excel_quote(meta_data, meta_labels, cart_data, file)
    }
  )
  
  # --- Handler: PDF Download ---
  output$dl_pdf <- downloadHandler(
    filename = function() { paste0(input$quote_id, "_Invoice.pdf") },
    content = function(file) {
      req(requireNamespace("tinytex", quietly = TRUE))
      generate_pdf_quote(input, cart_data, file)
    }
  )
}

setup_pdf_engine <- function(temp_path) {
  # Pre-warms the PDF engine to reduce latency during the first real PDF generation.
  #
  # Arguments:
  # temp_path(string) - The temporary directory used to temporarily store the PDF
  
  try({
    temp_warmup <- file.path(temp_path, "warmup.Rmd")
    writeLines("---\noutput: pdf_document\n---\nInit", temp_warmup)
    rmarkdown::render(temp_warmup, output_file = file.path(temp_path, "warmup.pdf"), quiet = TRUE)
  }, silent = TRUE)
}

generate_excel_quote <- function(meta_data, meta_label, cart_data, file) {
  # Constructs and formats the Excel workbook for project quotes.
  #
  # Arguments: 
  # meta_data(list) - List of metadata values derived from input 
  # meta_label(list) - List of metadata labels used for quote formatting
  # cart_data(dataframe) - Dataframe containing selected items/charges from cart
  # file(string) - The temporary file path for the output
  
  req(cart_data)
  
  style_header_title <- createStyle(fontSize = 14, fontColour = "#225EA8", textDecoration = "bold")
  style_italic <- createStyle(textDecoration = "italic")
  style_bold <- createStyle(textDecoration = "bold")
  style_currency <- createStyle(numFmt = "$#,##0.00")
  style_line_bold <- createStyle(border = "bottom", borderColour = "black", borderStyle = "medium")
  style_line_thin <- createStyle(border = "bottom", borderColour = "black", borderStyle = "thin")
  style_grand_total <- createStyle(fontSize = 12, textDecoration = "bold")
  
  df_table <- cart_data %>%
    mutate(Total_Amount_AUD = Unit_Price * Quantity) %>%
    select(Name, Unit_Price, Description, Quantity, Disc_Pct, Disc_Amt, Total_Amount_AUD)
  
  raw_total_batch <- sum(df_table$Total_Amount_AUD, na.rm = TRUE)
  discount_total_batch <- sum(cart_data$Disc_Amt, na.rm = TRUE)
  net_total_batch <- raw_total_batch - discount_total_batch
  
  batches <- as.numeric(meta_data$n_batches)
  project_total <- net_total_batch * batches
  batch_label <- ifelse(batches > 1, "batches", "batch")
  
  wb <- createWorkbook()
  addWorksheet(wb, "Invoice")
  showGridLines(wb, "Invoice", showGridLines = FALSE)
  
  # Titles
  write_data_to_excel(wb, "Invoice", meta_data$title, start_row = TITLE_SECTION, start_col = FIRST_COL, text_style = style_header_title)
  write_data_to_excel(wb, "Invoice", "Project based cost estimate", start_row = TITLE_SECTION + 1, start_col = FIRST_COL, text_style = style_italic)
  
  # Date and Quote ID
  write_data_to_excel(wb, "Invoice", paste(meta_label[[DATE_IDX]], meta_data[[DATE_IDX]]), start_row = DATE_SECTION, start_col = FIRST_COL, text_style = style_bold)
  write_data_to_excel(wb, "Invoice", paste(meta_label[[QUOTE_ID_IDX]], meta_data[[QUOTE_ID_IDX]]), start_row = QUOTEID_SECTION, start_col = FIRST_COL, text_style = style_bold)
  
  # Metadata section
  for(i in 4:length(meta_data)) {
    write_data_to_excel(wb, "Invoice", paste(meta_label[[i]], meta_data[[i]]), start_row = METADATA_SECTION + (i-4), start_col = FIRST_COL, text_style = style_bold)
  }
  
  # Main table section
  write_data_to_excel(wb, "Invoice", "Cost per batch", start_row = PER_BATCH_HEADING, start_col = FIRST_COL, text_style = createStyle(fontSize=12, textDecoration="bold"))
  addStyle(wb, "Invoice", style_line_bold, rows = MAIN_SECTION_HEADER, cols = FIRST_COL:LAST_COL, stack = TRUE)
  headers <- c("Item", "Amount", "Description", "Quantity", "Discount %", "Discount $", "Total Amount [AUD]")
  writeData(wb, "Invoice", t(headers), startRow = MAIN_SECTION_HEADER, startCol = ITEM_LABEL_COL, colNames = FALSE)
  addStyle(wb, "Invoice", style_bold, rows = MAIN_SECTION_HEADER, cols = FIRST_COL:LAST_COL)
  addStyle(wb, "Invoice", style_line_thin, rows = MAIN_SECTION_HEADER, cols = FIRST_COL:LAST_COL, stack = TRUE)
  
  if(nrow(df_table) > 0) {
    writeData(wb, "Invoice", df_table, startRow = MAIN_SECTION_HEADER + 1, startCol = ITEM_LABEL_COL, colNames = FALSE)
    data_rows <- (MAIN_SECTION_HEADER + 1):(MAIN_SECTION_HEADER + nrow(df_table))
    addStyle(wb, "Invoice", style_currency, rows = data_rows, cols = 2, stack = TRUE)
    addStyle(wb, "Invoice", style_currency, rows = data_rows, cols = 6, stack = TRUE)
    addStyle(wb, "Invoice", style_currency, rows = data_rows, cols = 7, stack = TRUE)
    last_row <- max(data_rows)
  } else {
    last_row <- MAIN_SECTION_HEADER + 1
  }
  addStyle(wb, "Invoice", style_line_bold, rows = last_row, cols = FIRST_COL:LAST_COL, stack = TRUE)
  
  # Per batch totals
  row_sum_start <- last_row + 1
  write_data_to_excel(wb, "Invoice", "Total (per batch)", start_row = row_sum_start, start_col = FIRST_COL, text_style = style_bold)
  write_data_to_excel(wb, "Invoice", raw_total_batch, start_row = row_sum_start, start_col = LAST_COL, text_style = style_currency)
  writeData(wb, "Invoice", "Discount", startRow = row_sum_start + 1, startCol = FIRST_COL)
  write_data_to_excel(wb, "Invoice", discount_total_batch, start_row = row_sum_start + 1, start_col = LAST_COL, text_style = style_currency)
  addStyle(wb, "Invoice", style_line_thin, rows = row_sum_start + 1, cols = FIRST_COL:LAST_COL, stack = TRUE)
  write_data_to_excel(wb, "Invoice", "Discount total (per batch)", start_row = row_sum_start + 2, start_col = FIRST_COL, text_style = style_bold)
  write_data_to_excel(wb, "Invoice", net_total_batch, start_row = row_sum_start + 2, start_col = LAST_COL, text_style = style_grand_total, text_style2 = style_currency)
  
  # Project total
  row_grand <- row_sum_start + 4 
  grand_label <- paste0("Total Project cost (", batches, " ", batch_label, ")")
  write_data_to_excel(wb, "Invoice", grand_label, start_row = row_grand, start_col = FIRST_COL, text_style = style_grand_total)
  write_data_to_excel(wb, "Invoice", project_total, start_row = row_grand, start_col = LAST_COL, text_style = style_grand_total, text_style2 = style_currency)
  
  # Footer section
  row_footer <- row_grand + 2
  footer_text <- c(
    "All prices are in AUD and exclude GST.",
    "Cost estimated and quotes are valid for 4 weeks.",
    "Purchase orders and additional price enquiries should be directed to Daniela Zalcenstein (zalcenstein.d@wehi.edu.au)."
  )
  for(i in 1:3) {
    write_data_to_excel(wb, "Invoice", footer_text[i], start_row = row_footer + i, start_col = FIRST_COL, text_style = createStyle(fontSize = 9))
  }
  
  setColWidths(wb, "Invoice", cols = FIRST_COL:LAST_COL, widths = c(30, 15, 30, 10, 15, 15, 20))
  saveWorkbook(wb, file, overwrite = TRUE)
}

write_data_to_excel <- function(wb_object, target_sheet, text, start_row, start_col, end_col = NULL, text_style = NULL, text_style2 = NULL) {
  # Utility function to write data to Excel with optional styling.
  #
  # Arguments: 
  # wb_object(Workbook) - Workbook object containing a worksheet
  # target_sheet(string) - The name of the worksheet to write to 
  # text(string) - The text to be written into the worksheet
  # start_row(integer) - The row for text to be written into
  # start_col(integer) - The first column for text to be written into
  # end_col(integer) - The last column for text style to be applied to (Optional)
  # text_style(style) - The 1st style of text to be applied (Optional)
  # text_style2(style) - The 2nd style of text to be applied (Optional)
  
  writeData(wb = wb_object, sheet = target_sheet, x = text, startRow = start_row, startCol = start_col)
  
  # Check for end column argument
  if(is.null(end_col)) {
    end_col_idx <- start_col
  } else {
    end_col_idx <- end_col
  }
  
  # Apply text styles if requested
  if(!is.null(text_style)) {
    addStyle(wb = wb_object, sheet = target_sheet, style = text_style, rows = start_row, cols = start_col:end_col_idx)
  }
  if(!is.null(text_style2)) {
    addStyle(wb = wb_object, sheet = target_sheet, style = text_style2, rows = start_row, cols = start_col:end_col_idx)
  }
}

generate_pdf_quote <- function(input, cart_data, file) {
  # Generates a PDF invoice using R Markdown and LaTeX templates with custom fonts and styles.
  #
  # Arguments: 
  # input(list) - List of input values from Shiny server function 
  # cart_data(dataframe) - Dataframe containing selected items/charges from cart
  # file(string) - The temporary file path for the PDF
  
  raw_total_batch <- sum(cart_data$Unit_Price * cart_data$Quantity, na.rm = TRUE)
  discount_total_batch <- sum(cart_data$Disc_Amt, na.rm = TRUE)
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
    "output:",
    "  pdf_document:",
    "    latex_engine: xelatex",
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
    "    - \\usepackage{graphicx}",
    "    - \\usepackage{tikz}",
    "    - \\usepackage{fontspec}",
    "    - \\IfFontExistsTF{Helvetica Neue}{\\newfontfamily\\headerfont{Helvetica Neue}}{\\newfontfamily\\headerfont{Arial}}",
    "    - \\definecolor{darkgrey}{RGB}{77,77,77}",
    "---",
    "",
    "\\begin{center}",
    "  \\begin{tikzpicture}[remember picture]",
    "    \\node[inner sep=0pt, outer sep=0pt] (img) at (0,0) {\\makebox[\\textwidth][c]{\\includegraphics[width=0.95\\paperwidth]{pdf_header.png}}};",
    "    \\node[anchor=north east, align=left, fill=white, inner sep=2pt, xshift=-0.4cm, yshift=-0.45cm] at (img.north east) {",
    "      \\begin{minipage}{7.5cm}",
    "        \\headerfont\\fontsize{10}{10.4}\\selectfont\\color{darkgrey}",
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
    "\\headerfont\\fontsize{9}{11}\\selectfont\\color{darkgrey}\\bfseries\\noindent",
    "All prices are in AUD and exclude GST. \\\\",
    "Cost estimated and quotes are valid for 4 weeks. \\\\",
    "Purchase orders and additional price enquiries should be directed to Daniela Zalcenstein (zalcenstein.d@wehi.edu.au).",
    sep = "\n"
  )
  
  temp_rmd <- file.path(tempdir(), "invoice.Rmd")
  
  if (file.exists("pdf_header.png")) {
    file.copy("pdf_header.png", file.path(tempdir(), "pdf_header.png"), overwrite = TRUE)
  }
  
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
    cart = cart_data,
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

escape_latex <- function(x) {
  # Escapes special LaTeX characters to prevent rendering errors.
  #
  # Arguments:
  # x(string) - The string to be formatted
  
  if (is.null(x) || is.na(x)) return("NA")
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x) 
  x <- gsub("([#$%&_{}])", "\\\\\\1", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("~", "\\textasciitilde{}", x)
  return(x)
}