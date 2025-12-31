setup_pdf_engine <- function(temp_path) {
  # Function to setup the PDF engine for quote generation later.
  #
  # Arguments:
  # temp_path(string) - The temporary directory used to temporarily store the PDF
  
  try({
    temp_warmup <- file.path(temp_path, "warmup.Rmd")
    writeLines("---\noutput: pdf_document\n---\nInit", temp_warmup)
    rmarkdown::render(temp_warmup, output_file = file.path(temp_path, "warmup.pdf"), quiet = TRUE)
  }, silent = TRUE)
}