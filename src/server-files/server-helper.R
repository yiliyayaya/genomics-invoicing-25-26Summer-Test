# --- Helper Function: Latex Escape ---
escape_latex <- function(x) {
  # Function to format strings and check for special characters for LaTeX.
  #
  # Arguments:
  # x(string) - The string to be formatted
  
  if (is.null(x) || is.na(x)) return("NA")
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x) 
  x <- gsub("([#$%&_{}])", "\\\\\\1", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  return(x)
}