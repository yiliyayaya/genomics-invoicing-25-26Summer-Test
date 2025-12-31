source("src/server-files/charges-select.R")
source("src/server-files/data-processing.R")
source("src/server-files/final-quote.R")
source("src/server-files/item-select.R")
source("src/server-files/output.R")
source("src/server-files/platform-select.R")
source("src/server-files/server-helper.R")

populate_select_lists <- function(session, data_list) {
  req(session, data)
  
  protocols_sorted <- sort(unique(data_list$items$Protocol))
  cats_sorted <- sort(unique(data_list$items$Category))
  groups_sorted <- sort(unique(data_list$services$Group))
  supplier_discount_labels <- sort(unique(data_list$supplier_discount$Display_Text))
  
  updateSelectInput(session, "filter_protocol", choices = c("All", protocols_sorted))
  updateSelectInput(session, "filter_category", choices = c("All", cats_sorted))
  updateSelectInput(session, "filter_group", choices = c("All", groups_sorted))
  updateSelectInput(session, "supplier_discount_select", choices = c("", supplier_discount_labels))
}