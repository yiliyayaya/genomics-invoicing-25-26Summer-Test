process_pricing_logic <- function(file_path) {
  # Main function to read and process raw data then return list of processed data
  # for later use.
  #
  # Arguments:
  # file_path(string) - The string of the master spreadsheet's filepath
  
  # Surcharge processing logic
  raw_surcharge_config <- read_excel(file_path, sheet = 3, col_names = TRUE)
  
  item_logic <- process_items_surcharges(raw_surcharge_config)
  processing_logic <- process_services_surcharges(raw_surcharge_config)
  
  # Price list processing logic
  raw_items <- read_excel(file_path, sheet = 1, col_names = TRUE)
  
  df_items <- process_items_data(raw_items)
  df_item_application_protocol <- get_item_application_protocol(raw_items)
  
  # Service charges processing logic
  raw_services <- read_excel(file_path, sheet = 2, col_names = TRUE)
  
  df_services <- process_services_data(raw_services)
  df_service_application_protocol <- get_service_application_protocol(raw_services)
  
  # Discounts processing logic
  raw_discounts <- read_excel(file_path, sheet = 4, col_names = TRUE)
  
  df_discounts <- process_discounts_data(raw_discounts)
  
  # Return structured list containing all processed data and logic maps
  return(list(items = df_items, services = df_services, logic_proc = processing_logic, logic_item = item_logic, 
              application_protocol_item = df_item_application_protocol, application_protocol_proc = df_service_application_protocol, supplier_discount = df_discounts))
}


process_items_surcharges <- function(raw_surcharges_data) {
  # Reads the raw surcharges data and returns a dataframe containing
  # a formatted version of surcharges on price list items.
  #
  # Arguments: 
  # raw_surcharges_data(dataframe) - The dataframe containing unprocessed surcharge data
  
  # Filter for price list data only
  items_surcharges_df <- raw_surcharges_data %>%
    setNames(c("Type", "Label", "Amount")) %>%
    filter(Type == "PRICE_LIST") %>%
    mutate(Amount = as.numeric(Amount)) %>%
    mutate(Label = as.character(Label))
  
  item_surcharge_list <- list()
  
  # Reads dataframe and creates list of surcharges
  for(i in 1:nrow(items_surcharges_df)) {
    row_data <- items_surcharges_df[i, ]
    
    surcharge_label <- row_data$Label
    surcharge_amount <- row_data$Amount
    
    item_surcharge_list[[surcharge_label]] <- surcharge_amount
  }
  
  return(item_surcharge_list)
}

process_services_surcharges <- function(raw_surcharges_data) {
  # Reads the raw surcharges data and returns a dataframe containing
  # a formatted version of surcharges on processing charges.
  #
  # Arguments: 
  # raw_surcharges_data(dataframe) - The dataframe containing unprocessed surcharge data
  
  # Process raw data
  df_config <- raw_surcharges_data %>%
    setNames(c("Type", "Label", "Amount")) %>%
    mutate(Amount = as.numeric(Amount))
  proc_rows <- df_config %>% 
    filter(Type == "PROCESSING") %>%
    mutate(Cumulative = cumprod(Amount)) # Multipliers stack on top of each other
  
  proc_surcharge_list <- list()
  
  for(i in 1:nrow(proc_rows)) {
    row_data <- proc_rows[i, ]
    
    cumulative_amt <- row_data$Cumulative
    surcharge_label <- row_data$Label
    
    proc_surcharge_list[[surcharge_label]] <- cumulative_amt
  }
  
  return(proc_surcharge_list)
}

process_items_data <- function(raw_items_data) {
  # Reads the raw price list data and returns a dataframe with cleaned data
  #
  # Arguments:
  # raw_items_data(dataframe) - The raw item price list data to be processed
  
  processed_items <- raw_items_data %>%
    select(1:8) %>% 
    setNames(c("Product_Code", "Brand", "Item", "Category", "Description", "Base_Cost", "Add_Cost", "Is_Constant")) %>%
    mutate(
      Base_Cost = as.numeric(Base_Cost),
      Add_Cost = as.numeric(replace_na(Add_Cost, 0)),
      Is_Constant = as.character(Is_Constant),
      # Normalize boolean text to logical TRUE/FALSE
      Is_Constant = case_when(
        toupper(Is_Constant) %in% c("TRUE", "T", "YES", "Y", "1") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(!is.na(Base_Cost)) %>%
    mutate(across(where(is.character), as.character)) 
  
  return(processed_items)
}

get_item_application_protocol <- function(raw_items_data) {
  # Reads the raw price list data and returns a dataframe of item-application relations
  #
  # Arguments:
  # raw_items_data(dataframe) - The raw price item list data to be processed
  
  application_protocol_needed_cols <- c("Brand", "Item", "Application", "Protocol")
  
  item_data <- raw_items_data[, application_protocol_needed_cols]
  item_data$Application[is.na(item_data$Application)] <- "ALL_APPLICATIONS"
  item_data$Protocol[is.na(item_data$Protocol)] <- "ALL_PROTOCOLS"
  item_data <- item_data %>% 
    setNames(c("Brand", "Item", "Application_String", "Protocol_String")) %>%
    mutate(Brand = as.character(Brand),
           Item = as.character(Item),
           Application_String = as.character(replace_na(as.character(Application_String), "ALL_APPLICATIONS")),
           Application = strsplit(Application_String, split = ";", fixed = TRUE),
           Protocol_String = as.character(replace_na(as.character(Protocol_String), "ALL_PROTOCOLS")),
           Protocol = strsplit(Protocol_String, split = ";", fixed = TRUE)) %>%
    select(-Application_String) %>%
    select(-Protocol_String)
  
  return(item_data)
}

process_services_data <- function(raw_services_data) {
  # Reads the raw processing charges data and returns a dataframe with cleaned data
  #
  # Arguments:
  # raw_services_data(dataframe) - The raw processing charges list data to be processed
  
  processed_services <- raw_services_data %>%
    select(1, 3, 4, 5) %>% 
    setNames(c("Group", "Service", "Description", "Base_Price")) %>%
    mutate(Base_Price = as.numeric(Base_Price)) %>%
    filter(!is.na(Base_Price)) %>%
    mutate(across(where(is.character), as.character))
  
  return(processed_services)
}

get_service_application_protocol <- function(raw_services_data) {
  # Reads the raw processing charges data and returns a dataframe of services-application relations
  #
  # Arguments:
  # raw_services_data(dataframe) - The raw processing charges list data to be processed
  
  application_protocol_needed_cols <- c("Service", "Application", "Protocol")
  
  service_data <- raw_services_data[, application_protocol_needed_cols]
  service_data <- service_data %>% 
    setNames(c("Service", "Application_String", "Protocol_String")) %>%
    mutate(Service = as.character(Service),
           Application_String = as.character(replace_na(as.character(Application_String), "ALL_APPLICATIONS")),
           Protocol_String = as.character(replace_na(as.character(Protocol_String), "ALL_PROTOCOLS")),
           Application = strsplit(Application_String, ";", fixed = TRUE),
           Protocol = strsplit(Protocol_String, ";", fixed = TRUE)) %>%
    select(-Application_String) %>%
    select(-Protocol_String)
  
  return(service_data)
}

process_discounts_data <- function(raw_discounts_data) {
  # Reads the raw discounts data and returns a dataframe with cleaned data
  #
  # Arguments:
  # raw_discounts_data(dataframe) - The raw discounts data to be processed
  
  processed_discounts <- raw_discounts_data %>%
    select(1, 2, 3, 4, 5) %>%
    setNames(c("Supplier", "Label", "Amount", "End_Date", "Type")) %>%
    mutate(
      Supplier = as.character(Supplier),
      Label = as.character(Label),
      Amount = as.numeric(Amount),
      End_Date = as.Date(End_Date),
      Type = as.character(tolower(Type))
    ) %>%
    filter(!is.na(Type)) %>%
    filter(!is.na(Amount)) %>%
    filter(Amount > 0) %>%
    mutate(End_Date = replace_na(End_Date, Sys.Date())) %>%
    filter(End_Date >= Sys.Date()) %>%
    select(-End_Date) %>%
    mutate(Formatted_Amount = ifelse(Type == "percentage", 
                                     paste0(Amount * 100, "%"),
                                     paste0("$", Amount))) %>%
    mutate(Display_Text = paste(Supplier, Label, Formatted_Amount, sep = " | ")) %>%
    select(-Formatted_Amount)
    
  return(processed_discounts)
}