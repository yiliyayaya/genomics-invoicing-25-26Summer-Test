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
  df_platform_item <- get_item_platform_data(raw_items)
  
  # Service charges processing logic
  raw_services <- read_excel(file_path, sheet = 2, col_names = TRUE)
  
  df_services <- process_services_data(raw_services)
  df_platform_process <- get_services_platform_data(raw_services)
  
  # Discounts processing logic
  raw_discounts <- read_excel(file_path, sheet = 4, col_names = TRUE)
  
  df_discounts <- process_discounts_data(raw_discounts)
  
  # Return structured list containing all processed data and logic maps
  return(list(items = df_items, services = df_services, logic_proc = processing_logic, logic_item = item_logic, 
              platform_item = df_platform_item, platform_proc = df_platform_process, supplier_discount = df_discounts))
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
    select(1, 3) %>% 
    setNames(c("Type", "Amount")) %>%
    mutate(Amount = as.numeric(Amount))
  proc_rows <- df_config %>% 
    filter(Type == "PROCESSING") %>%
    mutate(Cumulative = cumprod(Amount)) # Multipliers stack on top of each other
  
  # Helper function to safely retrieve cumulative value by row index
  get_proc_val <- function(idx) {
    if(nrow(proc_rows) >= idx) return(proc_rows$Cumulative[idx]) else return(1)
  }
  
  return(list("Internal" = get_proc_val(1), # Corresponds to Row 1
    "Ext.Collaborative" = get_proc_val(2), # Corresponds to Row 2
    "Ext.RSA"            = get_proc_val(3), # Corresponds to Row 3
    "Commercial"        = get_proc_val(4)  # Corresponds to Row 4
  ))
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

get_item_platform_data <- function(raw_items_data) {
  # Reads the raw price list data and returns a dataframe of item-platform relations
  #
  # Arguments:
  # raw_items_data(dataframe) - The raw price item list data to be processed
  
  item_platform_needed_cols <- c("Brand", "Item", "Platform")
  
  platform_item_data <- raw_items_data[, item_platform_needed_cols]
  platform_item_data$Platform[is.na(platform_item_data$Platform)] <- "ALL_PLATFORMS"
  platform_item_data <- platform_item_data %>% 
    setNames(c("Brand", "Item", "Platform_String")) %>%
    mutate(Brand = as.character(Brand),
           Item = as.character(Item),
           Platform_String = as.character(Platform_String),
           Platform = strsplit(Platform_String, split = ";", fixed = TRUE)) %>%
    select(-Platform_String)
  
  return(platform_item_data)
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

get_services_platform_data <- function(raw_services_data) {
  # Reads the raw processing charges data and returns a dataframe of services-platform relations
  #
  # Arguments:
  # raw_services_data(dataframe) - The raw processing charges list data to be processed
  
  service_platform_needed_col <- c("Service", "Platform")
  
  platform_service_data <- raw_services_data[, service_platform_needed_col]
  platform_service_data <- platform_service_data %>% 
    setNames(c("Service", "Platform_String")) %>%
    mutate(Service = as.character(Service),
           Platform_String = as.character(Platform_String),
           Platform = strsplit(Platform_String, ";", fixed = TRUE)) %>%
    select(-Platform_String)
  
  return(platform_service_data)
}

process_discounts_data <- function(raw_discounts_data) {
  # Reads the raw discounts data and returns a dataframe with cleaned data
  #
  # Arguments:
  # raw_discounts_data(dataframe) - The raw discounts data to be processed
  
  processed_discounts <- raw_discounts_data %>%
    setNames(c("Supplier", "Label", "Amount", "End_Date")) %>%
    mutate(
      Supplier = as.character(Supplier),
      Label = as.character(Label),
      Amount = as.numeric(Amount),
      End_Date = as.Date(End_Date)
    ) %>%
    filter(!is.na(Amount)) %>%
    mutate(End_Date = replace_na(End_Date, Sys.Date())) %>%
    filter(End_Date >= Sys.Date()) %>%
    mutate(Display_Text = paste(Supplier, Label, Amount, sep = " | ")) %>%
    select(-End_Date)
  
  return(processed_discounts)
}