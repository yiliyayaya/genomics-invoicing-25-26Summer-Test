create_services_datatable <- function(input, values) {
  # Function that takes inputs and processing charges data and generates 
  # datatable to be used for rendering on item select page.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values(list) - List of Reactive Values used in server
  
  req(values$data)
  df <- values$data$services
  if(input$filter_group != "All") df <- df %>% filter(Group == input$filter_group)
  
  # Filter only if a specific application (not 'All') is selected
  if(values$application_select != "All") {
    common_services <- values$data$application_proc %>% 
      rowwise() %>%
      filter(any(Application %in% values$application_select) | any(Application == "ALL_APPLICATIONS")) %>%
      ungroup()
    df <- df[df$Service %in% common_services$Service, ]
  }
  
  m_int <- values$data$logic_proc[["Internal"]]
  m_col <- values$data$logic_proc[["Ext.Collaborative"]]
  m_rsa <- values$data$logic_proc[["Ext.RSA"]]
  m_com <- values$data$logic_proc[["Commercial"]]
  
  df_display <- df %>% 
    mutate(
      P_Int = Base_Price * m_int,
      P_Col = Base_Price * m_col,
      P_RSA = Base_Price * m_rsa,
      P_Com = Base_Price * m_com
    ) %>%
    select(Group, Service, Description, P_Int, P_Col, P_RSA, P_Com)
  
  services_dt <- datatable(
    df_display,
    selection = "multiple", options = list(pageLength = 10, scrollX = TRUE),
    colnames = c("Group", "Service", "Description", "Internal", "Ext. Collab", "Ext. RSA", "Commercial")
  )
  services_dt <- services_dt %>%  formatCurrency(c("P_Int", "P_Col", "P_RSA", "P_Com"))

  return(services_dt)  
}

update_cart_services <- function(input, values) {
  # Function that takes user inputs and updates cart in values to reflect
  # selected processing charges.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values(list) - List of Reactive Values used in server
  
  if (is.null(input$table_proc_catalog_rows_selected)) return()
  
  df_full <- values$data$services
  if(input$filter_group != "All") df_full <- df_full %>% filter(Group == input$filter_group)
  
  if(values$application_select != "All") {
    common_services <- values$data$application_proc %>% 
      rowwise() %>%
      filter(any(Application %in% values$application_select) | any(Application == "ALL_APPLICATIONS")) %>%
      ungroup()
    df_full <- df_full[df_full$Service %in% common_services$Service, ]
  }
  
  selected_indices <- input$table_proc_catalog_rows_selected
  items_to_add <- df_full[selected_indices, ]
  existing_names <- values$cart$Name
  items_to_add_unique <- items_to_add %>% filter(!Service %in% existing_names)
  
  if (nrow(items_to_add_unique) == 0) return()
  
  new_entries <- items_to_add_unique %>%
    mutate(
      Cart_ID = paste0("P-", as.numeric(Sys.time()), "-", row_number()),
      Type = "Processing",
      Category = Group, 
      Quantity = 1, Disc_Pct = 0, Disc_Amt = 0,
      Product_Code = "SVC",
      Base_Ref = Base_Price,
      Add_Ref = 0,
      Is_Constant = FALSE,
      Unit_Price = 0, Final_Total = 0
    ) %>%
    select(Cart_ID, Product_Code, Name = Service, Description, Type, Category, 
           Base_Ref, Add_Ref, Is_Constant, Unit_Price, Quantity, Disc_Pct, Disc_Amt, Final_Total)
  
  values$cart <- bind_rows(values$cart, new_entries) %>% as.data.frame()
  showNotification(paste(nrow(new_entries), "new services added."), type = "message")
}