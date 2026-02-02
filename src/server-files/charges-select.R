create_services_datatable <- function(input, values) {
  # Function that takes inputs and processing charges data and generates 
  # datatable to be used for rendering on item select page.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values(list) - List of Reactive Values used in server
  
  req(values$data)
  df_display <- filter_services_data(input, values)
  surcharge_list <- names(values$data$logic_proc)
  
  for (i in surcharge_list) {
    col_name <- i
    col_mult <- values$data$logic_proc[[i]]
    
    df_display <- df_display %>%
      mutate(!!col_name := Base_Price * col_mult)
  }
  
  df_display <- df_display %>%
    select(-Base_Price)

  services_dt <- datatable(
    df_display,
    selection = "multiple", 
    options = list(pageLength = 10, scrollX = TRUE),
    colnames = colnames(df_display)
  )
  
  for (i in surcharge_list) {
    col_name <- i
    services_dt <- services_dt %>% formatCurrency(c(col_name))
  }

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
  
  df_full <- filter_services_data(input, values)
  
  selected_indices <- input$table_proc_catalog_rows_selected
  items_to_add <- df_full[selected_indices, ]
  items_to_add_unique <- items_to_add %>% filter(!Service %in% values$cart$Name)
  
  req(items_to_add_unique)
  
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
  if(nrow(new_entries) > 0) {
    values$cart <- bind_rows(values$cart, new_entries) %>% as.data.frame()
    showNotification(paste(nrow(new_entries), "new services added."), type = "message")    
  }
}

filter_services_data <- function(input, values) {
  # Function that takes filters services data based on user inputs.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values(list) - List of Reactive Values used in server
  
  services_df <- values$data$services
  
  if(input$filter_group != "All") services_df <- services_df %>% filter(Group == input$filter_group)
  common_services <- values$data$application_protocol_proc %>%
    filter(
      # Filter by application with anonymous func
      if(values$application_select == "All") TRUE else
        map_lgl(Application, ~ any(.x %in% c(values$application_select, "ALL_APPLICATIONS"))),
      
      # Filter by protocol with anonymous func
      if (values$protocol_select == "All") TRUE else 
        map_lgl(Protocol, ~ any(.x %in% c(values$protocol_select, "ALL_PROTOCOLS")))
    )
  if(nrow(common_services) > 0) {
    services_df <- services_df[services_df$Service %in% common_services$Service, ]
  }
  
  return(services_df)
}