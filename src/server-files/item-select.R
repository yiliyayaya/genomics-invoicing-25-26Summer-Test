create_items_datatable <- function(input, values) {
  # Function that takes inputs and items price list and generates datatable
  # to be used for rendering on item select page.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values(list) - List of Reactive Values used in server
  
  req(values$data$items, values$application_select, values$protocol_select)
  df_display <- filter_items_data(input, values)
  surcharge_list <- names(values$data$logic_item)
  
  for (i in surcharge_list) {
    col_name <- i
    col_mult <- values$data$logic_item[[i]]
    
    df_display <- df_display %>%
      mutate(!!col_name := ifelse(Is_Constant, Base_Cost + Add_Cost, (Base_Cost * col_mult) + Add_Cost))
  }
  
  df_display <- df_display %>%
    select(-Base_Cost, -Add_Cost, -Is_Constant, -Category) %>%
    rename(Code = Product_Code)

  # Generate display datatable
  items_datatable <- datatable(
    df_display,
    selection = "multiple",
    options = list(pageLength = 10),
    colnames = colnames(df_display))
  
  for (i in surcharge_list) {
    col_name <- i
    items_datatable <- items_datatable %>% formatCurrency(c(col_name))
  }
  
  return(items_datatable)
}

update_cart_items <- function(input, values) {
  # Function that takes user inputs and updates cart in values to reflect
  # selected items.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values(list) - List of Reactive Values used in server
  
  req(input$table_items_catalog_rows_selected)
  df_full <- filter_items_data(input, values)
  
  selected_indices <- input$table_items_catalog_rows_selected
  items_to_add <- df_full[selected_indices, ]
  items_to_add_unique <- items_to_add %>% filter(!Product_Code %in% values$cart$Product_Code)
  
  req(items_to_add_unique)
  
  new_entries <- items_to_add_unique %>%
    mutate(
      Cart_ID = paste0("I-", as.numeric(Sys.time()), "-", row_number()),
      Type = "Item",
      Quantity = 1, Disc_Pct = 0, Disc_Amt = 0,
      Base_Ref = Base_Cost,
      Add_Ref = Add_Cost,
      Unit_Price = 0, Final_Total = 0
    ) %>%
    select(Cart_ID, Product_Code, Name = Item, Description, Type, Category, 
           Base_Ref, Add_Ref, Is_Constant, Unit_Price, Quantity, Disc_Pct, Disc_Amt, Final_Total)
  
  values$cart <- bind_rows(values$cart, new_entries) %>% as.data.frame()
  
  showNotification(paste(nrow(new_entries), "new items added."), type = "message")
}

filter_items_data <- function(input, values) {
  # Function that takes filters items data based on user inputs.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values(list) - List of Reactive Values used in server
  
  items_df <- values$data$items
  
  if(input$filter_item_brand != "All") items_df <- items_df %>% filter(Brand == input$filter_item_brand)
  if(input$filter_category != "All") items_df <- items_df %>% filter(Category == input$filter_category)
  if(values$application_select != "All") {
    common_items <- values$data$application_protocol_item %>% 
      rowwise() %>%
      filter(any(Application %in% values$application_select) | any(Application == "ALL_APPLICATIONS")) %>%
      filter((any(Protocol %in% values$protocol_select) | any(Protocol == "ALL_PROTOCOLS")), values$protocol_select != "All") %>%
      ungroup()
    items_df <- items_df %>% semi_join(common_items, by=c("Item", "Brand"))
  }  
  
  return(items_df)
}