create_items_datatable <- function(input, values) {
  # Function that takes inputs and items price list and generates datatable
  # to be used for rendering on item select page.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values(list) - List of Reactive Values used in server
  
  req(values$data$items, values$platform_select)
  
  items_df <- values$data$items
  
  # Filter only if a specific platform (not 'All') is selected
  if(values$platform_select != "All") {
    common_items <- values$data$platform_item %>% 
      rowwise() %>%
      filter(any(Platform %in% values$platform_select) | any(Platform == "ALL_PLATFORMS")) %>%
      ungroup()
    items_df <- items_df %>% semi_join(common_items, by=c("Item", "Brand"))
  }

  #Filter by protocol and category
  if(input$filter_item_brand != "All") items_df <- items_df %>% filter(Brand == input$filter_item_brand)
  if(input$filter_category != "All") items_df <- items_df %>% filter(Category == input$filter_category)

  # Calculate Internal/External Surcharge prices
  internal_mult <- values$data$logic_item[["Internal"]]
  external_mult <- values$data$logic_item[["External"]]
  df_display <- items_df %>% 
    mutate(
      Price_Internal = ifelse(Is_Constant, Base_Cost + Add_Cost, (Base_Cost * internal_mult) + Add_Cost),
      Price_External = ifelse(Is_Constant, Base_Cost + Add_Cost, (Base_Cost * external_mult) + Add_Cost)
    ) %>%
    select(Product_Code, Brand, Item, Description, Price_Internal, Price_External)

  # Generate display datatable
  items_datatable <- datatable(
    df_display,
    selection = "multiple",
    options = list(pageLength = 10),
    colnames = c("Code", "Brand", "Item", "Description", "Internal Price", "External Price"))
  items_datatable <- items_datatable %>%  formatCurrency(c("Price_Internal", "Price_External"))
  
  return(items_datatable)
}

update_cart_items <- function(input, values) {
  # Function that takes user inputs and updates cart in values to reflect
  # selected items.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values(list) - List of Reactive Values used in server
  
  if (is.null(input$table_items_catalog_rows_selected)) return()
  
  df_full <- values$data$items
  if(input$filter_item_brand != "All") df_full <- df_full %>% filter(Brand == input$filter_item_brand)
  if(input$filter_category != "All") df_full <- df_full %>% filter(Category == input$filter_category)
  
  if(values$platform_select != "All") {
    common_items <- values$data$platform_item %>% 
      rowwise() %>%
      filter(any(Platform %in% values$platform_select) | any(Platform == "ALL_PLATFORMS")) %>%
      ungroup()
    df_full <- df_full %>% semi_join(common_items, by=c("Item", "Brand"))
  } 
  
  selected_indices <- input$table_items_catalog_rows_selected
  items_to_add <- df_full[selected_indices, ]
  existing_codes <- values$cart$Product_Code
  items_to_add_unique <- items_to_add %>% filter(!Product_Code %in% existing_codes)
  
  if (nrow(items_to_add_unique) == 0) return()
  
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