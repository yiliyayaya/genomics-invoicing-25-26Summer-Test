QUANTITY_COL_IDX <- 5
DISC_PCT_COL_IDX <- 6
DISC_AMT_COL_IDX <- 7

create_quote_datatable <- function(input, cart) {
  # Function that takes user inputs and creates datatable with selected values.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # cart(dataframe) - Dataframe of selcted items and services
  
  req(input$project_type)
  
  display_df <- cart %>% 
    select(Product_Code, Name, Description, Type, Unit_Price, Quantity, Disc_Pct, Disc_Amt, Final_Total) %>%
    as.data.frame() 
  
  # Structure the quote datatable
  quote_dt <- datatable(display_df,
            selection = "multiple",
            editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4, 8))),
            options = list(
              pageLength = 25, 
              dom = 't',
              scrollX = TRUE 
            ),
            rownames = FALSE,
            colnames = c("Code", "Name", "Description", "Type", "Unit Price", "Quantity", "Discount %", "Discount $", "Total"))%>% 
            formatCurrency(c("Unit_Price", "Disc_Amt", "Final_Total")) %>%
            formatRound("Disc_Pct", 2)

  return(quote_dt)  
}

create_mult_ref_table <- function(multiplier_data, project_type) {
  # Function that takes creates surcharge multiplier reference table.
  #
  # Arguments:
  # multiplier_data(dataframe) - Dataframe of multiplier reference data
  # project_type(string) - Code for type of project surcharge selected
  
  req(multiplier_data, project_type)
  if(project_type == "" || is.na(project_type)) { return(NULL) }
  
  proc_mult <- round(multiplier_data$logic_proc[[project_type]], 3)
  rate_type <- if(project_type == "Internal") "Internal" else "External"
  item_mult <- multiplier_data$logic_item[[rate_type]]
  
  return(data.frame(
    Category = c("Items (Consumables)", "Processing Services"),
    Multiplier = c(paste0("x", item_mult), paste0("x", proc_mult))
  ))
}

apply_percentage_discount <- function(input, values_rv) {
  # Function that applies specified % discount to selected items/charges.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values_rv(reactiveValues) - List of reactiveValues used in server
  
  req(input$table_final_quote_rows_selected, input$percent_discount_input)
  if(input$percent_discount_input < 0 | input$percent_discount_input > 100) {
    showNotification("Input Error: % Discount must be numeric between 0 and 100.", type = "warning")
    return()
  }
  
  discount_pct <- input$percent_discount_input
  for (row_idx in input$table_final_quote_rows_selected) {
    edit_table(values_rv, row_idx, new_disc_pct = discount_pct)
  }
}

apply_amount_discount <- function(input, values_rv) {
  # Function that applies specified $ discount to selected items/charges.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values_rv(reactiveValues) - List of reactiveValues used in server
  
  req(input$table_final_quote_rows_selected, input$amount_discount_input)
  if(input$amount_discount_input < 0) {
    showNotification("Input Error: $ Discount must non-negative.", type = "warning")
    return()
  }
  
  discount_amt <- input$amount_discount_input
  for (row_idx in input$table_final_quote_rows_selected) {
    edit_table(values_rv, row_idx, new_disc_amt = discount_amt)
  }
}

apply_supplier_discount <- function(input, values_rv) {
  # Function that applies specified supplier discount to selected items/charges.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values_rv(reactiveValues) - List of reactiveValues used in server
  
  req(input$supplier_discount_select, input$table_final_quote_rows_selected)
  if(input$supplier_discount_select == "") {
    return()
  }
  
  # Calculate new values accordingly
  discount_data <- values_rv$data$supplier_discount %>% filter(Display_Text == input$supplier_discount_select)
  for (row_idx in input$table_final_quote_rows_selected) {
    if(discount_data$Type == "percentage") {
      discount_pct <- discount_data$Amount[1] * 100
      edit_table(values_rv, row_idx, new_disc_pct = discount_pct)
    } else if (discount_data$Type == "amount") {
      discount_amount <- discount_data$Amount[1]
      edit_table(values_rv, row_idx, new_disc_amt = discount_amount)
    }
  }  
}

recalculate_cart <- function(input, values_rv) {
  # Function that recalculates cart items/charges.
  #
  # Arguments:
  # input(list) - List of input values from Shiny server function
  # values_rv(reactiveValues) - List of reactiveValues used in server
  
  req(values_rv$data, nrow(values_rv$cart) > 0)
  if(input$project_type == "") return()
  
  ptype <- input$project_type
  rate_type_item <- if(ptype == "Internal") "Internal" else "External"
  mult_item <- values_rv$data$logic_item[[rate_type_item]]
  mult_proc <- values_rv$data$logic_proc[[ptype]]
  
  # Default multiplier is 1
  if(is.null(mult_proc)) mult_proc <- 1 
  
  values_rv$cart <- values_rv$cart %>%
    mutate(
      Unit_Price = case_when(
        Type == "Item" & Is_Constant ~ Base_Ref + Add_Ref,
        Type == "Item" & !Is_Constant ~ (Base_Ref * mult_item) + Add_Ref,
        Type == "Processing" ~ Base_Ref * mult_proc,
        TRUE ~ 0
      ),
      Gross = Unit_Price * Quantity,
      Disc_Amt = Gross * (Disc_Pct / 100),
      Final_Total = Gross - Disc_Amt
    ) %>%
    select(-Gross)
}

update_quote_table <- function(table_edits, values_rv) {
  # Function that updates quote table to match user edits to quote table.
  #
  # Arguments:
  # table_edits(list) - List of variables containing details on user's action
  # values_rv(reactiveValues) - List of reactiveValues used in server
  
  req(table_edits)
  row_idx <- table_edits$row 
  col_idx <- table_edits$col
  new_val <- as.numeric(table_edits$value)
  
  if (col_idx == QUANTITY_COL_IDX) {
    # Edit Quantity
    edit_table(values_rv, row_idx, new_qty = new_val)
  } else if (col_idx == DISC_PCT_COL_IDX) { 
    # Edit Discount %
    edit_table(values_rv, row_idx, new_disc_pct = new_val)
  } else if (col_idx == DISC_AMT_COL_IDX) {
    # Edit Discount $
    edit_table(values_rv, row_idx, new_disc_amt = new_val)
  }
}

edit_table <- function(values_rv, edited_row_idx, new_qty=NULL, new_disc_pct=NULL, new_disc_amt=NULL) {
  # Function that updates the quote table based on given changed data of either quantity, discount % or discount amount $.
  #
  # Arguments:
  # values_rv(reactiveValues) - List of reactiveValues used in server
  # edited_row_idx (int) - The index of the edited row
  # new_qty (int) - The new quantity of items 
  # new_disc_pct (int) - The new discount %
  # new_disc_amt (int) - The new discount amount $
  
  current_row <- values_rv$cart[edited_row_idx, ]
  price <- current_row$Unit_Price
  qty <- current_row$Quantity
  pct <- current_row$Disc_Pct
  amt <- current_row$Disc_Amt
  
  # Check for input type
  if(!is.null(new_qty)) {
    req(new_qty >= 0)
    qty <- new_qty
  } else if(!is.null(new_disc_pct)) {
    req(new_disc_pct >= 0)
    pct <- new_disc_pct
  } else if(!is.null(new_disc_amt)) {
    req(new_disc_amt >= 0)
    amt <- new_disc_amt
  }
  
  # Calculate gross price and new % or $ discount
  gross <- price * qty
  if(!is.null(new_disc_amt)) {
    pct <- if (gross > 0) (amt/gross) * 100 else 0
  } else {
    amt <- gross * (pct / 100)
  }
  
  # Prevent negative values
  if (gross - amt < 0) {
    pct <- 100
    amt <- gross
  }
  
  values_rv$cart$Disc_Pct[edited_row_idx] <- pct
  values_rv$cart$Quantity[edited_row_idx] <- qty
  values_rv$cart$Disc_Amt[edited_row_idx] <- amt
  values_rv$cart$Final_Total[edited_row_idx] <- gross - amt
}