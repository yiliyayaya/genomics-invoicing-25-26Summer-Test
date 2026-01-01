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
  
  quote_dt <- datatable(display_df,
            selection = "multiple",
            editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4, 8))),
            options = list(
              pageLength = 25, 
              dom = 't',
              scrollX = TRUE 
            ),
            rownames = FALSE,
            colnames = c("Code", "Name", "Description", "Type", "Unit Price", "Quantity", "Discount %", "Discount $", "Total"))
  
  quote_dt <- quote_dt %>% 
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
    current_row <- values_rv$cart[row_idx, ]
    values_rv$cart$Disc_Pct[row_idx] <- discount_pct
    pre_discount_total <- current_row$Unit_Price * current_row$Quantity
    discount_amt <- pre_discount_total * (discount_pct / 100)
    values_rv$cart$Disc_Amt[row_idx] <- discount_amt
    values_rv$cart$Final_Total[row_idx] <- pre_discount_total - discount_amt
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
    current_row <- values_rv$cart[row_idx, ]
    values_rv$cart$Disc_Amt[row_idx] <- discount_amt
    pre_discount_total <- current_row$Unit_Price * current_row$Quantity
    if(pre_discount_total < discount_amt) {
      discount_pct <-  100
      values_rv$cart$Disc_Amt[row_idx] <- pre_discount_total
      values_rv$cart$Disc_Pct[row_idx] <- discount_pct
      values_rv$cart$Final_Total[row_idx] <- 0
    } else {
      discount_pct <- (discount_amt / pre_discount_total) * 100
      values_rv$cart$Disc_Pct[row_idx] <- discount_pct
      values_rv$cart$Final_Total[row_idx] <- pre_discount_total - discount_amt  
    }
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
  
  discount_data <- values_rv$data$supplier_discount %>% filter(Display_Text == input$supplier_discount_select)
  discount_pct <- discount_data$Amount[1] * 100  

  for (row_idx in input$table_final_quote_rows_selected) {
    current_row <- values_rv$cart[row_idx, ]
    values_rv$cart$Disc_Pct[row_idx] <- discount_pct
    pre_discount_total <- current_row$Unit_Price * current_row$Quantity
    discount_amt <- pre_discount_total * (discount_pct / 100)
    values_rv$cart$Disc_Amt[row_idx] <- discount_amt
    values_rv$cart$Final_Total[row_idx] <- pre_discount_total - discount_amt
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
  
  current_row <- values_rv$cart[row_idx, ]
  price <- current_row$Unit_Price
  qty   <- current_row$Quantity
  
  if (col_idx == 5) {
    qty <- new_val
    values_rv$cart$Quantity[row_idx] <- qty
    pct <- values_rv$cart$Disc_Pct[row_idx]
    gross <- price * qty
    disc_amt <- gross * (pct / 100)
    values_rv$cart$Disc_Amt[row_idx] <- disc_amt
    values_rv$cart$Final_Total[row_idx] <- gross - disc_amt
  } else if (col_idx == 6) { 
    pct <- new_val
    values_rv$cart$Disc_Pct[row_idx] <- pct
    gross <- price * qty
    disc_amt <- gross * (pct / 100)
    values_rv$cart$Disc_Amt[row_idx] <- disc_amt
    values_rv$cart$Final_Total[row_idx] <- gross - disc_amt
  } else if (col_idx == 7) { 
    amt <- new_val
    values_rv$cart$Disc_Amt[row_idx] <- amt
    gross <- price * qty
    pct <- if(gross > 0) (amt / gross) * 100 else 0
    values_rv$cart$Disc_Pct[row_idx] <- pct
    values_rv$cart$Final_Total[row_idx] <- gross - amt
  }
}