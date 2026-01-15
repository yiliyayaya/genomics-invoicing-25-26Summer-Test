structure_items_page <- function() {
  # Layout structure of main content of Items Price List page.
  
  items_panel <- nav_panel(
    title = "2. Select Items",
    value = "tab_items",
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span("Items Catalog (Consumables)"),
            actionButton("add_items_btn_top", "Add Selected to Quote", class = "btn-success btn-sm")
        )
      ),
      DTOutput("table_items_catalog")
    )
  )
  
  return(items_panel)
}

item_sidebar_filters <- function() {
  # Layout sidebar filters structure of Items Price List page.
  
  item_sidebar_panel <- conditionalPanel(
    condition = "input.nav_tabs == 'tab_items'",
    h5("Filter Items"),
    selectInput("filter_item_brand", "Filter Brand", choices = "All", selectize = TRUE), 
    selectInput("filter_category", "Filter Category", choices = "All"),
    div(class = "text-muted small mb-2", "Tip: Select items in the table, then click 'Add'."),
    actionButton("add_items_btn", "Add Selected to Quote", class = "btn-success w-100")
  )
  
  return(item_sidebar_panel)
}