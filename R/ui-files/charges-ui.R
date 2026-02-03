structure_charges_page <- function() {
  # Layout structure of main content of Processing Charges page.
  
  charges_panel <- nav_panel(
    title = "3. Select Processing",
    value = "tab_processing",
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span("Processing Services Catalog"),
            actionButton("add_proc_btn_top", "Add Selected to Quote", class = "btn-success btn-sm")
        )
      ),
      DTOutput("table_proc_catalog")
    )
  )
  
  return(charges_panel)
}

charges_sidebar_filters <- function() {
  # Layout sidebar filters structure of Processing Charges page.
  
  charges_sidebar_panel <- conditionalPanel(
    condition = "input.nav_tabs == 'tab_processing'",
    h5("Filter Services"),
    selectInput("filter_group", "Filter Group", choices = "All"),
    br(),
    actionButton("add_proc_btn", "Add Selected to Quote", class = "btn-success w-100")
  )
  
  return(charges_sidebar_panel)
}