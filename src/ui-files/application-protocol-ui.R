structure_application_protocol_page <- function() {
  # Layout structure of main content of Platform Select page.
  
  platform_panel <- nav_panel(
    title = "1. Select Application/Protocol",
    value = "tab_application",
    card(
      card_header("Select Application and Protocol"),
      layout_column_wrap(
        width = 1/2,
        fixed_width = TRUE,
        div(
          style = "min-height: 1066px; padding: 20px; width: 150%;",
          h3("Application Select", style = "color: rgba(31, 58, 147, 0.85); font-weight: bold; margin-bottom: 20px;"),
          # Vertical button stack generated dynamically
          uiOutput("application_button_ui")
        ),
        div(
          style = "min-height: 1066px; padding: 20px; width: 150%;",
          h3("Protocol Select", style = "color: rgba(31, 58, 147, 0.85); font-weight: bold; margin-bottom: 20px;"),
          uiOutput("protocol_button_ui")
        )
      )
    )
  )
  
  return(platform_panel)
}