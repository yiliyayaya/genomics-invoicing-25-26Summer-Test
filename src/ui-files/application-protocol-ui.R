structure_application_protocol_page <- function() {
  # Layout structure of main content of Platform Select page.
  
  platform_panel <- nav_panel(
    title = "1. Select Application/Protocol",
    value = "tab_application",
    card (
      card_header("Select Application and Protocol"),
      h3("Application Select"),
      # Vertical button stack generated dynamically
      uiOutput("application_button_ui"),
      h3("Protocol Select"),
      uiOutput("protocol_button_ui")
    )
  )
  
  return(platform_panel)
}