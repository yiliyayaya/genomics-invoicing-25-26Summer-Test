structure_platform_page <- function() {
  # Layout structure of main content of Platform Select page.
  
  platform_panel <- nav_panel(
    title = "1. Select Platform",
    value = "tab_platform",
    card (
      card_header("Select Platform"),
      # Vertical button stack generated dynamically
      uiOutput("platform_button_ui")
    )
  )
  
  return(platform_panel)
}