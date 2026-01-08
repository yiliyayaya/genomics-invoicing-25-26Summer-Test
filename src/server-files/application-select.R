create_application_select <- function(options_list) {
  # Function to create and return the application select div on the 
  # main section of 1st page of the app.
  #
  # Arguments:
  # options_list(list) - List of application options to select from
  
  req(options_list) 
  
  # Use professional Blue palette for the gradient
  n_colors <- length(options_list)
  palette <- colorRampPalette(brewer.pal(9, "Blues"))(n_colors + 1)
  
  select_container <- div(class = "application-btn-container",
      lapply(seq_along(options_list), function(i) {
        # Format string for Javascript
        formatted_text <- gsub("\\\\", "\\\\\\\\", options_list[i]) # Backslash formatting
        formatted_text <- gsub("'", "\\\\'", formatted_text) # Single Quote formatting
        
        btn_id <- paste0("btn_application_", gsub("[^a-zA-Z0-9]", "_", options_list[i]))
        onclick_code <- sprintf("Shiny.setInputValue('btn_application_click', '%s', {priority: 'event'});", formatted_text)
        
        # Gradient progresses through shades of blue
        text_color <- ifelse(i > n_colors/2 + 1, "white", "black")
        
        tags$button(
          options_list[i],
          id = btn_id,
          class = "application-action-btn",
          style = sprintf("background-color: %s; color: %s;", palette[i], text_color),
          onclick = onclick_code
        )
      })
  )
  
  return(select_container)
}

create_protocol_select <- function(options_list) {
  # Function to create and return the protocol select div on the 
  # main section of 1st page of the app.
  #
  # Arguments:
  # options_list(list) - List of protocol options to select from
  
  req(options_list) 
  
  # Use professional Blue palette for the gradient
  n_colors <- length(options_list)
  palette <- colorRampPalette(brewer.pal(9, "Blues"))(n_colors + 1)
  
  select_container <- div(class = "protocol-btn-container",
                          lapply(seq_along(options_list), function(i) {
                            # Format string for Javascript
                            formatted_text <- gsub("\\\\", "\\\\\\\\", options_list[i]) # Backslash formatting
                            formatted_text <- gsub("'", "\\\\'", formatted_text) # Single Quote formatting
                            
                            btn_id <- paste0("btn_protocol_", gsub("[^a-zA-Z0-9]", "_", options_list[i]))
                            onclick_code <- sprintf("Shiny.setInputValue('btn_protocol_click', '%s', {priority: 'event'});", formatted_text)
                            
                            # Gradient progresses through shades of blue
                            text_color <- ifelse(i > n_colors/2 + 1, "white", "black")
                            
                            tags$button(
                              options_list[i],
                              id = btn_id,
                              class = "protocol-action-btn",
                              style = sprintf("background-color: %s; color: %s;", palette[i], text_color),
                              onclick = onclick_code
                            )
                          })
  )
  
  return(select_container)
}