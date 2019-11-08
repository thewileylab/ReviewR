instrument_complete_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('save_abstraction_ui'))
  )
}

instrument_complete_logic <- function(input, output, session, rc_instrument, instrumentData) {
  ns <- session$ns
  
  # Determine how many fields are required in the redcap instrument
  qty_required <- reactive({
    message('qty_required reactive')
    req(rc_instrument() )
    rc_instrument() %>% 
      filter(required_field == 'y') %>% 
      nrow()
  })
  
  # As the instrument is updated, determine how many of the required questions have been answered
  qty_required_answered <- reactive({
    req(rc_instrument(), instrumentData() )
    message('qty_required_answered reactive')
    rc_instrument() %>% 
      filter(required_field == 'y') %>% 
      mutate(inputID = ns(shiny_inputID)) %>% 
      select(inputID) %>% 
      left_join(instrumentData() ) %>% 
      remove_missing(na.rm = T) %>% 
      filter(values != '') %>% 
      nrow()
  })
  
  # If the number of required responses matches the number of required questions, show the instrument complete dropdown
  redcap_instrument_complete <- reactive({
    message('redcap_instrument_complete reactive')
    # req(instrument_complete_test() )
    req(qty_required(), qty_required_answered() )
    if(qty_required() == qty_required_answered() ) {
      dropdown_choices <- c(0,1,2)
      names(dropdown_choices) <- c('Incomplete', 'Unverified', 'Complete')
      return(dropdown_choices)
    } else {
      dropdown_choices <- c(0,1)
      names(dropdown_choices) <- c('Incomplete', 'Unverified')
      return(dropdown_choices)    
    }
  })
  
  ## Create a Button allowing the status of the review to be saved. 
  save_abstraction_btn <- reactive({ actionButton(inputId = ns('save_abstraction'),label = "Save and Upload to REDCap") })
  save_abstraction_btn_press <- reactive({ input$save_abstraction })
  
  output$save_abstraction_ui <- renderUI({save_abstraction_btn() })
  
  return(
    list(
      'abstraction_save_btn_press' = save_abstraction_btn_press
      )
    )
}
