instrument_complete_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('abstraction_complete_ui')),
    uiOutput(ns('save_abstraction_ui'))
  )
}

instrument_complete_logic <- function(input, output, session, rc_instrument, instrumentData) {
  ns <- session$ns
  
  # ## Debug Observer
  # observeEvent(save_abstraction_btn_press(), {
  #   browser()
  # })
  
  # Determine how many fields are required in the redcap instrument
  qty_required <- reactive({
    req(rc_instrument() )
    rc_instrument() %>% 
      filter(required_field == 'y') %>% 
      nrow()
  })
  
  # As the instrument is updated, determine how many of the required questions have been answered
  qty_required_answered <- reactive({
    req(rc_instrument(), instrumentData() )
    rc_instrument() %>% 
      filter(required_field == 'y') %>% 
      mutate(inputID = shiny_inputID) %>% 
      select(inputID) %>% 
      left_join(instrumentData() , by = 'inputID') %>% 
      remove_missing(na.rm = T) %>% 
      filter(values != '') %>% 
      nrow()
  })
  
  # If the number of required responses matches the number of required questions, show the instrument complete dropdown
  redcap_instrument_complete <- reactive({
    req(qty_required() == qty_required_answered() )
    dropdown_choices <- c(0,1,2)
    names(dropdown_choices) <- c('Incomplete', 'Unverified', 'Complete')
    selectInput(inputId = ns('abstraction_complete'), label = 'Complete?', choices = dropdown_choices, selected = input$abstraction_complete)
  })
  abstraction_complete_val = reactive({ input$abstraction_complete })
  output$abstraction_complete_ui <- renderUI({ redcap_instrument_complete() })
  
  ## Create a Button allowing the status of the review to be saved. 
  save_abstraction_btn <- reactive({ actionButton(inputId = ns('save_abstraction'),label = "Save and Upload to REDCap") })
  save_abstraction_btn_press <- reactive({ input$save_abstraction })
  output$save_abstraction_ui <- renderUI({ save_abstraction_btn() })
  
  return(
    list(
      'abstraction_save_btn_press' = save_abstraction_btn_press,
      'abstraction_complete_val' = abstraction_complete_val
      )
    )
}
