instrument_complete_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('abstraction_complete_ui')),
    uiOutput(ns('save_abstraction_ui'))
  )
}

instrument_complete_logic <- function(input, output, session, rc_instrument, instrumentData, previousData, all_instruments, instrument_selection, subjectID) {
  ns <- session$ns
  
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
  
  # Determine the variable name of the currently selected instrument
  selected_instrument_name <- reactive({
    req(all_instruments(), instrument_selection() )
    all_instruments() %>%
      filter(instrument_label == instrument_selection() ) %>%
      extract2(1,1)
  })
  
  # Create a variable containing the instrument complete field name, following the REDCap convention of instrument_name_complete
  instrument_complete_field <- reactive({
    req(selected_instrument_name() )
    paste0(selected_instrument_name(),'_complete')
  })
  
  # Create a reactive storing any previously stored value
  instrument_complete_val <-reactive({
    req(previousData(), instrument_complete_field() )
    previousData() %>%
      select(instrument_complete_field() ) %>%
      extract2(1) %>%
      as.numeric()
    })
  
  # Create "Instrument Complete" selectInput choices as a named list
  dropdown_choices <- c(0,1,2)
  names(dropdown_choices) <- c('Incomplete', 'Unverified', 'Complete')
  
  # Initialize a select input that is hidden containing the "Instrument Complete" Choices
  output$abstraction_complete_ui <- renderUI({
    shinyjs::hidden(
      div(id = ns('abstraction_complete_div'),
          selectInput(inputId = ns('abstraction_complete'), label = 'Complete?', choices = dropdown_choices, selected = instrument_complete_val() )
          )
      )
    })
  
  # Hide or show the instrument complete field based on whether or not all required choices have been answered.  
  observeEvent(instrumentData(), {
    if(qty_required() == qty_required_answered() ) {
      shinyjs::show(id = 'abstraction_complete_div')
    } else {shinyjs::hide(id = 'abstraction_complete_div')}
  })
  
  # Store Value as a reactive, to pass to other modules
  abstraction_complete_val = reactive({ input$abstraction_complete })
  abstraction_complete <- reactive({ qty_required() == qty_required_answered() })
    
  ## Create a Button allowing the status of the review to be saved. 
  save_abstraction_btn <- reactive({ actionButton(inputId = ns('save_abstraction'),label = "Save and Upload to REDCap") })
  save_abstraction_btn_press <- reactive({ input$save_abstraction })
  output$save_abstraction_ui <- renderUI({ save_abstraction_btn() })
  
  return(
    list(
      'abstraction_save_btn_press' = save_abstraction_btn_press,
      'abstraction_complete' = abstraction_complete,
      'abstraction_complete_val' = abstraction_complete_val
      )
    )
}
