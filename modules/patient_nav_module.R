patient_nav_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('patient_nav_ui'))
  )
}

patient_nav_logic <- function(input, output, session, patient_table, selected_patient, parent) {
  ns <- session$ns
  
  observeEvent(c(patient_table(),selected_patient()), {
    req(patient_table(), selected_patient() )
    updateSelectizeInput(session = parent,
                         inputId = ns('subject_id'),
                         choices = patient_table() %>% 
                           select(ID) %>% 
                           deframe(),
                         selected = selected_patient(),
                         server = TRUE )
  })
  
  output$patient_nav_ui <- renderUI({
    tagList(
      selectizeInput(inputId = ns('subject_id'),
                     width = '100%',
                     label = 'Jump to Subject ID:',
                     choices = NULL,
                     selected = NULL
                     ),
      tags$head(tags$style("
                           #pt_nav_btns * {
                           display: inline;
                           }")),
      div(id="pt_nav_btns", actionButton(inputId = ns('previous_sub'), label = '<--Previous', width = '125px'), actionButton(inputId = ns('next_sub'), label = 'Next-->', width = '125px'))
  )
  })
  outputOptions(output, 'patient_nav_ui', suspendWhenHidden = F) #This output needs to run all the time, so that it can receive data from the Patient Search tab
  
  subject_id_val <- reactive({ input$subject_id })
  previous_sub <- reactive({ input$previous_sub })
  next_sub <- reactive({ input$next_sub })
  
  return(list(
    'subject_id' = subject_id_val,
    'previous_sub' = previous_sub,
    'next_sub' = next_sub
  ))
}

subject_info <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('subject_header_ui')) %>% withSpinner(type = 7, proxy.height = 100, size = .5)
  )
}

subject_info_logic <- function(input, output, session, previousData, all_instruments, instrument_selection, subject, subjectInfo) {
  ns <- session$ns
  
  # observeEvent(subject(), {
  #   browser()
  # })
  
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
  
  # Create a reactive to hold the previous Instrument Complete value.
  instrument_complete_val <-reactive({
    req(previousData(), instrument_complete_field() )
    previousData() %>%
      select(instrument_complete_field() ) %>%
      extract2(1) %>%
      as.numeric()
  })

  # Create text, with information about the subject
  subject_info_text <- reactive({ 
    req(subject() )
    tags$h3(paste('Subject ID: ', subject()), style="padding:0px;")
    })
  
  # Determine which icon is needed to depict the review status for the current subject
  subject_status <- reactive({
    if (instrument_complete_val() == 0 || identical(instrument_complete_val(), numeric(0) )  == TRUE ) { 'www/status_incomplete.png'
    } else if (instrument_complete_val() == 1) { 'www/status_unverified.png'
        } else { 'www/status_complete.png' }
    })
  
  status_indicator <- reactive({
    ## If previous data doesn't exist, the reactive will throw a silent error. If this happens, set status_indicator() to null, to remove status indicator from output. 
    tryCatch(
      {img(id = 'subject_status', src = subject_status(), style='width: 20px' )},
      error=function(error_condition) {
        return(NULL)
      }
    )
    })
  
  output$subject_header_ui <- renderUI({ 
    tagList(
      tags$div(subject_info_text(), style='display:inline-block;vertical-align:middle'),
      tags$div(status_indicator(), style='display:inline-block;vertical-align:middle'),
      renderTable(subjectInfo() %>% mutate_all(as.character) %>% select(-ID), width = '100%', align = 'l', digits = 0)
      )
    }) 
  
}