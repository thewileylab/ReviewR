#' Patient Navigation Module
#'
#' This module will assist with selecting a patient. It keeps the patient search tab data table up to date, server side renders the select input on the patient chart tab, and displays in context information about the selected patient. 
#'
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_patient_nav_module
#' 
#' @keywords internal
#' @export

patient_nav_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('patient_nav_ui'))
  )
}

#' @param patient_table A reactive expression containing a DT of the current "All Patients" table
#' @param selected_patient A reactive expression containing the currently selected patient subject ID
#' @param parent the parent environment of this module
#'
#' @rdname mod_patient_nav_module
#' 
#' @keywords internal
#' @export
#' @import shiny
#' @importFrom tibble deframe
#' @importFrom dplyr select
#' @importFrom rlang .data
patient_nav_logic <- function(input, output, session, patient_table, selected_patient, parent) {
  ns <- session$ns
  subject_choices_test <- reactive({
    req(patient_table() )
    patient_table() %>% 
      select(contains('Record Status')) %>% 
      ncol()
  })
  subject_choices <- reactive({
    req(patient_table(), subject_choices_test())
    if(subject_choices_test() >= 1){
    names<- patient_table() %>% 
      select(.data$ID, tail(names(.),1)) %>% 
      unite(subject_choices, sep = ' - ') %>% 
      deframe()
    choices <- patient_table() %>% select(.data$ID) %>% deframe()
    names(choices) <- names
    return(choices)
    } else {
      patient_table() %>% 
        select(.data$ID) %>% 
        unite(subject_choices, sep = ' - ') %>% 
        deframe()
    }
    })
  selected_patient_status <- eventReactive(selected_patient(), {
    req(patient_table(), subject_choices_test(), selected_patient(),subject_choices())
    if(subject_choices_test() >= 1 ) {
      name <- patient_table() %>% 
        select(.data$ID, tail(names(.),1)) %>% 
        filter(.data$ID == selected_patient()) %>% 
        unite(subject_choices, sep = ' - ') %>% 
        deframe()
      choice <- selected_patient()
      names(choice) <- name
      return(choice)
      } else {
        selected_patient()
        }
    })
  
  observeEvent(selected_patient_status(), {
    # req(selected_patient_status() )
    # browser()
    shinyjs::reset('jump_list_div') ## this is supposed to force the input to re-initialize, improving the consistency of the 'selected' option. Wishful thinking.
    # shinyjs::reset(ns('subject_id'))
    # message('resetting jump list')
    updateSelectizeInput(session = parent,
                         inputId = ns('subject_id'),
                         choices = subject_choices(),
                         selected = selected_patient_status(), ## this is supposed supposed to update the selected value and it totally does... sometimes. 
                         server = TRUE )
  })
  
  output$patient_nav_ui <- renderUI({
    tagList(
      div(id='jump_list_div',
        selectizeInput(inputId = ns('subject_id'),
                       width = '100%',
                       label = 'Jump to Subject ID:',
                       choices = NULL,
                       selected = NULL
                       )
        ),
      tags$head(tags$style("
                           #pt_nav_btns * {
                           display: inline;
                           }")),
      div(id="pt_nav_btns", 
          actionButton(inputId = ns('previous_sub'), 
                       label = '<--Previous', 
                       width = '125px'), 
          actionButton(inputId = ns('next_sub'), 
                       label = 'Next-->', 
                       width = '125px')
          )
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

#' @rdname mod_patient_nav_module
#' 
#' @keywords internal
#' @export
subject_info <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('subject_header_ui')) %>% withSpinner(type = 7, proxy.height = 100, size = .5)
  )
}


#' @param previousData Previous Abstraction "Instrument Complete" Value
#' @param all_instruments All data abstraction instruments in the project
#' @param instrument_selection The selected data abstraction instrument
#' @param subject A reactive expression containing the currently selected patient subject ID
#' @param subjectInfo Demographic information about the currently selected patient
#'
#' @rdname mod_patient_nav_module
#' 
#' @keywords internal
#' @export
#' @import shiny
#' @importFrom rlang .data
#' @importFrom magrittr extract2
#' @importFrom dplyr mutate_all

subject_info_logic <- function(input, output, session, previousData, all_instruments, instrument_selection, subject, subjectInfo) {
  ns <- session$ns
  
  # observeEvent(subject(), {
  #   browser()
  # })
  
  # Determine the variable name of the currently selected instrument
  selected_instrument_name <- reactive({
    req(all_instruments(), instrument_selection() )
    all_instruments() %>%
      filter(.data$instrument_label == instrument_selection() ) %>%
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
    tryCatch({
      img(id = 'subject_status', src = subject_status(), style='width: 20px' )
      },
      error=function(error_condition) {
        return(NULL)
      }
    )
    })
  
  output$subject_header_ui <- renderUI({ 
    tagList(
      tags$div(subject_info_text(), style='display:inline-block;vertical-align:middle'),
      tags$div(status_indicator(), style='display:inline-block;vertical-align:middle'),
      renderTable(subjectInfo() %>% mutate_all(as.character) %>% select(-.data$ID), width = '100%', align = 'l', digits = 0, sanitize.text.function=identity)
      )
    }) 
}
