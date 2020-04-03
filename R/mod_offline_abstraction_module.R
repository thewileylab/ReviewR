#' Offline Abstraction Module
#'
#' This module contains all of the Offline setup and instrumnet upload/download components.
#' 
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
#' @import shiny
#' 
offline_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('setup_ui')),
    uiOutput(ns('offline_config_btn'))
  )
}

#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
offline_setup_select_logic <-function(input, output, session) {
  ns <- session$ns
  
  choices <- reactive({
    c('Shiny Contest Demo' = 'demo',
      'New Session' = 'new')
  })
  
  setup_ui <- reactive({
    tagList(
      selectInput(inputId = ns('offline_session'),
                  label = 'Start a new offline session, or connect to an existing session:',
                  choices = choices()
                  )
    )
  })
  output$setup_ui <- renderUI({ setup_ui() })
  offline_selection <- reactive({ input$offline_session })
  return(list(
    'offline_selection' = offline_selection
  ))
}

#' @param input internal
#' @param output internal
#' @param session internal
#' @param selection Existing or new session?
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
offline_setup_logic <- function(input, output, session, selection) {
  ns <- session$ns
  
  config_button <- reactive({
    req(selection() )
   if (selection()  == 'new') {
      fileInput(inputId = ns('new'),
                label = 'Upload Abstraction Instrument CSV:',
                buttonLabel = 'Browse',
                multiple = F,
                accept =  c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv"))
   } else  {
     actionButton(inputId = ns('connect'),
                  label = 'Connect',
                  icon = icon('notes-medical')) }
  })
  
  connect <- reactive({ input$connect })
  new <- reactive({ input$new })

  output$offline_config_btn <- renderUI({ config_button() })
  return(list(
    'existing_session_btn' = connect,
    'existing_session' = selection,
    'new_session' = new
    )
  )
}

#' @param id The namespace id for the UI output
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export

offline_abs_connected_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('offline_connected_ui'))
  )
}

#' @param input internal
#' @param output internal
#' @param session internal
#' @param abstraction_vars Abstraction setup variables 
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
offline_connected_logic <- function(input, output, session, abstraction_vars) {
  ns <- session$ns
  # 
  # observeEvent(abstraction_vars$offline_press(), {
  #   browser()
  # })
   offline_info <- reactive({
     if(abstraction_vars$offline_existing_session() == 'demo') {
       'Shiny Contest Demo Abstraction'
     } else {
       abstraction_vars$offline_new_session()$name
     }
   })

   offline_connected_message <- eventReactive(abstraction_vars$offline_press(), {
     req(offline_info() )
     HTML(paste('<H3>Success!!</H3>',
                'You have connected to the', offline_info(), ' Instrument.',
                '<br>',
                '<br>',
                '<b>Please configure the Offline Instrument in the box below before continuing.</b>',
                '<br><br>'))
   })
  
  output$offline_connected_ui <- renderUI({
    req(offline_connected_message() )
    tagList(
      offline_connected_message(),
      actionButton(inputId = ns('offline_disconnect'),label = 'Disconnect')
    )
  })
  
  offline_disconnect <- reactive({ input$offline_disconnect })
  
  return(list(
    'offline_instrument' = offline_info,
    'offline_disconnect' = offline_disconnect
  ))
}


#' @param id The namespace id for the UI output
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
#' @importFrom readr read_csv

offline_abs_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('offline_config_ui')), 
    uiOutput(ns('offline_reviewer')), 
    uiOutput(ns('offline_current_reviewer')),
    uiOutput(ns('offline_configure_btn'))
  )
}

offline_abs_config_logic <- function(input, output, session, abstraction_vars) {
  ns <- session$ns
  
  instrument <- reactive({
    if(abstraction_vars$offline_existing_session() == 'demo') {
      read_csv(app_sys('extdata/shiny_contest_instrument.csv'))
    } else {
      read_csv(abstraction_vars$offline_new_session()$name)
    }
  })
  
  offline_instrument_patient_id <- reactive({
    req(instrument() )
    tagList(
      selectInput(inputId = ns('offline_identifier_field'),
                  label = 'Which variable contains your record identifier (e.g., MRN, subject ID)?',
                  choices = instrument() %>%
                    filter(.data$field_type == 'text') %>% 
                    select(.data$field_label) %>%
                    deframe()
      )
    )
  })
  
  offline_identifier <- reactive({ input$offline_identifier_field })
  
  output$offline_config_ui <- renderUI({offline_instrument_patient_id() })
  
  return(list(
    'offline_instrument' = instrument,
    'offline_identifier' = offline_identifier
  ))
}

offline_abs_config_reviewer_logic <- function(input, output, session, offline_instrument, offline_identifier) {
  ns <- session$ns
  
  offline_reviewer_id <- reactive({
    req(offline_instrument(), offline_identifier() )
    selectInput(inputId = ns('offline_reviewer_field'), 
                label = 'Which variable contains your reviewer identifier?',
                choices = append('(Not Applicable)', 
                                 offline_instrument() %>%
                                   filter(.data$field_type == 'text' & .data$field_label != offline_identifier() ) %>% 
                                   select(.data$field_label) %>%
                                   deframe()
                )
    )
  })
  offline_reviewer <- reactive({ input$offline_reviewer_field })
  
  output$offline_reviewer <- renderUI({offline_reviewer_id() })
  
  offline_previous_reviewers <- reactive({
    req(offline_instrument(), offline_reviewer() )
    list('Bill' = 'bill',
         'Ted' = 'ted')
  })
  
  offline_current_reviewer_question <- reactive({
    req(offline_reviewer())
    if(offline_reviewer() == '(Not Applicable)' ) {
      return(NULL)
    } else {
      selectizeInput(inputId = ns('offline_current_reviewer'),
                     label = 'Select your name from the list, or enter a new one:',
                     choices = append('',
                                      offline_previous_reviewers()
                     ), 
                     options = list(create = TRUE,
                                    placeholder = 'New Reviewer'))
    }
  })
  
  output$offline_current_reviewer <- renderUI( offline_current_reviewer_question() )
  output$offline_configure_btn <- renderUI({ actionButton(inputId = ns('offline_configure'), label = 'Configure Offline Instrument') })
  offline_selected_reviewer <- reactive({ input$offline_current_reviewer })
  offline_configure_btn_press <- reactive({ input$offline_configure})
  
  return(list(
    'offline_identifier' = offline_identifier,
    'offline_reviewer' = offline_reviewer,
    'offline_selected_reviewer' = offline_selected_reviewer,
    'offline_configure_btn_press' = offline_configure_btn_press
  ))
}

offline_instrument_configured_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('offline_configured_ui'))
    )
}

offline_instrument_configured_ui_logic <- function(input, output, session, offline_config_vars, offline_project_vars) {
  ns <- session$ns
  
  offline_configured_message <- eventReactive(offline_config_vars$offline_configure_btn_press(), {
    HTML(paste('<H3>Success!!</H3>', 
               'You have configured the Offline Instrument.',
               '<br>',
               '<br>',
               '<H4>Instrument Information:</H4>',
               '<b>Instrument Name:</b>', offline_project_vars$offline_instrument(),
               '<br>',
               '<b>Identifier Field:</b>', offline_config_vars$offline_identifier(),
               '<br>',
               '<b>Reviewer Field:</b>', offline_config_vars$offline_reviewer(),
               '<br>',
               '<b>Reviewer Name:</b>', offline_config_vars$offline_selected_reviewer(),
               '<br><br>',
               '<b>You may now proceed to record review. Have fun and watch out for bugs!</b>',
               '<br><br>'))
  })
  
  output$offline_configured_ui <- renderUI({
    req(offline_configured_message() )
    tagList(
      offline_configured_message(),
      actionButton(inputId = ns('offline_reconfig'),label = 'Reconfigure Instrument')
    )
  })
  offline_reconfig <- reactive({input$offline_reconfig})
  return(list(
    'offline_reconfig' = offline_reconfig
  ))
}
