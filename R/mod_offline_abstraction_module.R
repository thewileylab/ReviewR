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
    'offline_disconnect' = offline_disconnect
  ))
}