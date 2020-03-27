offline_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('setup_ui')),
    uiOutput(ns('offline_config_btn'))
  )
}

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

offline_connected_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('offline_connected_ui'))
  )
}
offline_connected_logic <- function(input, output, session, abstraction_vars) {
  ns <- session$ns
  
  offline_info <- reactive({
    req(abstraction_vars$offline_existing_session() )
    browser()
    if(abstraction_vars$offline_existing_session() == 'demo') {
      'Shiny Contest Demo Abstraction'
    } else {
      abstraction_vars$offline_new_session()$name
    }
  })
  
  offline_connected_message <- eventReactive(c(abstraction_vars$offline_press(),abstraction_vars$offline_new_session() ), {
    browser()
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