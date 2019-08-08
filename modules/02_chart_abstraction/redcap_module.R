redcap_connect_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('redcap_setup')),
    uiOutput(ns('redcap_connect'))
  )
}

redcap_connect_logic <- function(input, output, session, selection) { 
  ns <- session$ns
  
  redcap_setup <- reactive({
    req(selection() )
    if( selection() == 'redcap') {
      tagList(
        textInput(inputId = ns('redcap_url'),label = 'REDCap URL:',value = 'https://'),
        passwordInput(inputId = ns('redcap_token'),label = 'REDCap API Token:')
        
      )
    } else {return(NULL)}
  })
  
  output$redcap_setup <- renderUI({ redcap_setup() })
  
  rc_url <- reactive({input$redcap_url})
  rc_token <- reactive({input$redcap_token})

  return(list(
    'rc_url' = rc_url,
    'rc_token' = rc_token
  ))
}

redcap_initialize_logic <- function(input, output, session, rc_url, rc_token) {
  library(tidyverse)
  library(redcapAPI)
  
  ns <- session$ns
  rc_connect <- reactive({
    req(rc_url(),rc_token())
    if(rc_url() == '' | rc_token() == '') {
      return(NULL)
    } else {
    actionButton(inputId = ns('rc_connect'),label = "Connect to REDCap",icon = icon('notes-medical'))
      }
  })
  
  rc_con <- eventReactive(input$rc_connect, {
    redcapAPI::redcapConnection(url = rc_url(), token = rc_token() )
  })
  
  output$redcap_connect <- renderUI({ rc_connect() })
  
  return(list(
    'rc_con' = rc_con
  ))
  
}
