#' Chart Abstraction Setup Module
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_abstraction_setup_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
chart_abstraction_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('chart_abstraction_select')),
    uiOutput(ns('chart_abstraction_setup_ui'))
  )
}

chart_abstraction_select_logic <- function(input, output, session) {
  ns <- session$ns
  abstraction_choices <- c('REDCap' = 'redcap',
                           'Offline' = 'offline')
  abstraction_select_ui <- reactive({
    selectInput(inputId = ns('abstraction_selection'),label = 'Configuration Type:',choices = abstraction_choices) 
    })
  output$chart_abstraction_select <- renderUI({ abstraction_select_ui() })
  
  abstraction_selection <- reactive({ input$abstraction_selection }) 
  return(list(
    'abstraction_selection' = abstraction_selection
  ))
}

# Chart Abstraction Setup  Logic

#' @rdname mod_abstraction_setup_module
#' @param abstraction_selection Which abstraction backend has been selected?
#' @export
#' @keywords internal
chart_abstraction_setup_logic <- function(input, output, session, abstraction_selection) {
  ns <- session$ns
  
  ### REDCap
  source('modules/02_chart_abstraction/redcap_module.R')
  rc_vars <- callModule(redcap_connect_logic, 'abstraction_ns')
  rc_con <- callModule(redcap_initialize_logic, 'abstraction_ns', rc_vars$rc_url, rc_vars$rc_token)
  
  chart_abstraction_setup_ui <- reactive({
    req(abstraction_selection() )
    if(abstraction_selection() == 'redcap') {
      redcap_connect_ui(ns('abstraction_ns'))
    } else if (abstraction_selection() == 'offline') {
      renderUI({
        tagList(
          div('Error!!!'),
          br(),
          div('Offline Module is imaginary at this time. Do something about that eventually.')
        )
      })
    } else {
      return(NULL)
    }
  })
  
  output$chart_abstraction_setup_ui <- renderUI({
    tagList(
      chart_abstraction_setup_ui()
    )
  })
  
  return(list(
    'rc_url' = rc_vars$rc_url,
    'rc_token' = rc_vars$rc_token,
    'rc_con' = rc_con$rc_con,
    'rc_press' = rc_con$rc_connect_press
  ))
}