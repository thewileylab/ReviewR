chart_abstraction_select_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput( ns('chart_abstraction_select'))
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
