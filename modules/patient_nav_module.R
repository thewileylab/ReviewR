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
      actionButton(inputId = ns('previous_sub'), label = '<--Previous', width = '150px'),
      actionButton(inputId = ns('next_sub'), label = 'Next-->', width = '150px')
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
    uiOutput(ns('subject_info_ui'))
  )
}

subject_info_logic <- function(input, output, session, subject) {
  subject_info_text <- reactive({ 
    req(subject() )
    paste('Subject ID: ', subject() )
    })
  output$subject_info_ui <- renderText({ subject_info_text() })
}