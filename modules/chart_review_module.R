patient_nav_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('patient_nav_ui'))
  )
}

patient_nav_logic <- function(input, output, session, patient_table, selected_patient, parent) {
  ns <- session$ns
  
  observeEvent(selected_patient(), {
    req(patient_table())
    updateSelectizeInput(session=parent,
                         inputId = ns('subject_id'),
                         choices = patient_table() %>% 
                           select(ID) %>% 
                           deframe(),
                         selected = selected_patient()$value,
                         server = TRUE )
  })
  output$patient_nav_ui <- renderUI({
    tagList(
      selectizeInput(inputId = ns('subject_id'),
                     width = '100%',
                     label = 'Jump to Subject ID:',
                     choices = NULL,
                     selected = NULL,
                     size = 20
      ),
      actionButton(inputId = 'previous', label = '<--Previous',width = '150px'),
      actionButton(inputId = 'next', label = 'Next-->',width = '150px')
  )
  })
  outputOptions(output, 'patient_nav_ui', suspendWhenHidden = F)
  
  subject_id_val <- reactive({ input$subject_id })
  return(list(
    'subject_id' = subject_id_val
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
    paste('the selected subject is: ', subject() )
    })
  output$subject_info_ui <- renderText({ subject_info_text() })
}