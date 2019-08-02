#
# This file contains all elements that are needed to configure ReviewR and render the setup page
#

# Define Database Setup UI Elements----------
db_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('db_select_ui')),
    uiOutput(ns('db_connect_ui'))
    )
}

db_success_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput()
  )
}

# Setup Logic----------
  db_select_logic <- function(input, output, session) {
  ns <- session$ns

# Supported databases
  db_choices <- c('BigQuery' = 'bigquery',
                  'PostgreSQL' = 'pg_sql')
  db_ui <- 
    tagList(
      div("Please select a database type from the list of supported databases."),
      br(),
      selectInput(
        inputId = ns('db_type'),
        label = 'Database type:',
        choices = db_choices,
        selected = 'bigquery')
    )

  db_selection <- reactive({input$db_type})
  output$db_select_ui <- renderUI({ db_ui })
  
  return(list(
    'db_selection' = db_selection
    )
  )
}

db_connect_logic <- function(input, output, session, db_type){
  ns <- session$ns
  
# Load BigQuery Auth Module
  source('modules/bq_auth_module.R')
  bq_prj_connect_vars <- callModule(bq_project_auth_logic, id = 'bq_setup_ns')
  bq_ds_connect_vars <- callModule(bq_dataset_auth_logic, id = 'bq_setup_ns', bq_prj_connect_vars$bq_project)
  db_connection <- callModule(bq_initialize, id = 'bq_setup_ns', bq_prj_connect_vars$bq_project, bq_ds_connect_vars$bq_dataset)
  
  db_connection_ui <- reactive({
    if(is.null(db_type())) {
      return(NULL)
    } else if(db_type() == 'bigquery') {
      tagList(
        bq_auth_ui(ns('bq_setup_ns'))
      )
  } else { 
    tagList(
      actionButton(inputId = 'action_jackson',label = 'Postgres Placeholder')
    )
    }
  })
  
  output$db_connect_ui <- renderUI({ 
    tagList(
      db_connection_ui()
      ) 
    })
  return(list(
    'bq_project' = bq_prj_connect_vars$bq_project,
    'bq_dataset' = bq_ds_connect_vars$bq_dataset, 
    'db_connection' = db_connection$db_connection
  ))
}

