#
# This file contains all elements that are needed to configure ReviewR and render the setup page
#

# Define Database Setup UI Elements----------
db_setup_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('db_select_ui'))
}

db_connect_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('db_connect_ui'))
}

# Setup Logic----------
  db_setup_logic <- function(input, output, session) {
  ns <- session$ns

# Load available data model selections from previously uploaded CDM templates
  data_model_selections <-
    tibble(filename = list.files(path = 'data_models/')) %>%
      mutate(
        data_model = str_remove_all(string = filename, pattern = regex(pattern = '.csv')),
        data_model = str_replace(
          string = data_model,
          pattern = '_',
          replacement = '|'
        )
      ) %>%  # Replace first underscore with a '|'
      separate(
        col = data_model,
        into = c('data_model', 'version'),
        sep = '\\|',
        fill = 'right'
      ) %>% # Separate based on '|"
      select(data_model) %>%
      unique() %>%
      arrange() %>%
      deframe()
  
# Supported databases
  db_choices <- c('BigQuery' = 'bigquery',
                  'PostgreSQL' = 'pg_sql')
  db_ui <- 
    tagList(
      selectInput(
        inputId = ns('data_model'),
        label = 'Select your data model:',
        choices = data_model_selections
      ),
      selectInput(
        inputId = ns('db_type'),
        label = 'Select your database:',
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
  bq_connect_vars <- callModule(bq_auth_logic, id = 'bq_setup_ns')
  
# Create a connection UI based on the database type
  db_connection_ui <- reactive({
    if(is.null(db_type())) {
      return(NULL)
    } else if(db_type() == 'bigquery') {
      bq_auth_ui(ns('bq_setup_ns'))
  } else {
    actionButton(inputId = 'action_jackson',label = 'Postgres Placeholder') }
  })
  
  output$db_connect_ui <- renderUI({ tagList(
    renderText(db_type()),
    db_connection_ui()
    ) 
    })
}

