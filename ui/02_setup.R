#
# This file contains all elements that are needed to configure ReviewR and render the setup page
#

# db_setup_wtf <- function(id){
#   ns <- NS(id)
#   
# Setup Logic----------
  ## Load available data model selections from previously uploaded CDM templates
    data_model_selections <- reactive({
      tibble(filename = list.files(path = 'data_models/')) %>%
        mutate(data_model = str_remove_all(string = filename, pattern = regex(pattern = '.csv')),
               data_model = str_replace(string = data_model,pattern = '_',replacement = '|')) %>%  # Replace first underscore with a '|'
        separate(col = data_model,into = c('data_model','version'),sep = '\\|', fill = 'right') %>% # Separate based on '|"
        select(data_model) %>%
        unique() %>%
        arrange() %>%
        deframe()
    })

  ## Supported databases
    database_choices <- c('BigQuery' = 'bigquery',
                          'PostgreSQL' = 'pg_sql')
    
# Load BigQuery Connection Module    
bq_connect_vars <- callModule(bq_auth_logic, id = 'bq_setup_ns')
    

# Define Database Setup UI Elements----------
tagList(
  fluidRow(
    box(status = 'primary', solidHeader = F, width = 12,
        h2("ReviewR Setup", style='text-align: center;')
        )
    ),
  fluidRow(
    column(width = 6,
           box(title = 'Connect to Patient Database', width = '100%', status = 'primary', solidHeader = F,
               selectInput(inputId = 'data_model',label = 'Select your data model:',
                           choices = data_model_selections()),
               selectInput(inputId = 'db_type',label = 'Select your database:',
                           choices = database_choices),
               bq_auth_ui('bq_setup_ns')
               )
           )
    )
  )
    
    
    
    
    
    