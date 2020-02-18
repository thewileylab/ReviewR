# Define Database Setup UI Elements----------
#' Database Setup Module
#'
#' A Shiny module to step a user through the process of selecting a database and calling the appropriate module based on selection.
#'
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_database_setup_module
#'
#' @keywords internal
#' @export
db_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('db_select_ui')),
    uiOutput(ns('db_connect_ui'))
    )
}

# Setup Logic----------

#' @rdname mod_database_setup_module
#' 
#' @keywords internal
#' @export 

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

#' @param db_type Database type selection
#' @param db_disconnect An Action button press
#' 
#' @rdname mod_database_setup_module
#' 
#' @keywords internal
#' @export 

db_connect_logic <- function(input, output, session, db_type, db_disconnect){
  ns <- session$ns
  
# Load BigQuery Auth Module
  bq_prj_connect_vars <- callModule(bq_project_auth_logic, id = 'bq_setup_ns')
  bq_ds_connect_vars <- callModule(bq_dataset_auth_logic, id = 'bq_setup_ns', bq_prj_connect_vars$bq_project)
  db_connection <- callModule(bq_initialize, id = 'bq_setup_ns', bq_prj_connect_vars$bq_project, bq_ds_connect_vars$bq_dataset, db_disconnect)
  
  db_connection_ui <- reactive({
  req(db_type() )
    if(db_type() == 'bigquery') {
      tagList(
        bq_auth_ui(ns('bq_setup_ns'))
      )
  } else if(db_type() == 'pg_sql') {
    renderUI({
      tagList(
        div('Error!!!'),
        br(),
        div('PostgreSQL Module is imaginary at this time. Do something about that eventually.') 
      )
    })
    } else { return(NULL)}
  })
  
  output$db_connect_ui <- renderUI({ 
    tagList(
      db_connection_ui()
      ) 
    })

  return(list(
    'bq_token' = bq_prj_connect_vars$token,
    'bq_project' = bq_prj_connect_vars$bq_project,
    'bq_dataset' = bq_ds_connect_vars$bq_dataset, 
    'db_connection' = db_connection$db_connection,
    'connect_press' = db_connection$connect_press
  ))
}
