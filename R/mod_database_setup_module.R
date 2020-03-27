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
  db_choices <- c('SQLite (Example Data)' = 'sq_lite',
                  'Others' = 'other')
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

# Load SQLite Module
  sq_connection <- callModule(sqlite_logic, id = 'sqlite_setup_ns', db_disconnect)

  db_connection_ui <- reactive({
  req(db_type() )
    if (db_type() == 'sq_lite') {
      tagList(
        sqlite_ui(ns('sqlite_setup_ns'))
        )
      } else {
        renderUI({
          tagList(
            HTML(paste('Other databases supported by the <a href="https://cran.r-project.org/web/packages/dbplyr/vignettes/dbplyr.html">dbplyr</a> backend. Modules have been developed for Google BigQuery and Microsoft SQL server. <br><br>Visit the <a href="https://github.com/thewileylab/ReviewR">ReviewR GitHub</a> for additional information.') )
            )
        })
        }
    })
  
  output$db_connect_ui <- renderUI({ 
    tagList(
      db_connection_ui()
      ) 
    })
  
  return(list(
    'db_connection' = sq_connection$db_connection,
    'connect_press' = sq_connection$connect_press
  ))
}
