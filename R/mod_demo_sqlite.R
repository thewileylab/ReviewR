# UI ----
#' Demo SQLite Setup UI
#'
#' This module is designed to guide a user through the process of authenticating your database
#' 
#' @param id The module namespace
#' 
#' @return The Demo SQLite Setup UI
#' @export
#' 
#' @importFrom shiny NS
#' @importFrom shinyjs hidden
#'
demo_sqlite_setup_ui <- function(id) { 
  ns <- NS(id)
  tagList(
    div(id = ns('setup'),
      HTML('Connect to a demonstration SQLite database containing synPUF data.'),
      br(),
      actionButton(inputId = ns('connect'), label = 'Connect', icon = icon(name = 'database')),
      
      ),
    hidden(
      div(id = ns('connected'),
          h3('Success!'),
          actionButton(inputId = ns('disconnect'), label = 'Disconnect')
          )
      )
    )
  }

# Server ----
#' Demo SQLite Setup Server
#'
#' @param id The Module namespace
#'
#' @return Demo SQLite connection variables
#' @export
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite dbDisconnect dbWriteTable SQLite
#' @importFrom tibble as_tibble
#' @importFrom shinyjs hide show
#' @import synPUF
#' 
demo_sqlite_setup_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## demo_sqlite Export Values ----
      demo_sqlite_export <- reactiveValues(
        ### Module Info
        moduleName = 'Demo SQLite',
        moduleType = 'database',
        setup_ui = ReviewR::demo_sqlite_setup_ui,
        is_connected = 'no',       
        db_con = NULL
        )
      # Server Code Here ----
      observeEvent(input$connect, {
        message('creating sqlite db')
        shinyjs::hide('setup')
        ## Create a SQLite DB in memory
        demo_sqlite_export$db_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
        ## Populate with data from synPUF package
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'care_site', value = synPUF::care_site)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'concept', value = synPUF::concept)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'concept_class', value = synPUF::concept_class)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'concept_synonym', value = synPUF::concept_synonym)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'condition_era', value = synPUF::condition_era)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'condition_occurrence', value = synPUF::condition_occurrence)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'death', value = synPUF::death)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'device_exposure', value = synPUF::device_exposure)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'domain', value = synPUF::domain)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'drug_era', value = synPUF::drug_era)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'drug_exposure', value = synPUF::drug_exposure)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'drug_strength', value = synPUF::drug_strength)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'measurement', value = synPUF::measurement)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'note', value = synPUF::note)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'observation', value = synPUF::observation)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'observation_period', value = synPUF::observation_period)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'payer_plan_period', value = synPUF::payer_plan_period)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'person', value = synPUF::person)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'procedure_occurrence', value = synPUF::procedure_occurrence)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'provider', value = synPUF::provider)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'relationship', value = synPUF::relationship)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'visit_occurrence', value = synPUF::visit_occurrence)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'vocabulary', value = synPUF::vocabulary)
        ## Report connected
        demo_sqlite_export$is_connected = 'yes'
        shinyjs::show('connected')
      })
      
      observeEvent(input$disconnect, {
        shinyjs::hide('connected')
        RSQLite::dbDisconnect(demo_sqlite_export$db_con)
        demo_sqlite_export$db_con <- NULL
        demo_sqlite_export$is_connected = 'no'
        shinyjs::show('setup')
      })
      
      # Return ----
      return(demo_sqlite_export)
    }
  )
}