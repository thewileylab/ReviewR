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
    div(id = ns('demodb_setup'),
      HTML('Connect to a demonstration SQLite database containing synPUF data.'),
      br(),
      br(),
      actionButton(inputId = ns('demodb_connect'), label = 'Connect', icon = icon(name = 'database')),
      ),
    hidden(
      div(id = ns('demodb_connected'),
          h3('Success!'),
          HTML('This demonstration database module contains a 50 person subset of the CMS 2008-2010 Data Entrepreneurs’ Synthetic Public Use File (DE-SynPUF) from OHDSI.'),
          h4('Details'),
          HTML('<ul>
                <li><a href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF.html" target="_blank">Information on the SynPUF data source is available here</a></li>
                <li><a href="https://github.com/OHDSI/CommonDataModel/blob/v5.2.2/PostgreSQL/OMOP%20CDM%20ddl%20-%20PostgreSQL.sql" target="_blank">CDM 5.2.2 DDL for the OHDSI supported DBMSs is available here</a></li>
                <li><a href="https://www.mtsamples.com/" target="_blank">Notes Obtained from MTSamples.com</a></li>
                <li><a href="https://github.com/thewileylab/synPUF" target="_blank">R Package ETL Process</a></li>
                </ul>'
               ),
          br(),
          actionButton(inputId = ns('demodb_disconnect'), label = 'Disconnect')
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
      observeEvent(input$demodb_connect, {
        message('creating sqlite db')
        shinyjs::hide('demodb_setup')
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
        shinyjs::show('demodb_connected')
      })
      
      observeEvent(input$demodb_disconnect, {
        req(class(demo_sqlite_export$db_con)[1] == 'SQLiteConnection')
        shinyjs::hide('demodb_connected')
        RSQLite::dbDisconnect(demo_sqlite_export$db_con)
        demo_sqlite_export$db_con <- NULL
        demo_sqlite_export$is_connected = 'no'
        shinyjs::show('demodb_setup')
      })
      
      # Return ----
      return(demo_sqlite_export)
    }
  )
}