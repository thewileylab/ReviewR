# Datasets ----
# Demo data from: https://github.com/thewileylab/synPUF/releases/tag/0.0.1.10
#' synPUF
#' 
#' @description 
#' ## Overview
#' 
#' This dataset contains a 10 person subset of the CMS 2008-2010 Data Entrepreneurs’ 
#' Synthetic Public Use File (DE-SynPUF) from OHDSI. 
#' 
#' ## Details
#' 
#' * \href{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF.html}{Information on the SynPUF data source is available here}
#' * \href{https://github.com/OHDSI/CommonDataModel/blob/v5.2.2/PostgreSQL/OMOP%20CDM%20ddl%20-%20PostgreSQL.sql}{CDM 5.2.2 DDL for the OHDSI supported DBMSs is available here}
#' * \href{https://www.mtsamples.com/}{Notes Obtained from MTSamples.com}
#' 
#' 
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 23 rows and 2 variables:
#' \describe{
#'   \item{table_name}{\emph{character}: The table name in OMOP v5.2.2}
#'   \item{table_data}{\emph{list}: The table data, in OMOP v5.2.2}
#'   ...
#' }
#' @source 
#' * \url{https://github.com/thewileylab/synPUF/releases/tag/0.0.1.10}
#' * \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF.html}
#' * \url{https://github.com/OHDSI/CommonDataModel/blob/v5.2.2/PostgreSQL/OMOP%20CDM%20ddl%20-%20PostgreSQL.sql}
#' * \url{https://www.mtsamples.com/}
"synPUF"

# UI ----
#' Demo SQLite Setup UI
#'
#' This module is designed to guide a user through the process of authenticating your database
#' 
#' @param id The module namespace
#' 
#' @return The Demo SQLite Setup UI
#' @keywords internal
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
          HTML("This demonstration database module contains a 10 person subset of the CMS 2008-2010 Data Entrepreneurs' Synthetic Public Use File (DE-SynPUF) from OHDSI."),
          h4('Details'),
          HTML('<ul>
                <li><a href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF.html" target="_blank" rel="noopener noreferrer">SynPUF Dataset Information</a></li>
                <li><a href="https://github.com/OHDSI/CommonDataModel/blob/v5.2.2/PostgreSQL/OMOP%20CDM%20ddl%20-%20PostgreSQL.sql" target="_blank" rel="noopener noreferrer">OMOP CDM 5.2.2 DDL for OHDSI supported DBMSs</a></li>
                <li><a href="https://www.mtsamples.com/" target="_blank" rel="noopener noreferrer">Notes Obtained from MTSamples.com</a></li>
                <li><a href="https://github.com/thewileylab/synPUF" target="_blank" rel="noopener noreferrer">R Dataset ETL Process</a></li>
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
#' @keywords internal
#' @export
#'
#' @importFrom DBI dbConnect
#' @importFrom purrr map2
#' @importFrom RSQLite dbDisconnect dbWriteTable SQLite
#' @importFrom shinyjs hide show
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
        map2(.x = ReviewR::synPUF$table_name, .y = ReviewR::synPUF$table_data, ~ dbWriteTable(conn = demo_sqlite_export$db_con, name = .x, value = .y) )
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