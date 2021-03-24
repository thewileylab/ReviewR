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
#' * \href{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}{Information on the SynPUF data source is available here}
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
#' * \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
#' * \url{https://github.com/OHDSI/CommonDataModel/blob/v5.2.2/PostgreSQL/OMOP%20CDM%20ddl%20-%20PostgreSQL.sql}
#' * \url{https://www.mtsamples.com/}
"synPUF"

# Module Documentation ----
#' Demo SQLite Database Module
#' 
#' @description
#' 
#' This module will create an in memory SQLite database with demo data from
#' the CMS 2008-2010 Data Entrepreneurs’ Synthetic Public Use File (DE-SynPUF) 
#' from OHDSI. It will allow you to preview the functionality of ReviewR
#' if you do not have access to a database of patient information.
#' 
#' This module consists of the following components:
#' 
#' ## Module UI function
#' 
#' \itemize{
#' \item{`demo_sqlite_setup_ui`}: A uiOutput that allows users to connect to 
#' the demonstration database.
#' }
#' ## Module Server function
#' \itemize{
#' \item{`demo_sqlite_setup_server`}: The logic that creates the demonstration
#' SQLite database and returns a [DBI::dbConnect()] object used to connect 
#' to the demo database.
#' }
#' 
#' @param id The module namespace
#' @name mod_demo_sqlite
#' 
#' @return 
#' *demo_sqlite_setup_ui*:
#' \item{tagList}{The Demo SQLite Setup UI}
#' *demo_sqlite_setup_server*:
#' \item{reactiveValues}{
#' \itemize{
#' \item{moduleName}: A string, containing the module moniker.
#' \item{moduleType}: A string, with the module type (what does it do?)
#' \item{setup_ui}: The module setup ui function
#' \item{is_connected}: A string, with module connection status. Valid statuses are
#' 'yes' or 'no'.
#' \item{db_con}: A [DBI::dbConnect] object, containing the user configured 
#' connection information. 
#' }}
#' 
NULL
#> NULL

# UI ----
#' @rdname mod_demo_sqlite 
#' 
#' @keywords internal
#' 
#' @importFrom shiny NS
#' @importFrom shinyjs hidden
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
                <li><a href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF" target="_blank" rel="noopener noreferrer">SynPUF Dataset Information</a></li>
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
#' @rdname mod_demo_sqlite
#' 
#' @keywords internal
#'
#' @importFrom DBI dbConnect
#' @importFrom purrr map2
#' @importFrom RSQLite dbDisconnect dbWriteTable SQLite
#' @importFrom shinyjs hide show
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
        setup_ui = demo_sqlite_setup_ui,
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