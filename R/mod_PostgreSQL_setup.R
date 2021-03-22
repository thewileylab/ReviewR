# https://github.com/thewileylab/shinyPostgreSQL
# Helpers ----
#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
spg_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
  )
}

# Module Documentation ----
#' PostgreSQL Database Module
#' 
#' @description
#' 
#' This module is designed to guide a user through the process of authenticating with 
#' a PostgreSQL database. The user is visually prompted for typical PostgreSQL connection
#' parameters. User entered information is verified and once authenticated, a 
#' [DBI::dbConnect()] object is returned.
#' 
#' This module consists of the following components:
#' 
#' ## Module UI function
#' 
#' \itemize{
#' \item{`postgresql_setup_ui`}: A uiOutput that allows users to connect to provide
#' connection parameters required to connect to a PostgreSQL database.
#' }
#' ## Module Server function
#' \itemize{
#' \item{`postgresql_setup_server`}: The logic that controls the graphical user 
#' interface, validates user inputs, and returns a [DBI::dbconnect()] object 
#' used to connect to the desired PostgreSQL database.
#' }
#' 
#' @param id The module namespace
#' @name mod_postgres
#' 
#' @return 
#' *postgresql_setup_ui*:
#' \item{tagList}{The shinyPostgreSQL Setup UI}
#' *postgresql_setup_server*:
#' \item{reactiveValues}{
#' \itemize{
#' \item{moduleName}: A string, containing the module moniker.
#' \item{moduleType}: A string, with the module type (what does it do?)
#' \item{setup_ui}: The module setup ui function
#' \item{is_connected}: A string, with module connection status. Valid statuses are
#' 'yes' or 'no'.
#' \item{db_con}: A [DBI::dbConnect] object, containing the user configured PostgreSQL
#' connection information. 
#' }}
#' 
NULL
#> NULL

# UI ----
#' @rdname mod_postgres
#' 
#' @keywords internal
#' 
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
postgresql_setup_ui <- function(id){
  ns <- NS(id)
  tagList(
    spg_add_external_resources(),
    shinydashboard::box(title = 'Connect to PostgreSQL Database',
                        width = '100%',
                        status = 'primary',
                        solidHeader = F,
                        div(id = ns('postgresql_connect_div'),
                            HTML('To connect to a PostgreSQL Database, please provide your credentials.<br><br>'),
                            br(),
                            textInput(inputId = ns('host'), label = 'Hostname:', placeholder = 'e.g., ec2-54-83-201-96.compute-1.amazonaws.com'),
                            textInput(inputId = ns('port'), label = 'Port:', value = 5432),
                            textInput(inputId = ns('dbname'), label = 'Database Name:'),
                            textInput(inputId = ns('schema'), label = 'Schema:', placeholder = 'Optional, if using default "public" schema.'),
                            textInput(inputId = ns('username'), label = 'Username:'),
                            passwordInput(inputId = ns('password'), label = 'Password:'),
                            uiOutput(ns('setup_connect_btn')),
                            uiOutput(ns('setup_connect_error'))
                            ),
                        div(id = ns('postgresql_connect_success_div'),
                            uiOutput(ns('setup_connect_success')) %>% shinycssloaders::withSpinner()
                            )
                        )
    )
  }

# Server ----    
#' @rdname mod_postgres
#' 
#' @keywords internal
#' 
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres dbDisconnect
#' @importFrom magrittr %>% 
#' @importFrom glue glue
#' @importFrom magrittr extract
#' @importFrom shinyjs hide show reset
postgresql_setup_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## PostgreSQL Export Values ----
      postgresql_export <- reactiveValues(
        ### Module Info
        moduleName = 'PostgreSQL',
        moduleType = 'database',
        setup_ui = postgresql_setup_ui,
        ### Connection Variables
        is_connected = 'no',
        db_con = NULL
        )
      
      ## PostgreSQL Setup Values ----
      postgresql_setup <- reactiveValues(
        dbname = NULL,
        schema = NULL,
        host = NULL,
        port = NULL,
        username = NULL,
        db_con_class = NULL
        )
      
      ## Reactive UI Elements ----
      pg_connect_btn <- reactive({
        # Hide the connect button until "something" is added for host, port, dbname, and user. 
          ## Schema not required for successful connections. Default to 'public' schema if not user supplied.
          ## I'm not your mother, use a password or don't. 
        req(input$dbname, 
            input$host,
            input$port, 
            input$username
            )
        # Return NULL until user input is present for the following inputs
        if(input$dbname == '' | input$host == '' | input$port == '' | input$username == '') {
          return(NULL)
          } else {
            actionButton(inputId = ns('pg_connect'), label = 'Connect', icon = icon(name = 'database') )
            }
        })
      
      pg_connect_error <- reactive({
        req(postgresql_setup$db_con_class == 'html')
        postgresql_export$db_con
        })
      
      pg_connected_message <- eventReactive(postgresql_export$is_connected, {
        req(postgresql_export$is_connected == 'yes')
        # Connection Success Message
        HTML(paste('<H3>Success!!</H3>', 
                     'You have connected to the', postgresql_setup$dbname, 'database.',
                     '<br>',
                     '<br>',
                     '<H4>Database Information:</H4>', 
                     '<b>Username:</b>', postgresql_setup$username, '<br>',
                     '<b>Host:</b>', postgresql_setup$host, '<br>',
                     '<b>Port:</b>', postgresql_setup$port, '<br>',
                     '<b>Schema:</b>', postgresql_setup$schema,
                     '<br>')
             )
        })
      
      ## Observe Connect Button ----
      observeEvent(input$pg_connect, {
        # Depending on PostgreSQL config, this tryCatch will be insufficient. Eg, my local PostgreSQL install will accept totally blank
        # connection info as valid, forming a temporary connection. Ideally, this would be combined with dbListTables() to verify that 
        # tables exist before storing a connection object.
        postgresql_export$db_con <- tryCatch({
          if(input$schema != '') {
            DBI::dbConnect(RPostgres::Postgres(),
                           dbname = input$dbname, 
                           host = input$host, # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                           port = input$port, # or any other port specified by your DBA
                           user = input$username,
                           password = input$password,
                           timezone_out = 'UTC',
                           options = glue::glue('-c search_path={input$schema}') ## ensure this works with tbl(con, 'table_name') convention
                           )
            } else {
              DBI::dbConnect(RPostgres::Postgres(),
                             dbname = input$dbname, 
                             host = input$host, # e.g., 'ec2-54-83-201-96.compute-1.amazonaws.com'
                             port = input$port, # or any other port specified by your DBA
                             user = input$username,
                             timezone_out = 'UTC',
                             password = input$password
                             )
              }
          }, error = function(e) {
            message(glue::glue('{e}'))
            connection_error <- HTML(glue::glue("<font color='#e83a2f'>{e}<br>Please verify your PostgreSQL settings. For assistance with parameters, contact your database administrator.</font>"))
            return(connection_error)
            }
          )
        })
      
      ## Monitor DBI Connection Object  ----
      
      ### Check for valid connection information
      # If the conditions are sorted out appropriately in the previous chunk, this work flow should continue to function.
      observeEvent(postgresql_export$db_con, {
        postgresql_setup$db_con_class <- postgresql_export$db_con %>% class() %>% magrittr::extract(1)
        if(postgresql_setup$db_con_class == 'PqConnection') {
          message('PostgreSQL Database Connection Established')
          shinyjs::hide('postgresql_connect_div')
          postgresql_setup$host <- input$host
          postgresql_setup$port <- input$port
          postgresql_setup$schema <- if(input$schema == '') {'public'} else {input$schema}
          postgresql_setup$dbname <- input$dbname
          postgresql_setup$username <- input$username
          postgresql_export$is_connected <- 'yes'
        }
      })
      
      ## Observe disconnect Button ----
      observeEvent(input$pg_disconnect, {
        if ( input$pg_disconnect == 0 ) return()
        RPostgres::dbDisconnect(postgresql_export$db_con) ## Disconnect from database
        postgresql_export$is_connected <- 'no'
        postgresql_export$db_con <- NULL
        message('PostgreSQL Disconnected')
        postgresql_setup$host <- NULL
        postgresql_setup$port <- NULL
        postgresql_setup$dbname <- NULL
        postgresql_setup$schema <- NULL
        postgresql_setup$username <- NULL
        postgresql_setup$db_con_class <- NULL
        shinyjs::show('postgresql_connect_div')
        shinyjs::reset('postgresql_connect_div')
        })
      
      ## PostgreSQL Connection UI Outputs ----
      output$setup_connect_btn <- renderUI({ pg_connect_btn() })
      output$setup_connect_error <- renderUI({ pg_connect_error() })
      output$setup_connect_success <- renderUI({
        req(pg_connected_message() )
        tagList(
          pg_connected_message(),
          actionButton(inputId = ns('pg_disconnect'), label = 'Disconnect')
          )
        })
      
      ## Return ----
      return(postgresql_export)
      
      }
    )
}
