#' Connect to SQL Server UI
#'
#' @param id The namespace id for the UI output
#'
#' @return A Shiny outputUI to step a user through the process of authenticating with SQL Server
#'
#' @examples
#' ui <- fluidPage(
#'   mssql_auth_ui('mssql_setup_ns'),
#'   textOutput('connected')
#' )
#' server <- function(input,output){
#' mssql_connect_vars <- callModule(mssql_auth_logic, id = 'mssql_setup_ns')
#' output$mssql_connect_ui <- renderUI({
#'  mssql_connect_vars$ui()
#' })
#' output$connected <-
#'  renderText({
#'    paste('you have conneted to the',mssql_connect_vars$mssql_database(),'database within the',mssql_connect_vars$mssql_server(), 'server.')
#'  })
#' }
#' shinyApp(ui = ui, server = server)

mssql_auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('mssql_connect_server_ui')),
    uiOutput(ns('mssql_init_connection_ui'))
    )
}

#' Connect to SQL Server Logic
#'
#' @param input Required by Shiny for module operation
#' @param output Required by Shiny for module operation 
#' @param session Required by Shiny for module operation
#'
#' @return A list of reactive reactive objects containing the SQL server name, port, database to connect to, and optional credentials used to authenticate.
#'
#' @examples
#' ui <- fluidPage(
#'   mssql_auth_ui('mssql_setup_ns'),
#'   textOutput('connected')
#' )
#' server <- function(input,output){
#' mssql_connect_vars <- callModule(mssql_server_auth_logic, id = 'mssql_setup_ns')
#' output$mssql_connect_ui <- renderUI({
#'  mssql_connect_vars$ui()
#' })
#' output$connected <-
#'  renderText({
#'    paste('you have conneted to the',mssql_connect_vars$mssql_database(),'database within the',mssql_connect_vars$mssql_server(), 'server.')
#'  })
#' }
#' shinyApp(ui = ui, server = server)

mssql_server_auth_logic <- function(input, output, session) {
  # Load Module Libraries
  library(dplyr)
  library(odbc)
  # Pull the namespace function from the session info to assist in updating selectInputs
  ns <- session$ns

  # Define the reactive SQL Server Setup UI
  mssql_setup_ui <- reactive({
    tagList(
      textInput(inputId = ns('mssql_server'),label = 'Server:',value = '127.0.0.1'),
      textInput(inputId = ns('mssql_port'),label = 'Port:',value = '1433'),
      textInput(inputId = ns('mssql_database'),label = 'Database:',value = 'OHDSI'),
      textInput(inputId = ns('mssql_user'),label = 'User:',value = ''),
      passwordInput(inputId = ns('mssql_password'),label = 'Password:',value = '')
    )
  })

  # Create the output UI
  output$mssql_connect_server_ui <- renderUI({ mssql_setup_ui() })
  
  # Collect the user inputs to pass to other elements of the application
  mssql_server <- reactive({input$mssql_server})
  mssql_port <- reactive({input$mssql_port})
  mssql_database <- reactive({input$mssql_database})
  mssql_user <- reactive({input$mssql_user})
  mssql_password <- reactive({input$mssql_password})
  
  # Also Return a list of objects for use in other parts of the app.
  return(
    list(
      'mssql_server' = mssql_server,
      'mssql_port' = mssql_port,
      'mssql_database' = mssql_database,
      'mssql_user' = mssql_user,
      'mssql_password' = mssql_password
      )
    )
}

mssql_initialize <- function(input, output, session, mssql_server, mssql_port, mssql_database, mssql_user, mssql_password, disconnect) {
  library(DBI)
  library(odbc)

  ns <- session$ns
  # Create a connection UI based on the database type, add logic to hide connect button until required information is present.
  connect_button <- reactive({
    req(mssql_server(), mssql_port(), mssql_database())
    actionButton(inputId = ns('mssql_connect'),label = 'Connect',icon = icon('microsoft'))
  })
  
  observeEvent(disconnect(), {
    db_connection <- NULL
  })
  
  db_connection <- eventReactive(connect_press(), {
    req(mssql_server(), mssql_port(), mssql_database())
    if (mssql_user() != '') {
      odbc::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",
                      Server = mssql_server(),
                      Database = mssql_database(),
                      Uid = mssql_user(),
                      Pwd = mssql_password(),
                      port = mssql_port())
    }
    else {
      odbc::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",
                      Server = mssql_server(),
                      Database = mssql_database(),
                      port = mssql_port())
    }
  })
  
  output$mssql_init_connection_ui <- renderUI({ connect_button() })

  connect_press <- reactive({ input$mssql_connect })
  
  return(list(
    'db_connection' = db_connection,
    'connect_press' = connect_press
  ))
}
  

