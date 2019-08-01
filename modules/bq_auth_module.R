#' Connect to BigQuery UI
#'
#' @param id The namespace id for the UI output
#'
#' @return A Shiny outputUI to step a user through the process of authenticating with GoogleBigQuery
#'
#' @examples
#' ui <- fluidPage(
#' bq_auth_ui('ba_setup_ns'),
#' textOutput('connected')
#' )
#' server <- function(input,output){
#' bq_connect_vars <- callModule(bq_auth_logic, id = 'bq_setup_ns')
#' output$bq_connect_ui <- renderUI({
#'  bq_connect_vars$ui()
#' })
#' output$connected <-
#'  renderText({
#'    paste('you have conneted to the',bq_connect_vars$bq_dataset(),'dataset within the',bq_connect_vars$bq_project(), 'project.')
#'  })
#' }
#' shinyApp(ui = ui, server = server)

bq_auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('bq_connect_project_ui')),
    uiOutput(ns('bq_connect_dataset_ui')),
    uiOutput(ns('bq_init_connection_ui'))
    )
}

#' Connect to BigQuery Server Logic
#'
#' @param input Required by Shiny for module operation
#' @param output Required by Shiny for module operation 
#' @param session Required by Shiny for module operation
#'
#' @return A list of reactive reactive objects containing a an authorization token, the user selected project, and the user selected dataset
#'
#' @examples
#' ui <- fluidPage(
#' bq_auth_ui('ba_setup_ns'),
#' textOutput('connected')
#' )
#' server <- function(input,output){
#' bq_connect_vars <- callModule(bq_auth_logic, id = 'bq_setup_ns')
#' output$bq_connect_ui <- renderUI({
#'  bq_connect_vars$ui()
#' })
#' output$connected <-
#'  renderText({
#'    paste('you have conneted to the',bq_connect_vars$bq_dataset(),'dataset within the',bq_connect_vars$bq_project(), 'project.')
#'  })
#' }
#' shinyApp(ui = ui, server = server)

bq_project_auth_logic <- function(input, output, session) {
  # Load Module Libraries
  library(dplyr)
  library(bigrquery)
  library(httr)
  # Pull the namespace function from the session info to assist in updating selectInputs
  ns <- session$ns
  
  params <- parseQueryString(isolate(session$clientData$url_search))
  
  if (interactive()) {
    # For use locally, ensure the app is running on port 8100
    APP_URL <- "http://localhost:8100/"
  } else {
    # deployed URL (server deploymnet)
    APP_URL <- "http://localhost:3838/ReviewR/"
  }
  
  # Note that secret is not really secret, and it's fine to include inline
  app <- oauth_app(
    appname = "ReviewR_Google_Auth",
    key = "241672600185-vbmn82ge65skhe9gd9uihkah51eir82l.apps.googleusercontent.com",
    ## coursera-development-admin project, currently
    secret = "H_i96QZYXzkgKxIcCWt21VfY",
    redirect_uri = APP_URL
  )
  
  # Define Google as the endpoint (this one is canned)
  api <- oauth_endpoints("google")
  
  # Always request the minimal scope needed. Here, we are requesting access to BigQuery
  scope <- "https://www.googleapis.com/auth/bigquery"
  
  # Craft some URL's
  url <- oauth2.0_authorize_url(api, app, scope = scope)
  redirect <- sprintf("location.replace(\"%s\");", url)
  redirect_home <- sprintf("window.location.replace(\"%s\");", APP_URL)
  
  # Create an authorization token and authenticate with google
  token <- reactive({
    if (is.null(params$code))
      return(NULL)
    token <- oauth2.0_token(
      app = app,
      endpoint = api,
      credentials = oauth2.0_access_token(api, app, params$code),
      cache = FALSE
    )
  })
  
  # Create choices for project/dataset selectInputs
  available_projects <- reactive({
    if (is.null(params$code))
      return(NULL)
    # Authenticate session with Google, Gargle is unnessecarily verbose at it's current version, hence suppressWarnings()
    #suppressWarnings(bigrquery::bq_auth(token = token()))
    bigrquery::bq_auth(token = token())
    bigrquery::bq_projects()
  })
  
  # Create project/dataset selectInputs, using the namespace function extracted from the session info
  select_project_ui <- reactive({
    if(is.null(params$code))
      return(NULL)
    selectInput(inputId = ns('bq_project_id'),label = 'Select from Available Google Projects:',choices = available_projects())
  })
  
  
  # Collect the user inputs to pass to other elements of the application
  bq_project <- reactive({input$bq_project_id})
  
  # Define the reactive BQ Setup UI
  bq_setup_ui <- reactive({
    if (is.null(params$code)) {
      tagList(
        # Create an action button to redirect to Google, ask nicely if we can use BigQuery
        actionButton(
          inputId = 'login',
          label = 'Sign In with Google',
          icon = icon(name = 'google'),
          onclick = HTML(redirect)
        ) 
      )
    } else {
      # Create a UI offering a disconnect or selecting a project and dataset.
      tagList(
        div(
          "You have authenticated with Google BigQuery. Please select from the list of available projects, or sign out and sign in with a different Google Account."
        ),
        br(),
        # Create an action button to redirect to application home, which will clear any access tokens
        actionButton(
          inputId = 'logout',
          label = 'Sign Out of Google',
          icon = icon(name = 'sign-out-alt'),
          onclick = HTML(redirect_home)
        ),
        ## Otherwise, walk the user through selecting a project using reactive selectInput
        select_project_ui()
      )
    }
    })
  # Create the output UI
  output$bq_connect_project_ui <- renderUI({ bq_setup_ui() })
  
  # Also Return a list of objects for use in other parts of the app. Keep the dataset separate so that bigrquery joins can be performed across datasets.
  return(
    list(
      'token'= token,
      'bq_project' = bq_project
      )
    )
}

bq_dataset_auth_logic <- function(input, output, session, bq_project) {
  library(dplyr)
  library(tibble)
  library(bigrquery)
  
  ns <- session$ns
  
  available_datasets <- eventReactive(bq_project(), {
    # if( is.null(bq_project) )
    #   return(NULL)
    bigrquery::bq_project_datasets(bq_project()) %>%
      unlist() %>%
      tibble() %>%
      filter(. != bq_project()) %>% 
      deframe()
  })
  
  select_dataset_ui <- reactive({
    if(is.null(bq_project()) )
      return(NULL)
    selectInput(inputId = ns('bq_project_dataset'),label = 'Select from Available BigQuery Datasets:',choices = available_datasets())
  })
  
  bq_dataset <- reactive({input$bq_project_dataset})
  
  output$bq_connect_dataset_ui <- renderUI({ select_dataset_ui() })
  
  return(
    list(
      'bq_dataset' = bq_dataset
    )
  )
}

bq_initialize <- function(input, output, session, bq_project, bq_dataset) {
  library(DBI)
  library(pool) #https://www.r-bloggers.com/pool-package-on-cran/
  
  ns <- session$ns
  # Create a connection UI based on the database type, add logic for postgres to hide connect button until required information is present.
  connect_button <- reactive({
    if( is.null( bq_project() ) | is.null( bq_dataset() )) {
      return(NULL)
    } else {actionButton(inputId = ns('bq_connect'),label = 'Connect')}
  })
  
  output$bq_init_connection_ui <- renderUI({ connect_button() })
  
  # Using information from the connection UI Create and return a connection variable  
  db_connection <- eventReactive(input$bq_connect, {
    if(is.null( bq_project() ) | is.null( bq_dataset() )) {
      return(NULL)
    } else {
      pool::dbPool(drv = bigrquery::bigquery(), 
                   project = bq_project(),
                   dataset = bq_dataset()
                   )
      } 
  })
  
  return(list(
    'db_connection' = db_connection
  ))
}
  

