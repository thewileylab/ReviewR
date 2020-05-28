#' BigQuery Module
#'
#' This module is designed to guide a user through the process of authenticating with Google BigQuery. It is responsible for returning an authorization token, the user selected project,the user selected dataset, and a DBI connection to a BigQuery Dataset.
#'
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_BigQuery_module
#' 
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList parseQueryString isolate
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_authorize_url oauth2.0_token oauth2.0_access_token
#' @importFrom bigrquery bq_auth bq_projects bq_project_datasets bigquery dbDisconnect
#' @importFrom tibble tibble deframe
#' @importFrom dplyr filter
#' @importFrom magrittr %>% 
#' @importFrom DBI dbConnect
#' @importFrom glue glue
bq_auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('bq_connect_project_ui')),
    uiOutput(ns('bq_connect_dataset_ui')),
    uiOutput(ns('bq_init_connection_ui'))
    )
}

# BigQuery Module Server Logic

#' @rdname mod_BigQuery_module
#' @export
#' @keywords internal

bq_project_auth_logic <- function(input, output, session) {
  # Pull the namespace function from the session info to assist in updating selectInputs
  ns <- session$ns
  allow_nav_jscode <- 'window.onbeforeunload = null;'
  
  ## URL Information
  protocol <- isolate(session$clientData$url_protocol)
  hostname <- if (isolate(session$clientData$url_hostname) == '127.0.0.1') {
    'localhost'
  } else { isolate(session$clientData$url_hostname)
                   }
  port <- isolate(session$clientData$url_port)
  pathname <- isolate(session$clientData$url_pathname)
  APP_URL <- if(is.null(port) | port == '') {
    glue::glue('{protocol}//{hostname}{pathname}')
  } else {
    glue::glue('{protocol}//{hostname}:{port}{pathname}')
  }
  params <- parseQueryString(isolate(session$clientData$url_search))
  # browser()
  # if (interactive()) {
  #   # For use locally, ensure the app is running on port 8100
  #   APP_URL <- "http://localhost:8100/"
  # } else {
  #   # deployed URL (server deploymnet)
  #   APP_URL <- "http://localhost:3838/ReviewR/"
  # }
  
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
  scopes <- "https://www.googleapis.com/auth/bigquery.readonly https://www.googleapis.com/auth/devstorage.read_only"

  # Craft some URL's
  url <- oauth2.0_authorize_url(api, app, scope = scopes)
  redirect <- sprintf("location.replace(\"%s\");", url)
  redirect_home <- sprintf("window.location.replace(\"%s\");", APP_URL)
  
  # Create an authorization token and authenticate with google
  token <- reactive({
    req(params$code)
    token <- oauth2.0_token(
      app = app,
      endpoint = api,
      credentials = oauth2.0_access_token(api, app, params$code),
      cache = FALSE
    )
  })
  
  # Create choices for project/dataset selectInputs
  available_projects <- reactive({
    bigrquery::bq_auth(token = token())
    bigrquery::bq_projects()
  })
  
  # Create project/dataset selectInputs, using the namespace function extracted from the session info
  select_project_ui <- reactive({
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
          onclick = HTML(allow_nav_jscode, redirect)
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
          onclick = HTML(allow_nav_jscode, redirect_home)
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

# BigQuery Module Dataset Auth Logic

#' @rdname mod_BigQuery_module
#' @param bq_project A BigQuery Project ID
#' @export
#' @keywords internal
#' @importFrom purrr flatten
#' @importFrom tibble enframe
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @importFrom rlang .data

bq_dataset_auth_logic <- function(input, output, session, bq_project) {
  ns <- session$ns
  
  available_datasets <- eventReactive(bq_project(), {
    bigrquery::bq_project_datasets(bq_project()) %>%
      flatten() %>% 
      enframe %>% 
      filter(.data$name == 'dataset') %>% 
      unnest(.data$value) %>% 
      pull(.data$value)
  })
  
  select_dataset_ui <- reactive({
    req( bq_project() )
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

# BigQuery Module BigQuery Initialize Logic

#' @rdname mod_BigQuery_module
#' @param bq_project A BigQuery Project ID
#' @param bq_dataset A BigQuery Dataset ID
#' @param disconnect An action button press
#' @export
#' @keywords internal

bq_initialize <- function(input, output, session, bq_project, bq_dataset, disconnect) {
  ns <- session$ns
  # Create a connection UI based on the database type, add logic for postgres to hide connect button until required information is present.
  connect_button <- reactive({
    req(bq_project(), bq_dataset() )
    actionButton(inputId = ns('bq_connect'),label = 'Connect',icon = icon('cloud'))
  })
  
  observeEvent(disconnect(), {
    # db_connection <- NULL
    bigrquery::dbDisconnect(db_connection())
  })
  
  output$bq_init_connection_ui <- renderUI({ connect_button() })
  
  # Using information from the connection UI Create and return a connection variable  
  db_connection <- reactive({
    req(bq_project(), bq_dataset())
      DBI::dbConnect(drv = bigrquery::bigquery(), 
                   project = bq_project(),
                   dataset = bq_dataset()
                   )
  })
  
  connect_press <- reactive({ input$bq_connect })
  
  return(list(
    'db_connection' = db_connection,
    'connect_press' = connect_press
  ))
}
