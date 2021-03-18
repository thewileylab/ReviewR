# shinyBigQuery: https://github.com/thewileylab/shinyBigQuery/
# Helpers ----
#' Installed App
#' 
#' Invisibly returns an OAuth app.
#' 
#' @rdname internal-assets
#' @keywords internal
#' 
#' @return An Invisible OAuth consumer application, produced by [httr::oauth_app()]
#' 

installed_app <- function() {
  sbqoa()
}

#' @rdname internal-assets
#' @keywords internal
#' 
#' @noRd
print.hidden_fn <- function(x, ...) {
  x <- 'Nope'
  NextMethod('print')
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useShinydashboard useShinydashboardPlus
#' @noRd
sbq_add_external_resources <- function() {
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::useShinydashboardPlus()
    )
}

# Module Documentation ----
#' Google BigQuery Database Module
#' 
#' @description
#' 
#' This module is designed to guide a user through the process of authenticating with 
#' Google BigQuery. It is responsible for retrieving:
#' \itemize{
#' \item{An OAuth 2.0 authorization token}
#' \item{A list of GCP projects that are available to the authenticated user}
#' \item{A list of BigQuery datasets contained within available projects}
#' }
#' The user is visually guided through the authentication process. Once authenticated, 
#' the user is presented with project/dataset selections and once configured a 
#' [DBI::dbConnect()] object is returned.
#' 
#' This module consists of the following components:
#' 
#' ## Module UI function
#' 
#' \itemize{
#' \item{`bigquery_setup_ui`}: A uiOutput responsible for guiding a user through 
#' the Google OAuth 2.0 authorization flow and graphically selecting a Google Big
#' Query project/dataset.
#' }
#' ## Module Server function
#' \itemize{
#' \item{`bigquery_setup_server`}: The logic that controls the graphical user 
#' interface, including redirecting to Google, receiving an authorization code, 
#' requesting an authorization token, and authenticating the application. 
#' Ultimately responsible for returning public Google user information and a 
#' [DBI::dbconnect()] object used to connect to the configured BigQuery database.
#' }
#' 
#' @param id The module namespace
#' @name mod_bigquery
#' 
#' @return 
#' *bigquery_setup_ui*:
#' \item{tagList}{The Google BigQuery Setup UI}
#' *bigquery_setup_server*:
#' \item{reactiveValues}{
#' \itemize{
#' \item{moduleName}: A string, containing the module moniker.
#' \item{moduleType}: A string, with the module type (what does it do?)
#' \item{setup_ui}: The module setup ui function
#' \item{is_connected}: A string, with module connection status. Valid statuses are
#' 'yes' or 'no'.
#' \item{db_con}: A [DBI::dbConnect] object, containing the user configured BigQuery
#' connection information. 
#' \item{user_info}: A list, containing public user information from Google about 
#' the currently authenticated user.
#' }}
#' 
NULL
#> NULL

# UI ----
#' @rdname mod_bigquery
#' 
#' @keywords internal
#' 
#' @importFrom shinydashboard box
#' @importFrom shinyjs hidden
#' @importFrom shinycssloaders withSpinner
bigquery_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sbq_add_external_resources(),
    fluidRow(
      div(id = ns('google_connect_div'),
          uiOutput(ns('google_connect_ui')) %>% shinycssloaders::withSpinner(),
          style = 'margin-left:15px;margin-right:15px'
          ),
    shinyjs::hidden(
      div(id = ns('google_configured_div'),
          uiOutput(ns('google_configured_ui')) %>% shinycssloaders::withSpinner()
          )
      )
    )
    )
  }

# Server ----
#' @rdname mod_bigquery
#' 
#' @keywords internal
#' 
#' @param secrets_json A string, containing the location of Google secrets json. Defaults 
#' to '/srv/shiny-server/.bq_client_id/client_secret.json' for Shiny Server installations.
#'
#' @importFrom bigrquery bq_auth bq_projects bq_project_datasets bigquery dbDisconnect
#' @importFrom DBI dbConnect
#' @importFrom dplyr filter pull
#' @importFrom gargle token_userinfo
#' @importFrom glue glue
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_authorize_url oauth2.0_token oauth2.0_access_token
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>% 
#' @importFrom purrr flatten
#' @importFrom rlang .data
#' @importFrom shinyjs runjs show hide
#' @importFrom shinydashboardPlus userBox userDescription
#' @importFrom shinyWidgets actionBttn
#' @importFrom tibble enframe
#' @importFrom tidyr unnest
#' 
bigquery_setup_server <- function(id, secrets_json = '/srv/shiny-server/.bq_client_id/client_secret.json') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## Allows redirect to Google for Authentication if JS configured to prevent
      allow_nav_jscode <- 'window.onbeforeunload = null;'
      
      ## BigQuery Setup Values ----
      bigquery_setup <- reactiveValues(
        ### Connection Variables
        bq_projects = NULL,
        bq_project_id = NULL,
        bq_dataset_id = NULL
        )
      
      ## Google Values ----
      google_info <- reactiveValues(
        is_authorized = 'no'
        )
      
      ## BigQuery Export Values ----
      bigquery_export <- reactiveValues(
        ### Module Info
        moduleName = 'BigQuery',
        moduleType = 'database',
        setup_ui = bigquery_setup_ui,
        is_connected = 'no',       
        db_con = NULL,
        user_info = NULL
        )
      
      ## Client URL Information ----
      protocol <- isolate(session$clientData$url_protocol)
      hostname <- if (isolate(session$clientData$url_hostname) == '127.0.0.1') {
        'localhost'
        } else { isolate(session$clientData$url_hostname)
          }
      port <- isolate(session$clientData$url_port)
      pathname <- isolate(session$clientData$url_pathname)
      client_url <- if(is.null(port) | port == '') {
        glue::glue('{protocol}//{hostname}{pathname}')
        } else {
          glue::glue('{protocol}//{hostname}:{port}{pathname}')
          }
      ### Extract any parameters from the URL (anything that isn't one of the things above)
      params <- reactive({ parseQueryString(isolate(session$clientData$url_search)) })
      observeEvent(params(), {
        req(params()$code) 
        google_info$is_authorized <- 'yes'
        })
      ### Platform client_id path
      secrets_json_default <- if(.Platform$OS.type == 'unix') {
        '~/.bq_client_id/client_secret.json' 
        } else {'$HOMEPATH$/.bq_client_id/client_secret.json'}
      
      ## OAuth Dance ----
      #### We can dance if we want to
      ### OAuth 2.0 Client ID using user supplied client_secrets.json
      
      #### Server Installs
      if(hostname != 'localhost' & file.exists(secrets_json) ) { 
        ### Web Authorization using user defined client_secrets.json. Always use path specified by user, if it exists.
        secrets <- jsonlite::fromJSON(txt = file(secrets_json))
        app <- oauth_app(appname = "shinyBigQuery",
                         key = secrets$web$client_id,
                         secret = secrets$web$client_secret,
                         redirect_uri = client_url
                         )
        } else if(hostname != 'localhost' & file.exists('/srv/shiny-server/.bq_client_id/client_secret.json') ) {
          ### Web Authorization using client_secrets.json in prescribed default location.
          secrets <- jsonlite::fromJSON(txt = file('/srv/shiny-server/.bq_client_id/client_secret.json'))
          app <- oauth_app(appname = "shinyBigQuery",
                           key = secrets$web$client_id,
                           secret = secrets$web$client_secret,
                           redirect_uri = client_url
                           )
      #### Local Installs    
        } else if(hostname == 'localhost' & file.exists(secrets_json) ) {
          ### Installed Package Authorization using user defined client_secrets.json. Always use path specified by user, if it exists.
          secrets <- jsonlite::fromJSON(txt = file(secrets_json))
          app <- oauth_app(appname = "shinyBigQuery",
                           key = secrets$installed$client_id,
                           secret = secrets$installed$client_secret,
                           redirect_uri = client_url
                           )
          } else if(hostname == 'localhost' & file.exists(secrets_json_default)) {
            ### Installed Package Authorization using client_secrets.json in prescribed default location.
            secrets <- jsonlite::fromJSON(txt = file(secrets_json_default))
            app <- oauth_app(appname = "shinyBigQuery",
                             key = secrets$installed$client_id,
                             secret = secrets$installed$client_secret,
                             redirect_uri = client_url
                             )
            } else {
              ### Installed Package Authorization using package client_secrets.json
              ### After exhausting all other options, use the credentials installed by the package.
              app <- installed_app()
              }
      
      ### Define Google as the endpoint (this one is canned)
      api <- oauth_endpoints("google")
      
      ### Always request the minimal scope needed. Here, we are requesting:
      ### - Read access to BigQuery
      ### - Read access dev_storage (required to download collect results)
      ### - View your email address
      ### - See your personal info, including any personal info you've made publicly available
      scopes <- "https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/bigquery.readonly https://www.googleapis.com/auth/devstorage.read_only"
      
      ## Google Endpoint
      google_auth_url <- oauth2.0_authorize_url(api, app, scope = scopes)
      redirect <- sprintf("location.replace(\"%s\");", google_auth_url)
      redirect_home <- sprintf("window.location.replace(\"%s\");", client_url)
      
      ### When the login button is pressed, redirect to Google for authentication
      observeEvent(input$sbq_login, ignoreInit = T, {
        #### We can leave this app behind
        shinyjs::runjs( HTML(allow_nav_jscode, redirect) )
        })
      
      ### When the disconnect button is pressed, reset the UI by redirecting to the base url, minus the authorization code
      observeEvent(input$logout, {
        shinyjs::runjs( HTML(allow_nav_jscode, redirect_home) )
        shinyjs::hide(id = 'google_configured_div')
        shinyjs::show(id = 'google_connect_div')
      })
      observeEvent(input$logout_2, {
        shinyjs::runjs( HTML(allow_nav_jscode, redirect_home) )
        shinyjs::hide(id = 'google_configured_div')
        shinyjs::show(id = 'google_connect_div')
      })
      
      ### Create an oauth2.0 token and authenticate with Google, when authorization code is present in client URL. De-authenticate when not present.
      #### But when we come back, we'll run oauth2.0_token()
      observeEvent(google_info$is_authorized, {
        if(google_info$is_authorized == 'no') {
          bigrquery::bq_deauth()
          } else { 
            google_info$token <- oauth2.0_token(app = app,
                                                endpoint = api,
                                                credentials = oauth2.0_access_token(api, app, params()$code),
                                                cache = FALSE
                                                )
            #### And authenticate the UI
            bigrquery::bq_auth(token = google_info$token)
            bigquery_export$user_info <- gargle::token_userinfo(token = google_info$token)
            bigquery_setup$bq_projects <- bigrquery::bq_projects()
          }
        })
      
      ## BQ Setup UI ----
      google_connect_ui <- reactive({
        if(!file.exists(secrets_json) & hostname != 'localhost') {
          tagList(
            shinydashboard::box(title = 'Warning: Application Client Credentials Not Found',
                                width = '100%',
                                status = 'primary',
                                solidHeader = F,
                                HTML('To connect to a Google BigQuery database, please generate a Google OAuth2.0 Client ID and enable access the BigQuery API within your project:<br><br>
                                        <ul>
                                             <li> <a href="https://developers.google.com/identity/protocols/oauth2/web-server" target="_blank" rel="noopener noreferrer">https://developers.google.com/identity/protocols/oauth2/web-server </a></li>
                                        </ul>
                                     Download the client ID JSON as "client_secret.json" and copy it to "/srv/shiny_server/.bq_client_id". Then reload the application.'
                                     ),
                                br()
                                ) 
            )
        } else if(google_info$is_authorized  == 'no') {
          tagList(
            shinydashboard::box(title = 'Connect to BigQuery',
                                width = '100%',
                                status = 'primary',
                                solidHeader = F,
                                HTML('To connect to Google BigQuery, please sign in with your Google Account.<br><br>'),
                                br(),
                                actionButton(inputId = ns('sbq_login'),
                                             label = 'Sign In with Google',
                                             icon = icon(name = 'google')
                                             )
                                )
            )
          } else { 
            tagList(
              div(
                shinydashboardPlus::userBox(
                  title = userDescription(
                    title = bigquery_export$user_info$name,
                    subtitle = bigquery_export$user_info$email,
                    image = bigquery_export$user_info$picture,
                    type = 2),
                  width = 12,
                  status = 'primary',
                  collapsible = FALSE,
                  HTML(glue::glue('{bigquery_export$user_info$given_name}, you have successfully authenticated with Google BigQuery. Please select a dataset from from the list of available projects, or sign out and sign in with a different Google Account.<br><br>')),
                  br(),
                  selectizeInput(inputId = ns('bq_project_id'),
                                 label = 'Select from Available Google Projects:',
                                 choices = bigquery_setup$bq_projects,
                                 options = list(create = FALSE,
                                                placeholder = 'No Available Projects')
                                 ),
                  selectizeInput(inputId = ns('bq_dataset_id'),
                                 label = 'Select from Available BigQuery Datasets:',
                                 choices = NULL
                                 ),
                  shinyjs::hidden(
                    div(
                      id = ns('bq_connect_div'),
                      actionButton(inputId = ns('bq_connect'),label = 'Connect',icon = icon('cloud'))
                      )
                    ),
                  footer = fluidRow(
                    div(
                      actionBttn(inputId = ns('logout'),
                                 label = 'Sign Out of Google',
                                 style = 'jelly',
                                 icon = icon(name = 'sign-out-alt')
                                 ),
                      style="float:right;margin-right:20px"
                      )
                    )
                  ), 
                style = 'margin-left:-15px;margin-right:-15px'
                )
              ) 
            }
        })
      
      ### When the user selects a GCP Project, populate the available dataset selector choices. 
      observeEvent(input$bq_project_id, {
        req(input$bq_project_id)
        dataset_choices <- bigrquery::bq_project_datasets(input$bq_project_id) %>%
          purrr::flatten() %>%
          tibble::enframe() %>%
          dplyr::filter(.data$name == 'dataset') %>%
          tidyr::unnest(.data$value) %>%
          dplyr::pull(.data$value)
        updateSelectizeInput(session = session,
                             inputId = 'bq_dataset_id',
                             choices = dataset_choices,
                             server = T,
                             options = list(create = FALSE,
                                            placeholder = 'Please ensure you have access to a BigQuery dataset in this project.')
                             )
        })
      
      ### When the user selects an available dataset, show the 'connect' button
      observeEvent(input$bq_dataset_id, {
        req(input$bq_dataset_id)
        shinyjs::show(id = 'bq_connect_div')
        })
      
      ### Store configured values as a connection object
      observeEvent(input$bq_connect, {
        bigquery_setup$bq_project_id <- input$bq_project_id
        bigquery_setup$bq_dataset_id <- input$bq_dataset_id
        bigquery_export$db_con <- DBI::dbConnect(drv = bigquery(),
                                                project = bigquery_setup$bq_project_id,
                                                dataset = bigquery_setup$bq_dataset_id
                                                )
        bigquery_export$is_connected <- 'yes'
        shinyjs::hide('google_connect_div')
        shinyjs::show('google_configured_div')
        })
      
      google_configured_ui <- reactive({
        req(bigquery_export$is_connected == 'yes')
        tagList(
          shinydashboardPlus::userBox(
            title = userDescription(
              title = bigquery_export$user_info$name,
              subtitle = bigquery_export$user_info$email,
              image = bigquery_export$user_info$picture,
              type = 2),
            width = 12,
            status = 'primary',
            collapsible = FALSE,
            HTML(paste('<H3>Success!!</H3>',
                       'You have connected to a Google BigQuery database.',
                       '<br>',
                       '<br>',
                       '<H4>Connection Information:</H4>',
                       '<b>Project:</b>', bigquery_setup$bq_project_id,
                       '<br>',
                       '<b>Dataset:</b>', bigquery_setup$bq_dataset_id,
                       '<br>'
                       )
                 ),
            actionButton(inputId = ns('sbq_disconnect'), label = 'Disconnect'),
            footer = fluidRow(
              div(
                actionBttn(inputId = ns('logout_2'),
                           label = 'Sign Out of Google',
                           style = 'jelly',
                           icon = icon(name = 'sign-out-alt')
                           ),
                style="float:right;margin-right:20px"
                )
              )
            )
          )
        })
      
      observeEvent(input$sbq_disconnect, {
        req(class(bigquery_export$db_con)[[1]] == 'BigQueryConnection')
        bigquery_setup$bq_project_id <- NULL
        bigquery_setup$bq_dataset_id <- NULL
        bigquery_export$db_con <- NULL
        bigquery_export$is_connected <- 'no'
        shinyjs::hide('google_configured_div')
        shinyjs::show('google_connect_div')
      })
      
      ## BigQuery Setup Outputs ----
      output$google_connect_ui <- renderUI({ google_connect_ui() })
      output$google_configured_ui <- renderUI({ google_configured_ui() })
      
      # Return Setup Values ----
      return(bigquery_export)
      }
    )
  }
