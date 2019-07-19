#
# This file contains all elements that are needed to configure ReviewR and render the setup page
#

# Setup Logic----------
  ## Load available data model selections from previously uploaded CDM templates
    data_model_selections <- reactive({
      tibble(filename = list.files(path = 'data_models/')) %>% 
        mutate(data_model = str_remove_all(string = filename, pattern = regex(pattern = '.csv')),
               data_model = str_replace(string = data_model,pattern = '_',replacement = '|')) %>%  # Replace first underscore with a '|'
        separate(col = data_model,into = c('data_model','version'),sep = '\\|', fill = 'right') %>% # Separate based on '|"
        select(data_model) %>% 
        unique() %>% 
        arrange() %>% 
        deframe()
    })

  ## Supported databases
    database_choices <- c('BigQuery' = 'bigquery',
                          'PostgreSQL' = 'pg_sql')

# BigQuery Connection Logic----------
  ## BigQuery Authentication
    ## Let's dance the OAuth :), Hit up google for an authorization code and read from URL on redirect.
    source('lib/oauth_bigquery.R')
    ## Extract the authorization code from the URL, use to create a token when present
    params <- parseQueryString(isolate(session$clientData$url_search))
    
    ## Create an authorization token
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
    available_projects <- reactive({
      if (is.null(params$code))
        return(NULL)
      #updateTabItems(session, inputId = "tabs", selected = "setup") 
      bigrquery::set_access_cred(token())
      #bigrquery::bq_auth(token = token(), cache = F)
      available_projects <- bigrquery::bq_projects()
    })
    
    available_datasets <- eventReactive(input$project_id, {
      if (input$project_id == '')
        return(NULL)
      available_datasets <- bigrquery::bq_project_datasets(input$project_id) %>%
        unlist() %>%
        tibble() %>%
        filter(. != input$project_id)
    })
    observe({
      updateSelectInput(session = session, inputId = 'dataset', choices = available_datasets()$.)
    })

# BigQuery UI----------
  select_project_ui <- reactive({
    if (is.null(params$code))
      return(NULL)
    list(
      div("You have authenticated with Google. Please select from the list of available projects, or sign out and sign in with a different Google Account."),
      br(),
      actionButton(inputId = 'logout',label = 'Sign Out of Google', icon = icon(name = 'sign-out-alt'), onclick=HTML(redirect_home)), # Redirect to home, which will clear any access tokens
      selectInput(inputId = 'project_id',label = 'Select from Available Projects:',
                  choices = available_projects()),
      selectInput(inputId = 'dataset',label = 'Select from Available Datasets:',
                  choices = NULL)
      )
    })
    
  bq_setup <- reactive({
    if(input$db_type == 'bigquery' & is.null(params$code)){
      tagList(
        actionButton(inputId = 'login',label = 'Sign In with Google',icon = icon(name = 'google'), onclick=HTML(redirect)) # Go talk to Google, ask nicely if we can use BigQuery
        )
    } else if(input$db_type == 'bigquery' & is.null(params$code) == F) {
      select_project_ui()
  } else {
      return(NULL)
    }
  })


# Define Overall Setup UI Elements----------
tagList(
  fluidRow(
    box(status = 'primary', solidHeader = F, width = 12,
        h2("ReviewR Setup", style='text-align: center;')
        )
    ),
  fluidRow(
    column(width = 6,
           box(title = 'Connect to Patient Database', width = '100%', status = 'primary', solidHeader = F,
               selectInput(inputId = 'data_model',label = 'Select your data model:',
                           choices = data_model_selections()),
               selectInput(inputId = 'db_type',label = 'Select your database:',
                           choices = database_choices),
               renderUI(bq_setup())
               )
           )
    )
  )