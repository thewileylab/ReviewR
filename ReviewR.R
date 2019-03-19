# ReviewR: A light-weight, portable tool for reviewing individual patient records
#
# https://zenodo.org/badge/latestdoi/140004344
#
# This is a shiny tool that allows for manual review of the MIMIC-III (https://mimic.physionet.org/) or 
# OMOP (https://www.ohdsi.org/data-standardization/the-common-data-model/) database.  Current support is
# for Postgres and Google BigQuery, with more to come in the future. 
#
# This is a work in progress and thus there are no guarantees of functionality or accuracy. Use at your own risk.


source('lib/reviewr-core.R')

# We will make sure all required packages are installed and loaded
check.packages(c("shiny", "shinyjs", "shinydashboard", "shinycssloaders",
                 "tidyverse", "DT", "dbplyr", "magrittr", "readr", "configr"))

# Lookups for mapping selection values to a display value in the UI
data_model_display_name = list("omop" = "OMOP", "mimic" = "MIMIC-III")
database_display_name = list("postgres" = "PostgreSQL", "bigquery" = "BigQuery")

## Survey complete choices
redcap_survey_complete_values <- c(0,1,2)
names(redcap_survey_complete_values) <- c('Incomplete', 'Unverified','Complete')

# Define server logic 
server <- function(input, output, session) {
  options("httr_oob_default" = TRUE)

  # Attempt to load the ReviewR connection configuration from config.yml.  If it doesn't exist, the rest of the UI
  # will render to allow the user to specify the database connection details.
  reviewr_config <- load_reviewr_config()
  
  # Initialize a collection of reactive values.  These are going to be used across our two different connection
  # methods (through config.yml, or from a form) to trigger UI updates.
  values <- reactiveValues()
  values$all_people_data = NULL
  
  render_data_tables <- NULL

  output$subject_id_status_redcap <- renderUI({
    div(class="subject_id_header",
      span(paste(values$redcap_text_fields %>% filter(field_name == input$redcap_patient_id) %>% select(field_label), " - "), class="subject_id_label"),
      span(input$subject_id, class="subject_id_label_value"),
      img(id="subject_review_status", src = if (input$redcap_survey_status == 0) { "inprogress.png" }
                                            else if (input$redcap_survey_status == 1) { "incomplete.png" }
                                            else { "complete.png" })
    )#div
  }) #renderUI subject_id_status_redcap
  
  output$subject_id_status <- renderUI({
    div(class="subject_id_header",
        span("Record ID - ", class="subject_id_label"),
        span(input$subject_id, class="subject_id_label_value")
    )#div
  }) #renderUI subject_id_status

  # Create the panel that hosts navigating between patient records
  output$patient_navigation_list_redcap <- renderUI({
    div(style="min-width: 300px",
      div(column(style="padding: 0px; text-align: left;",width=6, actionButton(inputId = 'prev_patient',label = '<< Previous')),
          column(style="padding: 0px; text-align: right;",width=6, actionButton(inputId = 'next_patient',label = 'Next >>'))),
      div(style="padding: 0px; text-align: left;",
          span(values$redcap_text_fields %>% filter(field_name == input$redcap_patient_id) %>% select(field_label)),
          br(),
          selectInput("subject_id", label = NULL, selectize = FALSE, selected = input$subject_id, choices = values$all_people_data$ID)
      ) #div
    ) #div
  })
  output$patient_navigation_list <- renderUI({
    div(style="min-width: 300px",
        div(column(style="padding: 0px; text-align: left;",width=6, actionButton(inputId = 'prev_patient',label = '<< Previous')),
            column(style="padding: 0px; text-align: right;",width=6, actionButton(inputId = 'next_patient',label = 'Next >>'))),
        div(style="padding: 0px; text-align: left;",span("Record ID"), br(),
          selectInput("subject_id", label = NULL, selectize = FALSE, selected = input$subject_id, choices = values$all_people_data$ID))
    ) #div
  })
  
  output$redcap_patient_id_field <- renderUI({
    selectInput("redcap_patient_id", label = "Which variable contains your record identifier (e.g., MRN, subject ID)?", selected = input$redcap_patient_id,
                choices = append("", values$redcap_text_fields_selection_list))
  })
  
  output$redcap_reviewer_id_field <- renderUI({
    selectInput("redcap_reviewer_id", label = "Which variable contains your reviewer identifier?", selected = input$redcap_reviewer_id,
                choices = append("(Not applicable)", values$redcap_text_fields_selection_list))
  })
  
  # Handle the rendering of our mapped REDCap fields to the appropriate UI widgets
  output$redcap_instrument <- renderUI({
    lapply(1:nrow(values$redcap_instrument), function(i) {
      render_redcap(values$redcap_instrument[i,])
    })
  })
  
  # Because we want to use uiOutput for the patient chart panel in multiple locations in the UI,
  # we need to implement the workaround described here (https://github.com/rstudio/shiny/issues/743)
  # where we actually render multiple outputs for each use.
  patient_chart_panel = reactive({
    table_names <- get_review_table_names(values$data_model)
    tabs <- lapply(table_names, function(id) { create_data_panel(id, paste0(id, "_tbl"))})
    panel <- div(
      fluidRow(column(12, do.call(tabsetPanel, tabs)))
    ) #div
    panel
  })
  
  output$patient_chart_panel_abstraction <- renderUI({ 
    fluidRow(
      box(width=9, status = "primary", solidHeader = FALSE, patient_chart_panel()),
      box(width=3, status = "danger", solidHeader = FALSE,
          uiOutput('redcap_instrument'),
          selectInput(inputId = 'redcap_survey_status',label = 'Form Complete?',choices = redcap_survey_complete_values),
          actionButton(inputId ='redcap_upload_survey',label = 'Upload to REDCap')
      ) #box
    ) #fluidRow
  })
  output$patient_chart_panel_no_abstraction <- renderUI({
    fluidRow(
      box(width=12, status = "primary", solidHeader = FALSE, patient_chart_panel())
    ) #fluidRow
  })

  observeEvent(input$viewPatients, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "patient_search")
  })
  
  toggleShinyDivs("redcap_connection_fields", "redcap_connection_status")
  toggleShinyDivs("redcap_configure_status", "redcap_configure_fields")
  toggleShinyDivs("db_connection_fields", "db_connection_status")
  
  # Set up our default sidebar menu, which only has a welcome screen and connection/setup
  initial_menu <- reactive({
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Setup", tabName = "setup", icon = icon("cog"))
    )
  })
  # Because there are a few paths by which the full menu can get activated, we're going to save it
  # off here for easy calling (and to avoid duplicating code later)
  full_menu <- reactive({sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Setup", tabName = "setup", icon = icon("cog")),
    menuItem("Patient Search", tabName = "patient_search", icon = icon("users")),
    menuItem("Chart Review", icon = icon("table"), tabName = "chart_review")
  )})
  
  output$menu <- renderMenu({ initial_menu() })
  
  # If during initial setup of the server we have configuration data, attempt to use it to initialize
  # the application, including establishing a connection to the underlying database.
  if (!is.null(reviewr_config) && length(reviewr_config) > 0) {
    reviewr_config <- initialize(reviewr_config)
    values$data_model <- reviewr_config$data_model
    render_data_tables = get_render_data_tables(reviewr_config$data_model)
    values$all_people_data = get_all_people_for_list(reviewr_config$data_model)(reviewr_config)
    output = render_data_tables(input, output, reviewr_config)
    
    toggleShinyDivs("db_connection_status", "db_connection_fields")
    
    output$menu <- renderMenu({ full_menu() })
    
    # Optionally, there may be REDCap connection information included.  If so, we will go ahead and
    # get that all loaded as well.
    if (!is.null(reviewr_config$redcap_api_token)) {
      reviewr_config <- initialize_redcap(reviewr_config)
      toggleShinyDivs("redcap_connection_status", "redcap_connection_fields")
      toggleShinyDivs("redcap_configure_fields", "redcap_configure_status")
      values$redcap_instrument <- reviewr_config$redcap_instrument
      values$redcap_text_fields <- reviewr_config$redcap_text_fields
      values$redcap_text_fields_selection_list <- reviewr_config$redcap_text_fields %>% deframe() # Specifically set up for use in select control
    }
  }
  
  output$connected_text <- renderText(paste("You have connected to a", database_display_name[input$db_type],
                                            "database stored in the", data_model_display_name[input$data_model], "data model"))
  
  # If the user clicks the button to connect to the database, we will perform the initialization and
  # setup here.  This mimics what's done in the previous block if a config file is present.
  observeEvent(input$connect, {
    toggleShinyDivs("db_connection_fields", "db_connection_status")
    
    reviewr_config = isolate({ list(
      data_model=input$data_model,
      db_type=input$db_type,
      database=input$dbname,
      host=input$host,
      port=input$port,
      user=input$user,
      password=input$password,
      project = input$project_id,
      dataset = input$dataset)
    })
    
    tryCatch({
      # Initialize the ReviewR application
      reviewr_config <- initialize(reviewr_config)
      render_data_tables = get_render_data_tables(reviewr_config$data_model)
      values$all_people_data = get_all_people_for_list(reviewr_config$data_model)(reviewr_config)
      output = render_data_tables(input, output, reviewr_config)
      
      # Set our reactive values based on the input
      values$data_model <- reviewr_config$data_model
      toggleShinyDivs("db_connection_status", "db_connection_fields")
      
      output$menu <- renderMenu({ full_menu() })
      isolate({updateTabItems(session, "tabs", "setup")})
    },
    error=function(e) {
      reviewr_config <- NULL  # Reset the configuration information
      showNotification(
        paste("There was an error when trying to connect to the database.  Please make sure that you have configured the application correctly, and that the database is running and accessible from your machine.\r\n\r\n",
              "You will need to resolve the connection issue before ReviewR will work properly.  If you need help configuring ReviewR, please see the README.md file that is packaged with the repository.\r\n\r\nError:\r\n", e),
        duration = NULL, type = "error", closeButton = TRUE)
    })
  })
  
  observeEvent(input$database_disconnect, {
    toggleShinyDivs("db_connection_fields", "db_connection_status")
    if (!is.null(reviewr_config) & !is.null(reviewr_config$connection)) {
      dbDisconnect(reviewr_config$connection)
    }
    reviewr_config <- NULL
    output$menu <- renderMenu({ initial_menu() })
    isolate({updateTabItems(session, "tabs", "setup")})
  })
  
  observeEvent(input$redcap_connect, {
    toggleShinyDivs("redcap_connection_fields", "redcap_connection_status")
    toggleShinyDivs("redcap_configure_status", "redcap_configure_fields")
    
    redcap_config = isolate({ list(
      redcap_api_url=input$redcap_api_url,
      redcap_api_token=input$redcap_api_token)
    })
    reviewr_config$redcap_api_url <- redcap_config$redcap_api_url
    reviewr_config$redcap_api_token <- redcap_config$redcap_api_token
    values$redcap_text_fields <- NULL
    
    tryCatch({
      # Initialize the ReviewR application for our REDCap connection
      reviewr_config = initialize_redcap(reviewr_config)
      values$redcap_text_fields <- reviewr_config$redcap_text_fields
      values$redcap_instrument <- reviewr_config$redcap_instrument
      values$redcap_text_fields_selection_list <- reviewr_config$redcap_text_fields %>% deframe() # Specifically set up for use in select control
      toggleShinyDivs("redcap_configure_fields", "redcap_configure_status")
      toggleShinyDivs("redcap_connection_status", "redcap_connection_fields")
    },
    error=function(e) {
      reviewr_config <- NULL  # Reset the configuration information
      showNotification(
        paste("There was an error when trying to connect to REDCap.  Please make sure that you have configured the application correctly, and that you have access to the REDCap API.\r\n\r\n",
              "If you need help configuring ReviewR, please see the README.md file that is packaged with the repository.\r\n\r\nError:\r\n", e),
        duration = NULL, type = "error", closeButton = TRUE)
    })
  })
  
  observeEvent(input$redcap_disconnect, {
    toggleShinyDivs("redcap_configure_status", "redcap_configure_fields")
    toggleShinyDivs("redcap_connection_fields", "redcap_connection_status")
    reviewr_config$redcap_api_url <- NULL
    reviewr_config$redcap_api_token <- NULL
    reviewr_config$redcap_patient_id_field <- NULL
    reviewr_config$redcap_reviewer_id_field <- NULL
  })
  
  observeEvent(input$redcap_configure, {
    reviewr_config$redcap_patient_id_field <- input$redcap_patient_id_field
    reviewr_config$redcap_reviewer_id_field <- input$redcap_reviewr_id_field
  })
  
  # When the Shiny session ends, perform cleanup (closing connections, removing objects from environment).
  # Note that there is no cleanup function for the REDCap API, so the fact we're not cleaning it up is by design.
  session$onSessionEnded(function() {
    if (!is.null(reviewr_config) & !is.null(reviewr_config$connection)) {
      dbDisconnect(reviewr_config$connection)
    }
    rm(list = ls())
  })
}


# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(title = "ReviewR"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      sidebarMenuOutput("menu")
    ) #sidebarMenu
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
    tags$script(HTML("$(document).on('click', '#prev_patient', function () {
                    var myOpts = document.getElementById('subject_id').options;
                    var index = $('#subject_id').prop('selectedIndex');
                    if (index == 0) {
                      return;
                    }

                    Shiny.onInputChange('subject_id', myOpts[index - 1].value);
                  });
                
                $(document).on('click', '#next_patient', function () {
                    var myOpts = document.getElementById('subject_id').options;
                    var index = $('#subject_id').prop('selectedIndex');
                    if (index == (myOpts.length - 1)) {
                      return;
                     }
                    
                    Shiny.onInputChange('subject_id', myOpts[index + 1].value);
                });"
    )),  #script
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(status = "primary", solidHeader = FALSE, width=12, style='padding:0px;',
                    h2("Welcome to ReviewR", style='text-align: center;')
                ), #box
                box(width=12, style="font-size: 12pt;",
                    div("ReviewR is a portable tool to help you explore data across different data models.  Within ReviewR, you can browse patient data stored in either the OMOP or MIMIC-III data model."),
                    br(),
                    div("In addition to viewing patient data, you may also connect to a REDCap project to perform a chart review"),
                    br(),
                    div("To get started, please complete the 'Setup' step (found in the left navigation menu)")
                ) #box
              ) #fluidRow
      ), #tabItem
      tabItem(tabName = "setup",
              fluidRow(
                box(status = "primary", solidHeader = FALSE, width=12, style='padding:0px;',
                    h2("ReviewR Setup", style='text-align: center;')
                ) #box
              ), #fluidRow
              fluidRow(
                column(width=6, style='padding:0px;',
                       box(title="Connect to Database", width=12, status='warning',
                           div(id = "db_connection_status",
                               h3("Success!"),
                               uiOutput("connected_text"),
                               br(),
                               div("To conduct a chart review, connect and configure REDCap."),
                               br(),
                               div("If you would like to just view patient records, please select 'Patient Search' from the main menu."),
                               br(),
                               br(),
                               actionButton("database_disconnect", "Disconnect Database")
                           ), #div
                           div(id = "db_connection_fields",
                             selectInput("data_model", "Select your data model:",
                                         c("OMOP" = "omop",
                                           "MIMIC" = "mimic")),
                             selectInput("db_type", "Select your database:",
                                         c("PostgreSQL" = "postgres",
                                           "BigQuery" = "bigquery")),
                             conditionalPanel(
                               condition = "(input.db_type == 'postgres')",
                               textInput("user", "User:"),
                               passwordInput("password", "Password:"),
                               textInput("host", "Database Host/Server:", "localhost"),
                               textInput("port", "Port:", "5432"),
                               textInput("dbname", "Database Name:")
                             ), #conditionalPanel
                             conditionalPanel(
                               condition = "(input.db_type == 'bigquery')",
                               textInput("project_id", "Project ID:"),
                               textInput("dataset", "Dataset:")
                             ), #conditionalPanel
                             actionButton("connect", "Connect")
                           ) #div
                        ) #box
                ), #column
                column(width=6, style='padding:0px;',
                       box(title="Connect to REDCap", width=12, status='danger',
                           div(id="redcap_connection_status",
                               h3("Success!"),
                               div("Once connected to a database, you can enter your chart abstractions result in your REDCap form."),
                               br(),
                               br(),
                               actionButton("redcap_disconnect", "Disconnect REDCap")
                           ), #div
                           div(id="redcap_connection_fields",
                             textInput("redcap_api_url", "REDCap URL:"),
                             passwordInput("redcap_api_token", "REDCap API Token:"),
                             actionButton("redcap_connect", "Connect to REDCap")
                           ) #div
                       ), #box
                       box(title="Configure REDCap", width=12, status='danger',
                           div(id="redcap_configure_status",
                               div("Please connect to a REDCap instance to enable configuration.")
                           ), #div
                           div(id="redcap_configure_fields",
                             uiOutput("redcap_patient_id_field"),
                             uiOutput("redcap_reviewer_id_field"),
                             conditionalPanel(
                               condition = "input.redcap_reviewer_id != '(Not applicable)'",
                               textInput("redcap_reviewer_name", "Reviewer name:", placeholder = "Your name")
                             ), #conditionalPanel
                             
                             # We have two conditionalPanels that will display the current configuration state
                             conditionalPanel(
                               condition = "input.redcap_patient_id == null || input.redcap_patient_id == undefined || input.redcap_patient_id == ''",
                               div(class="redcap_configure redcap_configure_needed", "Please select the record identifier field")
                             ), #conditionalPanel
                             conditionalPanel(
                               condition = "input.redcap_patient_id != null && input.redcap_patient_id != undefined && input.redcap_patient_id != ''",
                               div(class="redcap_configure redcap_configure_complete", "REDCap is configured!")
                             ) #conditionalPanel
                           ) #div
                       ) #box
                ) #column
              ) #fluidRow
      ), #tabItem
      tabItem(tabName = "patient_search",
              h2("Select a patient to view"),
              withSpinner(DT::dataTableOutput('all_patients_tbl'))
      ), #tabItem
      tabItem(tabName = "chart_review",
              conditionalPanel(
                condition = "input.subject_id == null || input.subject_id == undefined || input.subject_id == ''",
                h4("Please select a patient from the 'Patient Search' tab")
              ),
              conditionalPanel(
                condition = "(input.subject_id != null && input.subject_id != undefined && input.subject_id != '') && output.redcap_patient_id_field != null",
                fluidRow(
                  column(width=9, uiOutput("subject_id_status_redcap")),
                  column(width=3, uiOutput("patient_navigation_list_redcap"))
                ), #fluidRow
                uiOutput("patient_chart_panel_abstraction")
              ), #conditionalPanel - with abstraction
              conditionalPanel(
                condition = "input.subject_id != null && output.redcap_patient_id_field == null",
                fluidRow(
                  column(width=9, uiOutput("subject_id_status")),
                  column(width=3, uiOutput("patient_navigation_list"))
                ), #fluidRow
                uiOutput("patient_chart_panel_no_abstraction")
              ) #conditionalPanel - no abstraction
      ) #tabItem
    )
  )
)


# Run the application 
shinyApp(ui = ui, server = server)