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

# Save some lookups for mapping selection values to a display value in the UI
data_model_display_name = list("omop" = "OMOP", "mimic" = "MIMIC-III")
database_display_name = list("postgres" = "PostgreSQL", "bigquery" = "BigQuery")

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
  has_projects = FALSE
  output$has_projects <- reactive({ has_projects })

  output$subject_id_status <- renderUI({
    div(class="subject_id_header",
      span(paste("Subject - ",input$subject_id), class="subject_id_label")
      #span("[status]")  #TODO - display status image
    )#div
  }) #renderUI

  # Create the panel that hosts navigating between patient records
  output$patient_navigation_list <- renderUI({
    div(actionButton(inputId = 'prev_patient',label = '<< Previous'),
        actionButton(inputId = 'next_patient',label = 'Next >>'),
        br(),
        selectInput("subject_id", label = NULL, selectize = FALSE, selected = input$subject_id, choices = values$all_people_data$ID)
    ) #div
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
  output$patient_chart_panel_abstraction <- renderUI({ patient_chart_panel() })
  output$patient_chart_panel_no_abstraction <- renderUI({ patient_chart_panel() })
  
  output$selected_project_title <- renderText({
    ifelse(is.null(input$project_id),
           "(No project selected)",
           paste("Project: ", project_list[project_list$id == input$project_id, "name"]))
  })
  output$selected_project_id <- renderText({input$project_id})
  
  observeEvent(input$viewProjects, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "projects")
  })
  observeEvent(input$viewPatients, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "patient_search")
  })
  
  toggleShinyDivs("redcap_connection_fields", "redcap_connection_status")
  toggleShinyDivs("redcap_configure_status", "redcap_configure_fields")
  toggleShinyDivs("db_connection_fields", "db_connection_status")
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Setup", tabName = "setup", icon = icon("cog"))
    )
  })
  
  # If during initial setup of the server we have configuration data, attempt to use it to initialize
  # the application, including establishing a connection to the underlying database.
  if (!is.null(reviewr_config)) {
    reviewr_config <- initialize(reviewr_config)
    values$data_model <- reviewr_config$data_model
    render_data_tables = get_render_data_tables(reviewr_config$data_model)
    values$all_people_data = get_all_people_for_list(reviewr_config$data_model)(reviewr_config)
    output = render_data_tables(input, output, reviewr_config)
    
    toggleShinyDivs("db_connection_status", "db_connection_fields")
    
    output$menu <- renderMenu({
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Setup", tabName = "setup", icon = icon("cog")),
        menuItem("Patient Search", tabName = "patient_search", icon = icon("users")),
        menuItem("Chart Review", icon = icon("table"), tabName = "chart_review")
      )
    })
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
      
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Home", tabName = "home", icon = icon("home")),
          menuItem("Setup", tabName = "setup", icon = icon("cog")),
          menuItem("Patient Search", tabName = "patient_search", icon = icon("users")),
          menuItem("Chart Review", icon = icon("table"), tabName = "chart_review")
        )
      })
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
  
  observeEvent(input$redcap_connect, {
    toggleShinyDivs("redcap_configure_fields", "redcap_configure_status")
    toggleShinyDivs("redcap_connection_status", "redcap_connection_fields")
  })
  
  outputOptions(output, "has_projects", suspendWhenHidden = FALSE)
  
  # When the Shiny session ends, perform cleanup (closing connections, removing objects from environment)
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
                               div("If you would like to just view patient records, please select 'Patient Search' from the main menu.")
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
                               div("Once connected to a database, you can enter your chart abstractions result in your REDCap form.")
                           ), #div
                           div(id="redcap_connection_fields",
                             textInput("redcap_url", "REDCap URL:"),
                             textInput("redcap_api_key", "REDCap API Key:"),
                             actionButton("redcap_connect", "Connect to REDCap")
                           ) #div
                       ), #box
                       box(id = "redcap_configure", title="Configure REDCap", width=12, status='danger',
                           div(id="redcap_configure_status",
                               div("Please connect to a REDCap instance to enable configuration.")
                           ), #div
                           div(id="redcap_configure_fields",
                             selectInput("redcap_patient_id", "Which variable contains your patient identifier?", c()),
                             checkboxInput("redcap_multiple_reviewers", "Multiple reviewers?"),
                             actionButton("redcap_configure", "Configure REDCap")
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
                condition = "input.subject_id != ''",
                fluidRow(
                  column(width=8, uiOutput("subject_id_status")),
                  column(width=4, uiOutput("patient_navigation_list"))
                ), #fluidRow
                uiOutput("patient_chart_panel_no_abstraction")
              ) #conditionalPanel
      ) #tabItem
    )
  )
)


# Run the application 
shinyApp(ui = ui, server = server)