#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
## Requires the following packages:  install.packages(c("shiny", "tidyverse", "shinythemes", "DT", "bigrquery"))


# First time setup for all dependencies:
# install.packages(c("shiny", "shinyjs", "DT", "bigrquery", "tidyverse"))

# install.packages("RPostgreSQL")

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
source('data_helpers.R')
source('project_helpers.R')
#library(shinythemes)

# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(title = "MIMICIII Chart Review Tool"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Projects", icon = icon("tasks"), tabName = "projects"),
      menuItem("Patient Search", tabName = "patient_search", icon = icon("users")),
      menuItem("Chart Review", icon = icon("table"), tabName = "chart_review")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
    
    tabItems(
      tabItem(tabName = "home",
              h2("Welcome to the MIMIC Chart Reviewer"),
              fluidRow(class="home_container",
                column(6,
                       div(class="jumbotron home_panel",
                         h3("Project List"),
                         div("Conduct chart reviews for your configured projects", class="lead"),
                         actionLink(inputId = "viewProjects", label = "View Projects", class="btn btn-primary btn-lg"))),
                column(6,
                       div(class="jumbotron home_panel",
                         h3("Browse Patients"),
                         div("Navigate through the full list of MIMIC III patients", class="lead"),
                         actionLink(inputId = "viewPatients", label = "View Patients", class="btn btn-primary btn-lg")))
              )
      ),
      tabItem(tabName = "projects",
              conditionalPanel(
                condition = "!(input.project_id)",
                h2("Select a project to work on"),
                withSpinner(DT::dataTableOutput('projects_tbl'))
              ),
              conditionalPanel(
                condition = "(input.project_id)",
                h2(textOutput("selected_project_title")),
                HTML("<a id='reset_project_id' href='#' onclick='Shiny.onInputChange(\"project_id\", null);'>View All Projects</a>"),
                div(id="active_cohort",
                    withSpinner(DT::dataTableOutput('cohort_tbl')))
              )
      ),
      tabItem(tabName = "patient_search",
              h2("Select a patient to view"),
              withSpinner(DT::dataTableOutput('all_patients_tbl'))
      ),
      tabItem(tabName = "chart_review",
              conditionalPanel(
                condition = "!(input.subject_id)",
                h4("Please select a patient from the 'Patient Search' tab")
              ),
              conditionalPanel(
                condition = "input.subject_id",
                splitLayout(
                  cellWidths = c("70%", "30%"),
                  div(
                    fluidRow(
                      column(12, h2("You are seeing the record of:"))),
                    fluidRow(
                      column(12, div(class='parameters', textOutput("subject_id_output")))),
                    fluidRow(
                      column(12, div(id='hadm_id_display', class='parameters', textOutput("hadm_id_output")),
                             HTML("<a id='reset_hadm_id' href='#' onclick='Shiny.onInputChange(\"hadm_id\", null);'>Clear HADM ID</a>"))),
                    br(),
                    fluidRow(
                      column(12,
                             tabsetPanel(
                               tabPanel("ADMISSIONS", withSpinner(DT::dataTableOutput('admissions_tbl'))),
                               tabPanel("CALLOUT", withSpinner(DT::dataTableOutput('callout_tbl'))),
                               tabPanel("CHARTEVENTS", withSpinner(DT::dataTableOutput('chartevents_tbl'))), ## MAKE SURE TO MAP TO D_ITEMTS
                               tabPanel("CPTEVENTS", br(), em("Please note that due to licensing restrictions, neither MIMIC-III nor course instructors can provide labels for these CPT codes."), br(), withSpinner(DT::dataTableOutput('cptevents_tbl'))),
                               #tabPanel("DATETIMEEVENTS", DT:dataTableOutput('datetimeevents_tbl')), ## MAKE SURE TO MAP TO D_ITEMS
                               tabPanel("DIAGNOSES_ICD", br(), em("Please note that this table has been joined with 'D_ICD_DIAGNOSES' to provide the long title value for your reference."), br(), withSpinner(DT::dataTableOutput('diagnoses_icd_tbl'))),
                               tabPanel("DRGCODES", withSpinner(DT::dataTableOutput('drgcodes_tbl'))),
                               tabPanel("ICUSTAYS", withSpinner(DT::dataTableOutput('icustays_tbl'))),
                               # tabPanel("INPUTEVENTS_CV", DT::dataTableOutput('inputevents_cv_tbl')),
                               # tabPanel("INPUTEVENTS_MV", DT::dataTableOutput('inputevents_mv_tbl')),
                               tabPanel("LABEVENTS", br(), em("Please note that this table has been joined with 'D_LABITEMS' to provide the lab label, fluid, and category for your reference."), br(), withSpinner(DT::dataTableOutput('labevents_tbl'))),
                               tabPanel("MICROBIOLOGYEVENTS", withSpinner(DT::dataTableOutput('microbiologyevents_tbl'))),
                               tabPanel("NOTEEVENTS", br(), em("Please note that all 'ISERROR' notes have been removed from this table."),br(), withSpinner(DT::dataTableOutput('noteevent_tbl'))),
                               #tabPanel("OUTPUTEVENTS", DT::dataTableOutput('outputevents_tbl')),
                               tabPanel("PATIENTS", withSpinner(DT::dataTableOutput('patients_tbl'))),
                               tabPanel("PRESCRIPTIONS", withSpinner(DT::dataTableOutput('prescriptions_tbl'))),
                               tabPanel("PROCEDUREEVENTS_MV", br(), em("Please note that this table has been joined with 'D_ITEMS' to provide the Label, and DBSource for your reference."), br(), withSpinner(DT::dataTableOutput('procedureevents_mv_tbl'))),
                               tabPanel("PROCEDURES_ICD", br(), em("Please note that this table has been joined with 'D_ICD_PROCEDURES' to provide the long title value for your reference."), br(), withSpinner(DT::dataTableOutput('procedures_icd_tbl'))),
                               tabPanel("SERVICES", br(), em("Please note that service descriptions have been provided from the data dictionary for your reference."), br(), withSpinner(DT::dataTableOutput('services_tbl'))),
                               tabPanel("TRANSFERS", withSpinner(DT::dataTableOutput('transfers_tbl')))
                             )
                      )
                    )
                  ),

                  # Chart review row
                  div(
                    fluidRow(
                      column(12, 
                             div(
                               id = "abstraction_form",
                               h3("Abstraction Form"),
                               span("Subject ID: ", textOutput("subject_id")),
                               span("HADM ID: ", textOutput("hadm_id")),
                               checkboxInput("has_dx", "Has diagnosis of...", FALSE),
                               actionButton("submit", "Save", class = "btn-primary")
                             )
                      )
                  )
                  )
              )  # div
          ) #splitLayout
      )
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  library(bigrquery)
  library(RPostgreSQL)
  
  library(readr)
  library(DT)
  library(tidyverse)
  library(magrittr)
  
  options("httr_oob_default" = TRUE)
  
  app_config_df <- read.csv(file="./conf/app_config.csv", stringsAsFactors = FALSE)
  app_config <- as.list(app_config_df[,"value"])
  names(app_config) <- app_config_df[,"key"]
  rm(app_config_df)
  
  
  if (app_config["database"] == "bigquery") {
    bq_connection_details_df <- read.csv(file="./conf/bigquery_secrets.csv", stringsAsFactors = FALSE)
    db_config <- as.list(bq_connection_details_df[,"value"])
    names(db_config) <- bq_connection_details_df[,"key"]
    rm(bq_connection_details_df)

    connection <- db_config["project_id"]
  }
  else if (app_config["database"] == "postgres") {
    pg_connection_details_df <- read.csv(file="./conf/postgres_secrets.csv", stringsAsFactors = FALSE)
    db_config <- as.list(pg_connection_details_df[,"value"])
    names(db_config) <- pg_connection_details_df[,"key"]
    rm(pg_connection_details_df)
    
    drv <- dbDriver("PostgreSQL")
    connection <- dbConnect(drv, dbname = db_config["dbname"],
                     host = db_config["host"], port = as.integer(db_config["port"]),
                     user = db_config["user"], password = db_config["password"])
  }
  else {
    stop("Currently this application only supports Postgres and BigQuery databases")
  }
  
  if ("project_list" %in% names(app_config)) {
    tmp_project_list <- as.list(unlist(strsplit(as.character(app_config["project_list"]), split="|", fixed = TRUE)))
    projects <- do.call(rbind, lapply(tmp_project_list, loadProject))
    rm(tmp_project_list)
  }
  
  # Load the table mapping configuration.  For a given database system, this will tell us
  # the correct table names to use in our SQL statements
  table_config_df <- read.csv(file=paste0("./conf/",app_config["table_map_file"]), stringsAsFactors = FALSE)
  table_config <- as.list(table_config_df[,"database_table"])
  names(table_config) <- table_config_df[,"table_key"]
  rm(table_config_df)

  admissions_data <- reactive({query_admissions(table_config, db_config, input, app_config["database"], connection)})
  callout_data <- reactive({query_callout(table_config, db_config, input, app_config["database"], connection)})
  chartevents_data <- reactive({query_chartevents(table_config, db_config, input, app_config["database"], connection)})
  cptevents_data <- reactive({query_cptevents(table_config, db_config, input, app_config["database"], connection)}) 
  diagnoses_icd_data <- reactive({query_diagnoses_icd(table_config, db_config, input, app_config["database"], connection)})
  drgcodes_data <- reactive({query_drgcodes(table_config, db_config, input, app_config["database"], connection)})
  icustays_data <- reactive({query_icustays(table_config, db_config, input, app_config["database"], connection)})
  labevents_data <- reactive({query_labevents(table_config, db_config, input, app_config["database"], connection)})
  microbiologyevents_data <- reactive({query_microbiologyevents(table_config, db_config, input, app_config["database"], connection)})
  noteevent_data <- reactive({query_noteevent(table_config, db_config, input, app_config["database"], connection)})
  patients_data <- reactive({query_patients(table_config, db_config, input, app_config["database"], connection)})
  prescriptions_data <- reactive({query_prescriptions(table_config, db_config, input, app_config["database"], connection)})
  procedureevents_mv_data <- reactive({query_procedureevents_mv(table_config, db_config, input, app_config["database"], connection)})
  procedures_icd_data <- reactive({query_procedures_icd(table_config, db_config, input, app_config["database"], connection)})
  services_data <- reactive({query_services(table_config, db_config, input, app_config["database"], connection)})
  transfers_data <- reactive({query_transfers(table_config, db_config, input, app_config["database"], connection)})
  
  project_list = projects %>%
    mutate(project_id = paste0("<a class='row_project_id' href='#'>", id, "</a>")) %>%
    select(id, project_id, name)
  
  output$subject_id_output <- renderText({paste("Subject ID - ",input$subject_id)})
  output$hadm_id_output <- renderText({ paste("Hospital Admission ID - ", input$hadm_id) })
  output$subject_id <- renderText({input$subject_id})
  output$hadm_id <- renderText({input$hadm_id})
  
  output$admissions_tbl <- DT::renderDataTable(admissions_data(), options = list(paging = FALSE, searchHighlight = TRUE),
                                               escape=FALSE, rownames=F,
                                               callback = JS(
                                                 'table.on("click", "tr td a.row_hadm_id", function() {
                                                 Shiny.onInputChange("hadm_id", $(this).text());
                                                 });'))
  output$callout_tbl <- DT::renderDataTable(callout_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$chartevents_tbl <- DT::renderDataTable(chartevents_data(), filter = 'top', options = list(paging = TRUE, searchHighlight = TRUE), rownames=F)
  output$cptevents_tbl <- DT::renderDataTable(cptevents_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F) 
  output$diagnoses_icd_tbl <- DT::renderDataTable(diagnoses_icd_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$drgcodes_tbl <- DT::renderDataTable(drgcodes_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$icustays_tbl <- DT::renderDataTable(icustays_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F) 
  output$labevents_tbl <- DT::renderDataTable(labevents_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$microbiologyevents_tbl <- DT::renderDataTable(microbiologyevents_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$noteevent_tbl <- DT::renderDataTable(noteevent_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$all_patients_tbl <- DT::renderDataTable(
    query_all_patients(table_config, db_config, app_config["database"], connection),
    options = list(paging = TRUE, pageLength = 20, searchHighlight = TRUE),
    escape=FALSE, rownames=F,
    callback = JS(
      'table.on("click", "tr td a.row_subject_id", function() {
         Shiny.onInputChange("subject_id", $(this).text());
         $(".main-sidebar li a").click();
       });'))
  output$patients_tbl <- DT::renderDataTable(patients_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$prescriptions_tbl <- DT::renderDataTable(prescriptions_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$procedureevents_mv_tbl <- DT::renderDataTable(procedureevents_mv_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$procedures_icd_tbl <- DT::renderDataTable(procedures_icd_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$services_labels_tbl <- renderTable(services_labels)
  output$services_tbl <-  DT::renderDataTable(services_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$transfers_tbl <- DT::renderDataTable(transfers_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)

  output$projects_tbl <- DT::renderDataTable(project_list[,c("project_id","name")], options = list(paging=FALSE, searchHighlight = TRUE),
                                             escape = FALSE, rownames=F,
                                             callback = JS(
                                               'table.on("click", "tr td a.row_project_id", function() {
                                               Shiny.onInputChange("project_id", $(this).text());
                                               });'))
  output$cohort_tbl <- DT::renderDataTable(active_cohort_data(input$project_id, projects), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  
  output$selected_project_title <- renderText({
    ifelse(is.null(input$project_id),
           "(No project selected)",
           paste("Project: ", project_list[project_list$id == input$project_id, "name"]))
    })
  output$selected_project_id <- renderText({input$project_id})
  
  session$onSessionEnded(function() {
    if (app_config["database"] == "postgres") {
      dbDisconnect(connection);
    }
  })
  
  observeEvent(input$viewProjects, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "projects")
  })
  observeEvent(input$viewPatients, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "patient_search")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

