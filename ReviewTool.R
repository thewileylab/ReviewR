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
library(bigrquery)
source('lib/helpers.R')

# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(title = textOutput('title')),
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
              h2("Welcome to ReviewR"),
              fluidRow(class="home_container",
                column(6,
                       div(class="jumbotron home_panel",
                         h3("Project List"),
                         div("Conduct chart reviews for your configured projects", class="lead"),
                         actionLink(inputId = "viewProjects", label = "View Projects", class="btn btn-primary btn-lg"))),
                column(6,
                       div(class="jumbotron home_panel",
                         h3("Browse Patients"),
                         div("Navigate through the full list of OMOP patients", class="lead"),
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
                condition = "input.subject_id & !output.has_projects",
                uiOutput("patient_chart_panel_no_abstraction")
              ), #conditionalPanel
              conditionalPanel(
                condition = "input.subject_id & output.has_projects",
                splitLayout(
                  cellWidths = c("70%", "30%"),
                  uiOutput("patient_chart_panel_abstraction"),
                  # Chart review row
                  div(
                     fluidRow(
                       column(12, 
                              div(
                                id = "abstraction_form",
                                h4("Abstraction Results"),
                                withSpinner(DT::dataTableOutput('abstraction_tbl'))
                              ) #div
                       ) #column
                     ) #fluidRow
                   ) #div
                ) #splitLayout
              ) #conditionalPanel
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
  
  output$title = renderText({ paste0("ReviewR (", app_config["data_model"], ")") })
  
  if (app_config["database"] == "bigquery") {
    bq_connection_details_df <- read.csv(file="./conf/bigquery_secrets.csv", stringsAsFactors = FALSE)
    db_config <- as.list(bq_connection_details_df[,"value"])
    names(db_config) <- bq_connection_details_df[,"key"]
    rm(bq_connection_details_df)

    connection <- as.character(db_config["project_id"])
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
  
  if (tolower(app_config["data_model"]) == "mimic") {
    render_data_tables = mimic_render_data_tables
  }
  else if (tolower(app_config["data_model"]) == "omop") {
    render_data_tables = omop_render_data_tables
  }
  else {
    stop("Currently this application only supports MIMIC III and OMOP data models")
  }
  
  has_projects = FALSE
  if ("project_list" %in% names(app_config)) {
    tmp_project_list <- as.list(unlist(strsplit(as.character(app_config["project_list"]), split="|", fixed = TRUE)))
    projects <- do.call(rbind, lapply(tmp_project_list, loadProject))
    has_projects <- ifelse(length(tmp_project_list) > 0, TRUE, FALSE)
    rm(tmp_project_list)
  }
  output$has_projects <- reactive({ has_projects })
  
  # Load the table mapping configuration.  For a given database system, this will tell us
  # the correct table names to use in our SQL statements
  table_config_df <- read.csv(file=paste0("./conf/",app_config["table_map_file"]), stringsAsFactors = FALSE)
  table_config <- as.list(table_config_df[,"database_table"])
  names(table_config) <- table_config_df[,"table_key"]
  rm(table_config_df)

  output = render_data_tables(table_config, db_config, input, output, app_config["database"], connection)
  cohort_data <- reactive({ active_cohort_data(input$project_id, projects) })
  abstraction_data <- reactive({ active_abstraction_data(input$project_id, projects) %>% filter(subject_id == input$subject_id)})
  
  if (has_projects) {
    project_list = projects %>%
      mutate(project_id = paste0("<a class='row_project_id' href='#'>", id, "</a>")) %>%
      select(id, project_id, name)
    
    output$projects_tbl <- DT::renderDataTable(project_list[,c("project_id","name")], options = list(paging=FALSE, searchHighlight = TRUE),
                                               escape = FALSE, rownames=F,
                                               callback = JS(
                                                 'table.on("click", "tr td a.row_project_id", function() {
                                                 Shiny.onInputChange("project_id", $(this).text());});'))
    output$cohort_tbl <- DT::renderDataTable(cohort_data(), options = list(paging = FALSE, searchHighlight = TRUE),
                                             escape = FALSE, rownames=F, selection='none',
                                             callback = JS(
                                               'table.on("click", "tr td a.row_subject_id", function() {
                                               Shiny.onInputChange("subject_id", $(this).text());
                                               $(".main-sidebar li a").click();});'))
    output$abstraction_tbl <- DT::renderDataTable(abstraction_data(),
                                                   options = list(paging = FALSE, searchHighlight = FALSE, dom='t', ordering=FALSE),
                                                   rownames=F, edit=TRUE, selection='none')
  }
  
  output$subject_id_output <- renderText({paste("Subject ID - ",input$subject_id)})
  output$subject_id <- renderText({input$subject_id})
  
  # Because we want to use uiOutput for the patient chart panel in multiple locations in the UI,
  # we need to implement the workaround described here (https://github.com/rstudio/shiny/issues/743)
  # where we actually render multiple outputs for each use.
  patient_chart_panel = reactive({
    table_names <- get_review_table_names(app_config["data_model"])
    tabs <- lapply(table_names, function(id) { create_data_panel(id, paste0(id, "_tbl"))})
    panel <- div(
      fluidRow(column(12, h2("You are seeing the record of:"))),
      fluidRow(column(12, div(class='parameters', textOutput("subject_id_output")))),
      br(),
      fluidRow(column(12, do.call(tabsetPanel, tabs)))
    ) #div
    panel
  })
  output$patient_chart_panel_abstraction <- renderUI({ patient_chart_panel() })
  output$patient_chart_panel_no_abstraction <- renderUI( {patient_chart_panel() })
  
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
  
  outputOptions(output, "has_projects", suspendWhenHidden = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

