# UI ----
#' Module Selector
#'
#' A Shiny module to select from available ReviewR database modules
#'
#' @param id 
#' 
#' @rdname mod_database_setup
#' 
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList 
database_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # actionButton(inputId = ns('debug'), label = 'Debug'),
    h4('Connect to Patient Database'),
    HTML(glue::glue('To begin, please select a ReviewR database module:')),
    br(),
    br(),
    selectInput(inputId = ns('database_modules'), label = 'Database Module:', choices = NULL),
    uiOutput(ns('database_module_ui'))
    )
}

# Server ----    
#' Module Selector
#'
#' @param id 
#' 
#' @rdname mod_database_setup
#' 
#' @keywords internal
#' @export
#' 
#' @importFrom magrittr %>% extract2
#' @importFrom purrr map
#' @importFrom rlang exec
mod_database_setup_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      namespace <- 'db-selector-ns'
      # Database Module Setup ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Add Database Setup Modules Here!!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
      ## Add Database Setup Modules Here
      database_setup_vars <- reactiveValues(bigquery = shinyBigQuery::bigquery_setup_server(id = namespace ),
                                            postgresql = shinyPostgreSQL::postgresql_setup_server(id = namespace )
                                            )
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
      
      # observeEvent(input$debug, {
      #   browser()
      # })
      
      selector_vals <- reactiveValues(
        module_names = '<empty>'
        )
      # Identify Database Modules ----
      ## Parse module names from reactive values object
      observe({
        req(database_setup_vars)
        # browser()
        values <- database_setup_vars %>% names()
        names <- map(values, ~extract2(database_setup_vars[[.x]], 'moduleName'))
        names(values) <- names
        selector_vals$module_names <- values
        })
      
      # Update selectInput ----
      ## Add Module names as choices for selectInput
      observeEvent(selector_vals$module_names, {
        req(selector_vals$module_names != '<empty>')
        updateSelectInput(session = session,
                          inputId = 'database_modules', 
                          choices = selector_vals$module_names
                          )
        })
      
      # Extract Module UI Function ----
      ## Render the UI function from the selected module
      selected_module_ui <- reactive({
        req(input$database_modules)
        ### Nested modules need ns() wrapper around UI so
        ### they inherit the outer module namespace
        module_ui_args <- list(id = ns(namespace))
        ### Execute selected DB Module UI function 
        ### with ns() wrapped namespace
        rlang::exec(database_setup_vars[[input$database_modules]]$ui,
                    !!!module_ui_args)
        })
      
      # UI Outputs ----
      output$database_module_ui <- renderUI({ selected_module_ui() })
      
      # Return ----
      ## Return setup variables from the selected module
      database_module_vars <- reactive({
        req(input$database_modules)
        database_setup_vars %>% extract2(input$database_modules)
        })
      return(database_module_vars)
    }
  )
}