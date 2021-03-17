# Module Documentation ----
#' Database Module Selector
#' 
#' @description 
#' 
#' This module allows the user to select from available ReviewR database modules. It 
#' dynamically returns the database setup UI and database connection information, based 
#' on the user's selection. 
#' 
#' See \code{vignette("customize_support_new_rdbms", package = "ReviewR")} for more 
#' information on database modules and how to add support for additional databases.
#' 
#' This module consists of the following components:
#' 
#' ## Module UI function
#' 
#' \itemize{
#' \item{`database_setup_ui`}: A uiOutput with a dropdown selector containing 
#' available database modules.
#' }
#' ## Module Server function
#' \itemize{
#' \item{`database_setup_server`}: Processes user selection and returns the selected
#' database module's configuration UI and any connection information returns.
#' }
#' 
#' @param id The Module namespace
#' @name mod_database_setup
#' 
#' @return 
#' *database_setup_ui*:
#' \item{tagList}{A tagList containing a selectInput that allows for selection of 
#' available database setup modules.}
#' *database_setup_server*:
#' \item{reactiveValues}{This module has no returns of its own, but will pass on
#' the `reactiveValues` returns from the user selected database module.}
#' 
NULL
#> NULL

# UI ----
#' @rdname  mod_database_setup
#' 
#' @keywords internal
#'
#' @importFrom shiny NS tagList 
database_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4('Connect to Patient Database'),
    HTML(glue::glue('To begin, please select a ReviewR database module:')),
    br(),
    br(),
    selectInput(inputId = ns('database_modules'), label = 'Database Module:', choices = NULL),
    uiOutput(ns('database_module_ui'))
    )
}

# Server ----
#' @rdname  mod_database_setup
#' 
#' @keywords internal
#' 
#' @import dbplyr
#' @importFrom magrittr %>% extract2
#' @importFrom purrr map
#' @importFrom rlang exec
#' @importFrom shinyjs disable enable
#' 

database_setup_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Database Module Setup ----
      namespace <- 'db-selector-ns'
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Add Database Setup Modules Here!!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
      ## Add Database Setup Modules Here
      database_setup_vars <- reactiveValues(bigquery = bigquery_setup_server(id = namespace ),
                                            demo_sqlite = demo_sqlite_setup_server(id = namespace),
                                            postgres = postgresql_setup_server(id = namespace)
                                            )
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
      
      selector_vals <- reactiveValues(
        module_names = '<empty>'
        )
      # Identify Database Modules ----
      ## Parse module names from reactive values object
      observe({
        req(database_setup_vars)
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
                          choices = selector_vals$module_names %>% sort()
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
        rlang::exec(database_setup_vars[[input$database_modules]]$setup_ui,
                    !!!module_ui_args)
        })
      
      # Disable Selector on Successful Database Connection ----
      ## Prevent multiple modules from being used simultaneously
      observeEvent(database_module_vars()$is_connected, {
        if(database_module_vars()$is_connected == 'yes') {
          shinyjs::disable('database_modules')
          } else {
            shinyjs::enable('database_modules')
            }
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
