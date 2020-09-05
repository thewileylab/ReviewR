# UI ----
#' Module Selector
#'
#' A Shiny module to select from available ReviewR abstraction modules
#'
#' @param id 
#' 
#' @rdname mod_abstraction_setup
#' 
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList 
abstraction_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # actionButton(inputId = ns('debug'), label = 'Debug'),
    h4('Configure Patient Chart Abstraction'),
    HTML(glue::glue('To begin, please select a ReviewR abstraction module:')),
    br(),
    br(),
    selectInput(inputId = ns('abstraction_modules'), label = 'Abstraction Module:', choices = NULL),
    uiOutput(ns('abstraction_module_setup_ui'))
    )
}

abstraction_instrument_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('abstraction_module_instrument_ui'))
  )
}

# Server ----    
#' Module Selector
#'
#' @param id 
#' 
#' @rdname mod_abstraction_setup
#' 
#' @keywords internal
#' @export
#' 
#' @importFrom magrittr %>% extract2
#' @importFrom purrr map
#' @importFrom rlang exec
mod_abstraction_setup_server <- function(id, subject_id){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Abstraction Module Setup ----
      namespace <- 'abs-selector-ns'
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Add Abstraction Setup Modules Here!!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
      ## Add Abstraction Setup Modules Here
      abstraction_setup_vars <- reactiveValues(redcap = shinyREDCap::redcap_server(id = namespace, subject_id = subject_id) )
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
      
      # observeEvent(input$debug, {
      #   browser()
      # })
      
      selector_vals <- reactiveValues(
        module_names = '<empty>'
        )
      # Identify Abstraction Modules ----
      ## Parse module names from reactive values object
      observe({
        req(abstraction_setup_vars)
        # browser()
        values <- abstraction_setup_vars %>% names()
        names <- map(values, ~extract2(abstraction_setup_vars[[.x]], 'moduleName'))
        names(values) <- names
        selector_vals$module_names <- values
        })
      
      # Update selectInput ----
      ## Add Module names as choices for selectInput
      observeEvent(selector_vals$module_names, {
        req(selector_vals$module_names != '<empty>')
        updateSelectInput(session = session,
                          inputId = 'abstraction_modules', 
                          choices = selector_vals$module_names %>% sort()
                          )
        })
      
      # Extract Setup Module UI Function ----
      ## Render the Setup UI function from the selected module
      selected_module_setup_ui <- reactive({
        req(input$abstraction_modules)
        ### Nested modules need ns() wrapper around UI so
        ### they inherit the outer module namespace
        module_ui_args <- list(id = ns(namespace))
        ### Execute selected DB Module UI function 
        ### with ns() wrapped namespace
        rlang::exec(abstraction_setup_vars[[input$abstraction_modules]]$setup_ui,
                    !!!module_ui_args)
        })
      
      # Extract Instrument Module UI Function ----
      ## Render the Instrument UI function from the selected module
      selected_module_instrument_ui <- reactive({
        req(input$abstraction_modules)
        ### Nested modules need ns() wrapper around UI so
        ### they inherit the outer module namespace
        module_ui_args <- list(id = ns(namespace))
        ### Execute selected DB Module UI function 
        ### with ns() wrapped namespace
        rlang::exec(abstraction_setup_vars[[input$abstraction_modules]]$instrument_ui,
                    !!!module_ui_args)
        })
      
      
      # UI Outputs ----
      output$abstraction_module_setup_ui <- renderUI({ selected_module_setup_ui() })
      output$abstraction_module_instrument_ui <- renderUI({ selected_module_instrument_ui() })
      
      # Return ----
      ## Return setup variables from the selected module
      abstraction_module_vars <- reactive({
        req(input$abstraction_modules)
        abstraction_setup_vars %>% extract2(input$abstraction_modules)
        })
      return(abstraction_module_vars)
    }
  )
}
