# UI ----
#' Module Selector
#'
#' A Shiny module to select from available ReviewR modules
#'
#' @param id 
#' 
#' @rdname mod_selector
#' 
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList 
mod_selector_ui <- function(id, type = c('Database', 'Abstraction'), color = '#ebf0f6') {
  ns <- NS(id)
  tagList(
    wellPanel(
      style = glue::glue('background: {color}'),
      selectInput(inputId = ns('modules'),label = glue::glue('{type} Module Selector'), choices = NULL),
      shiny::helpText(glue::glue('To begin, please select a ReviewR {type} Module.')),
      uiOutput(ns('db_module'))
    )
 
  )
}

# Server ----    
#' Module Selector
#'
#' @param id 
#' @param database_vars A reactiveValues() object containing the return values from modules in your Shiny application.
#' 
#' @rdname mod_selector
#' 
#' @keywords internal
#' @export
#' 
#' 
mod_selector_server <- function(id, database_vars){
  moduleServer(
    id,
    function(input, output, session) {
      # Identify Modules ----
      ## Parse module names from reactive values object
      module_names <- reactive({
        values <- database_vars %>% names()
        names <- map(database_vars %>% names(), ~extract2(database_vars[[.x]], 'moduleName'))
        names(values) <- names
        return(values)
        })
      
      # Update selectInput ----
      ## Add Module names as choices for selectInput
      observeEvent(module_names(), {
        updateSelectInput(session = session,
                          inputId = 'modules', 
                          choices = module_names()
        )
      })
      
      # Extract Module UI Function ----
      ## Render the UI function from the selected module
      selected_module <- reactive({
        req(input$modules)
        database_vars[[input$modules]]$ui
      })
      
      # UI Outputs ----
      output$db_module <- renderUI({ selected_module() })
      
      # Return ----
      ## Return setup variables from the selected module
      module_vars <- reactive({
        req(input$modules)
        database_vars %>% extract2(input$modules)
        })
      return(module_vars)
    }
  )
}
