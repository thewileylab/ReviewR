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
#' @importFrom dplyr if_else
#' @importFrom snakecase to_title_case
mod_selector_ui <- function(id, type = c('database', 'abstraction'), color = '#ebf0f6') {
  ns <- NS(id)
  # Evaluate Type Selection ----
  type <- match.arg(type)
  # message(type)
  tagList(
    wellPanel(
      # actionButton(inputId = ns('debug'), label = 'Debug'),
      h4(dplyr::if_else(type == 'database',
                        'Connect to Patient Database',
                        'Configure Patient Chart Abstraction'
                        )
         ),
      style = glue::glue('background: {color}'),
      HTML(glue::glue('To begin, please select a ReviewR {type} module:')),
      br(),
      br(),
      selectInput(inputId = ns('modules'), label = glue::glue('{snakecase::to_title_case(type)} Module:'), choices = NULL),
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
      # observeEvent(input$debug, {
      #   browser()
      # })
      selector_vals <- reactiveValues(
        module_names = '<empty>'
        )
      # Identify Modules ----
      ## Parse module names from reactive values object
      observe({
        values <- database_vars %>% names()
        names <- map(database_vars %>% names(), ~extract2(database_vars[[.x]], 'moduleName'))
        names(values) <- names
        selector_vals$module_names <- values
        })
      
      # Update selectInput ----
      ## Add Module names as choices for selectInput
      observeEvent(selector_vals$module_names,{
        req(selector_vals$module_names != '<empty>')
        updateSelectInput(session = session,
                          inputId = 'modules', 
                          choices = selector_vals$module_names
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
