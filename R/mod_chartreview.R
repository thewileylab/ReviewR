# UI ----
#' Chart Review
#'
#' @param id The namespace id for the UI output
#'
#' @rdname mod_chartreview
#' 
#' @keywords internal
#' @export
#'
chartreview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns('debug'), label = "I'm alive!"),
    uiOutput(ns('view_review'))
  )
}

# Server ----
#' Chart Review
#'
#' @param id The namespace id for the UI output
#' @param database_vars Database variables returned from user selected database setup module
#' @param datamodel_vars Datamodel variables returned from mod_datamodel_setup
#' @param abstract_vars Abstraction variables returned from user selected abstraction module
#' @param navigation_vars Navigation variables returned from mod_navigation
#'
#' @rdname mod_chartreview
#' 
#' @keywords internal
#' @export
mod_chartreview_server <- function(id, database_vars, datamodel_vars, abstract_vars, navigation_vars) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$debug, {
        browser()
        })
      
      view_review <- reactive({
        if(database_vars()$is_connected == 'yes' & abstract_vars()$is_configured == 'yes') {
          tagList(
            column(width = 9,
                   box(width = '100%',
                       status = 'primary'
                       ## Patient Chart
                       )
                   ),
            column(width = 3,
                   ## Abstraction Instrument
                   abstraction_instrument_ui('abs-selector')
                   )
            )
        } else {
          tagList(
            box(width = '100%',
                status = 'primary')
            )
          }
      })
      
      output$view_review <- renderUI({ view_review() })
    }
  )
}