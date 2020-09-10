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
    uiOutput(ns('view_review'))
    )
  }

# Server ----
#' Chart Review
#'
#' @param id The namespace id for the UI output
#' @param database_vars Database variables returned from user selected database setup module
#' @param abstract_vars Abstraction variables returned from user selected abstraction module
#'
#' @rdname mod_chartreview
#' 
#' @keywords internal
#' @export
#' @importFrom magrittr %>% 
mod_chartreview_server <- function(id, database_vars, abstract_vars) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Render View/Review ----
      ## Determine if View or Review interface is needed based on presence or absence of 
      ## a configured Abstraction Module. 
      ## Both View and Review require a connected database.
      view_review <- reactive({
        req(database_vars()$is_connected == 'yes')
        ## Abstraction Module configured 
        if(database_vars()$is_connected == 'yes' & abstract_vars()$is_configured == 'yes') {
          tagList(
            column(width = 9,
                   box(width = '100%',
                       status = 'primary',
                       ## Patient Chart
                       patient_chart_ui('data-model')
                       )
                   ),
            column(width = 3,
                   ## Abstraction Instrument
                   abstraction_instrument_ui('abs-selector')
                   )
            )
          ## Abstraction Module NOT configured
          } else {
            tagList(
              box(width = 12,
                  status = 'primary',
                  ## Patient Chart
                  patient_chart_ui('data-model')
                  )
              )
            }
        })

      # Output UI ----
      output$view_review <- renderUI({ view_review() })
    }
  )
}