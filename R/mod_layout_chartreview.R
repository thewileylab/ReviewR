# Module Documentation ----
#' Chart Review Interface Module
#' 
#' @description 
#' 
#' This module determines if "View" or "Review" interface is needed based on presence 
#' or absence of a configured abstraction module. When an abstraction module
#' is configured on the "Setup" tab of ReviewR, a column will be created to the 
#' right of the patient chart for the abstraction data collection instrument. 
#' Otherwise, the patient chart will take up the full view.
#' 
#' This module consists of the following components:
#' 
#' ## Module UI function
#' 
#' \itemize{
#' \item{`chartreview_ui`}: A uiOutput containing the View or Review
#' interface
#' }
#' ## Module Server function
#' \itemize{
#' \item{`chartreview_server`}: The logic that controls the layout
#' of the "Chart Review" tab in ReviewR. "View" if no abstraction module
#' is configured. "Review" if abstraction module configured.
#' }
#' @param id The module namespace
#' @name mod_layout_chartreview
#' 
#' @return 
#' *chartreview_ui*:
#' \item{tagList}{The Chart Review UI}
#' *chartreview_server*: 
#' \item{NULL}{This function has no return, other than creating a UI output for the
#' UI function of this module.}
#' 
NULL
#> NULL

# UI ----
#' @rdname mod_layout_chartreview
#' 
#' @keywords internal
chartreview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('view_review'))
    )
  }

# Server ----
#' @rdname mod_layout_chartreview
#' 
#' @keywords internal
#' 
#' @param database_vars A reactiveValues object as returned by \link[ReviewR]{mod_database_setup}.
#' @param abstract_vars A reactiveValues object as returned by \link[ReviewR]{mod_abstraction_setup}.
#' 
#' @importFrom magrittr %>% 
chartreview_server <- function(id, database_vars, abstract_vars) {
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