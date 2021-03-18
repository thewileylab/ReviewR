#' ReviewR Setup Tab
#'
#' This function contains all of the elements that control the layout of 
#' the Setup Tab.
#'
#' @return [shiny::renderUI] Output containing the Setup Tab
#' 
#' @keywords internal
#' @family layout
#' 
setup <- function() {
  renderUI({
    # Define Setup Tab UI ----
    tagList(
      fluidRow(
        #Box Setup
        box(title = h2("ReviewR Setup", style = 'text-align: center;'),
            width = 12,
            status = 'primary',
            solidHeader = F
            )
        ),
      fluidRow(
        column(width = 6,
               wellPanel(
                 style = 'background: #ebf0f6',
                 ## Database Setup
                 database_setup_ui('db-selector'),
                 ## Data Model Detection
                 data_model_detection_ui('data-model')
                 )
               ),
        column(width = 6,
               wellPanel(
                 style = 'background: #ebf0f6',
                 ## Abstraction Setup
                 abstraction_setup_ui('abs-selector')
                 )
               )
        )
      )
    })
  }
