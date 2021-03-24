#' ReviewR Homepage Tab
#'
#' This function contains all of the elements that control the layout of 
#' the Homepage Tab.
#' 
#' @return [shiny::renderUI] Output containing the Homepage Tab 
#' 
#' @keywords internal
#' @family layout
#' 
homepage <- function() {
  renderUI({
    tagList(
      fluidRow(
        box(title = h2("Welcome to ReviewR", style='text-align: center;'),
            width = 12,
            status = 'primary', 
            solidHeader = F
            )
        ),
      fluidRow(
        box(width = 12,
            status = 'primary', 
            solidHeader = F, 
            HTML("ReviewR is a portable tool to help you explore data across different data models.  Within ReviewR, you can browse patient data stored in either the OMOP or MIMIC-III data model.<br>"),
            br(),
            HTML("In addition to viewing patient data, you may also connect to a REDCap project to perform a chart review<br>"),
            br(),
            HTML("To get started, please complete the 'Setup' step (found in the left navigation menu)<br>")
            )
        )
      )
    })
  }
