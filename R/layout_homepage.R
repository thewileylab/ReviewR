#' ReviewR Homepage Tab
#'
#' This file contains all the UI elements that are needed to render the Homepage Tab, which contains basic information about using ReviewR.
#'
#' @return Homepage Tab UI Output
#' @export
#' @keywords internal
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
