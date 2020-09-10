#' ReviewR Patient Search Tab
#'
#' This file contains all elements that are needed to render the Patient Search Tab
#'
#' @return Patient Search Tab UI Output
#' @export
#' @keywords internal
#'
patient_search <- function() {
  renderUI({
    tagList(
      fluidRow(
        #Box Setup
        box(title = h2('Select a Patient'),
            width = 12, 
            status = 'primary', 
            solidHeader = F,
            #Box Contents
            navigation_message('pt-navigation'),
            all_patient_search_dt('pt-navigation')
            )
        )
      )
    })
  }
