#' ReviewR Patient Search Tab
#'
#' This function contains all of the elements that control the layout of 
#' the Patient Search Tab.
#'
#' @return [shiny::renderUI] Output containing the Patient Search Tab
#' 
#' @keywords internal
#' @family layout
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
