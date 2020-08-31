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
      fluidPage(
        #Box Setup
        box(title = h2('Select A Patient'),
            width = '100%', 
            status = 'primary', 
            solidHeader = F,
            #Box Contents
            HTML('To select a patient, please click the desired Subject ID from the table below:'),
            all_patient_search_dt('pt-navigation'),
            uiOutput('datamodel_message')
            )
        )
      )
    })
  }
