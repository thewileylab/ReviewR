#' ReviewR Chart Review Tab
#'
#' This file contains all elements that are needed to render the Chart Review Tab
#'
#' @return Chart Review Tab UI Output
#' @export
#' @keywords internal
#'
chart_review_tab <- function() {
  renderUI({
    tagList(
      fluidPage(
        fluidRow(
          column(
            #Column Setup
            width = 9,
            box(
              #Box Setup
              # title = 'Subject Information',
              width = '100%',
              height = '130px',
              status = 'primary', 
              solidHeader = F,
              #Box Contents
              subject_info('chart_review')
              )
            ),
          column(
            #Column Setup
            width = 3,
            box(
              #Box Setup
              width = '100%',
              height = '130px',
              status = 'primary', 
              solidHeader = F,
              #Box Contents
              patient_nav_ui('chart_review')
              )
            )
          ),
        fluidRow(
          uiOutput('chart_review')
          )
        )
      )
  })
  }
