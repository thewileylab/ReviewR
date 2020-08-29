#' ReviewR Chart Review Tab
#'
#' This file contains all elements that are needed to render the Chart Review Tab
#'
#' @return Chart Review Tab UI Output
#' @export
#' @keywords internal
#'
chart_review <- function() {
  renderUI({
    tagList(
      fluidPage(
        fluidRow(
          #Column Setup
          column(width = 9,
                 #Box Setup
                 box(width = '100%',
                     height = '130px',
                     status = 'primary', 
                     solidHeader = F,
                     #Box Contents
                     subject_info('chart_review')
                     )
                 ),
          #Column Setup
          column(width = 3,
                 #Box Setup
                 box(width = '100%',
                     height = '130px',
                     status = 'primary', 
                     solidHeader = F,
                     #Box Contents
                     patient_nav_ui('chart_review')
                     )
                 )
          ),
        fluidRow(
          # Column Setup
          column(width = 12,
                 chart_review_ui('chart_review')
                 )
          )
        )
      )
    })
  }
