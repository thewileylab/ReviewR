#' ReviewR Chart Review Tab
#'
#' This function contains all of the elements that control the layout of 
#' the Chart Review Tab.
#'
#' @return [shiny::renderUI] Output containing the Chart Review Tab 
#'
#' @keywords internal
#' @family layout
#'
chart_review <- function() {
  renderUI({
    tagList(
      fluidRow(
        #Column Setup
        column(width = 9,
               #Box Setup
               box(width = '100%',
                   # height = '130px',
                   status = 'primary',
                   solidHeader = F,
                   #Box Contents
                   chart_review_subject_info('pt-navigation')
                   )
               ),
        #Column Setup
        column(width = 3,
               #Box Setup
               box(width = '100%',
                   # height = '130px',
                   status = 'primary', 
                   solidHeader = F,
                   #Box Contents
                   chart_review_navigation('pt-navigation')
                   )
               )
        ),
      fluidRow(
        chartreview_ui('chart-review')
        )
      )
    })
  }
