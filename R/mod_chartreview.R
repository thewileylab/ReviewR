# UI ----
#' Chart Review
#'
#' @param id The namespace id for the UI output
#'
#' @rdname mod_chartreview
#' 
#' @keywords internal
#' @export
#'
chartreview_ui <- function(id) {
  ns <- NS(id)
  tagList(
  
  )
}

# Server ----
#' Chart Review
#'
#' @param id 
#'
#' @rdname mod_chartreview
#' 
#' @keywords internal
#' @export
mod_chartreview_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}