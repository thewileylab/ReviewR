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
#' @param id The namespace id for the UI output
#' @param database_vars Database variables returned from user selected database setup module
#' @param datamodel_vars Datamodel variables returned from mod_datamodel_setup
#' @param abstract_vars Abstraction variables returned from user selected abstraction module
#'
#' @rdname mod_chartreview
#' 
#' @keywords internal
#' @export
mod_chartreview_server <- function(id, database_vars, datamodel_vars, abstract_vars) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}