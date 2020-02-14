#' ReviewR Datatable
#'
#' @param .data A local tibble or dataframe to be rendered in the ReviewR UI
#'
#' @return return a DT with custom options
#' @keywords internal
#' @export 
#' @importFrom DT datatable
#'
reviewr_datatable <- function(.data) {
  DT::datatable(data = .data,
            extensions = list('Scroller' = NULL),
  options = list(scrollX = TRUE,
                 deferRender = TRUE,
                 scrollY = '600px',
                 scroller = TRUE,
                 searchHighlight = TRUE, 
                 search = list(regex = TRUE, 
                               caseInsensitive = TRUE)
  ),
  rownames = F, 
  selection = 'single',
  escape = F,
  filter = 'top',
  class = 'cell-border strip hover'
  )
}
