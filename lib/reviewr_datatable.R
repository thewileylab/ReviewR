
#' ReviewR Datatable
#'
#' @return
#' @export
#'
#' @examples

reviewr_datatable <- function(.data) {
  library(DT)
  datatable(data = .data,
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