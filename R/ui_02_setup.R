# Source required setup modules ----
#' ReviewR Setup Tab
#'
#' This file contains the UI elements that are needed to render the Setup Tab and configure ReviewR. Each element consists of a module, which is rendered within it's own box element on the setup tab. 
#'
#' @return Setup Tab UI Output
#'
#' @examples output$setup_tab <- setup_tab()
setup_tab <- function() {
  renderUI({
    # Define Setup Tab UI ----
    tagList(
      fluidRow(
        box(
          title = h2("ReviewR Setup", style = 'text-align: center;'),
          width = 12,
          status = 'primary',
          solidHeader = F
          )
        ),
    fluidRow(
      column(
        width = 6,
        box(
          #Box Setup
          title = 'Connect to Patient Database',
          width = '100%',
          status = 'primary',
          solidHeader = F,
          #Box Contents
            uiOutput('db_setup'),
            uiOutput('model')
          )
        ),
      column(
        width = 6,
        box(
          #Box Setup
          title = 'Configure Patient Chart Abstraction',
          width = '100%',
          status = 'danger',
          solidHeader = F,
          #Box Contents
            uiOutput('rc_setup'),
            uiOutput('rc_connected')
          ),
        uiOutput('rc_config_ui')
        )
      )
    )
  })
}
