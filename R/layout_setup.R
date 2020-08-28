#' ReviewR Setup Tab
#'
#' This file contains the UI elements that are needed to render the Setup Tab and configure ReviewR. Each element consists of a module, which is rendered within it's own box element on the setup tab. 
#'
#' @return Setup Tab UI Output
#' @export
#' @keywords internal
#'
setup_tab <- function() {
  renderUI({
    # Define Setup Tab UI ----
    tagList(
      fluidRow(
        #Box Setup
        box(title = h2("ReviewR Setup", style = 'text-align: center;'),
            width = 12,
            status = 'primary',
            solidHeader = F
            )
        ),
      fluidRow(
        column(width = 6,
               wellPanel(
                 style = 'background: #ebf0f6',
                 ## Database Module Selector
                 mod_selector_ui(id = 'selector',type = 'database') 
                 )
               ),
        column(width = 6,
               wellPanel(
                 style = 'background: #ebf0f6',
                 # Abstraction Module Selector
                 mod_selector_ui(id = 'rc-selector',type = 'abstraction'),
                 selectInput(inputId = 'subject_id',label = 'Subject ID',choices = NULL) 
                 )
               )
        )
      )
    })
  }
