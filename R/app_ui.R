#' @import shiny
#' @import shinydashboard
#' @importFrom dashboardthemes shinyDashboardThemes
app_ui <- function() {
  stop_nav_jscode <- 'window.onbeforeunload = function() { return true; };'
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(title = 'ReviewR',
                  dashboardHeader(title = tags$a(href='https://github.com/orgs/thewileylab', target='_blank',
                                                 tags$img(src='www/logo.png', width = '125px', height = '50px')
                                                 ),
                                  tags$li(class = 'dropdown', 
                                          actionButton(inputId = 'quit', label = 'Leave ReviewR',
                                                       icon = icon('suitcase-rolling')
                                                       )
                                          )
                                  ),
                  dashboardSidebar(sidebarMenu(id = 'main_tabs',
                                               sidebarMenuOutput('application_menu')
                                               ),
                                   uiOutput('reviewr_version')
                                   ),
                  dashboardBody(shinyDashboardThemes(theme = "blue_gradient"),
                                tags$script(stop_nav_jscode), #Prevent navigation using browser buttons (back, reload, close tab)
                                uiOutput('main_ui')
                                )
                  )
    )
  }

#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useShinydashboard useSweetAlert
golem_add_external_resources <- function(){
  addResourcePath('www', system.file('app/www', package = 'ReviewR') )
  tags$head(golem::activate_js(),
            golem::favicon('www/favicon.png'),
            # Add here all the external resources
            # If you have a custom.css in the inst/app/www
            # Or for example, you can add shinyalert::useShinyalert() here
            # tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
            shinyWidgets::useShinydashboard(),
            shinyWidgets::useSweetAlert(),
            shinyjs::useShinyjs()
            )
  }
