#' @import shiny
#' @import shinydashboard
app_ui <- function() {
  stop_nav_jscode <- 'window.onbeforeunload = function() { return true; };'
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(title = 'ReviewR',
                  dashboardHeader(title = tags$a(href='https://github.com/orgs/thewileylab', target='_blank',
                                                 tags$img(src='www/wl_logo.png', width = '125px', height = '50px')
                                                 )
                                  ),
                  dashboardSidebar(sidebarMenu(id = 'main_tabs',
                                               sidebarMenuOutput('application_menu')
                                               ),
                                   uiOutput('reviewr_version')
                                   ),
                  dashboardBody(tags$script(stop_nav_jscode), #Prevent navigation using browser buttons (back, reload, close tab)
                                uiOutput('main_ui')
                                )
                  )
    )
  }

#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useShinydashboard useSweetAlert
golem_add_external_resources <- function() {
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(ext = 'ico'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ReviewR'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::useShinydashboardPlus(),
    shinyWidgets::useSweetAlert()
  )
}
