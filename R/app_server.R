#' @import shiny
#' @importFrom glue glue
#' @importFrom shinyBigQuery bigquery_setup_server
#' @importFrom shinyPostgreSQL postgresql_setup_server
#' @importFrom shinyREDCap redcap_server 
#' @importFrom utils packageDescription
app_server <- function(input, output, session) {
  # Main UI  ----
  ## Define a dynamic application menu
  output$application_menu <- renderMenu({
    sidebarMenu(id = 'main_tabs',
                menuItem(tabName = 'homepage', text = 'Homepage',icon = icon('home')),
                menuItem(tabName = 'setup', text = 'Setup', icon = icon('cog')),
                menuItem(tabName = 'patient_search', text = 'Patient Search', icon = icon('users')),
                menuItem(tabName = 'chart_review', text ='Chart Review', icon = icon('table'))
                )
    })
  
  ## Application Menu Outputs
  ### Assign layout functions to Dashboard Tab Outputs
  output$homepage <- homepage()
  output$setup <- setup()
  output$patient_search <- patient_search()
  output$chart_review <- chart_review()
  
  ## Initialize All Dashboard Tabs
  ### Occasionally, observers will not trigger if they are located on a tab that has not been initialized.
  outputOptions(output, 'homepage', suspendWhenHidden = F)
  outputOptions(output, 'setup', suspendWhenHidden = F)
  outputOptions(output, 'patient_search', suspendWhenHidden = F)
  outputOptions(output, 'chart_review', suspendWhenHidden = F)
  
  ## Render Main UI
  output$main_ui <- renderUI({
    tabItems(
      ### https://stackoverflow.com/questions/36817407/content-doesnt-show-up-in-the-dashboard-body-if-the-sidebar-menu-is-dynamically/36819190#36819190
      tabItem(tabName = 'welcome', uiOutput('homepage'), class = 'active'), 
      tabItem(tabName = 'setup', uiOutput('setup')),
      tabItem(tabName = 'patient_search', uiOutput('patient_search')),
      tabItem(tabName = 'chart_review', uiOutput('chart_review'))
      )
    })
  
  ## Render ReviewR Version Info
  output$reviewr_version <- renderUI({ 
    HTML(glue::glue("<small>Version: {packageDescription(pkg = 'ReviewR', fields = 'Version')}</small>") )
    })
  
  # Main UI Observers ---- 
  ## Quit ReviewR  
  ### Close Application when "Leave ReviewR" button is clicked
  observeEvent(input$quit, {
    browser()
    # stopApp()
    })
  
  ## BigQuery Redirect  
  ### After leaving ReviewR to authenticate with Google, take the user back to the Setup Tab to complete database configuration.
  observeEvent(database_vars()$user_info, {
    req(database_vars()$user_info)
    updateTabItems(session, 'main_tabs', selected = 'setup')
    })
  
  # Setup Modules ---- 
  ## Database
  database_vars <- mod_database_setup_server('db-selector')
  ## Database Detection 
  datamodel_vars <- mod_datamodel_detection_server('data-model', database_vars, navigation_vars)
  ## Abstraction
  abstract_vars <- mod_abstraction_setup_server('abs-selector', selected_subject_id)
  
  # Patient Navigation Module ----
  navigation_vars <- mod_navigation_server('pt-navigation', database_vars, datamodel_vars, abstract_vars, session)
  ## Encapsulate 'selected_subject_id' return as a reactive, to be used in abstraction modules
  selected_subject_id <- reactive({ navigation_vars$selected_subject_id })

  # Chart Review Module ----
  mod_chartreview_server('chart-review', database_vars, abstract_vars)
  }
