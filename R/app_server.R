#' @import shiny
#' @importFrom shinyBigQuery bigquery_setup_server
#' @importFrom shinyPostgreSQL postgresql_setup_server
#' @importFrom shinyREDCap redcap_setup_server redcap_instrument_server
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
  
  ## Render Application Menu Outputs
  output$homepage <- homepage()
  output$setup <- setup()
  output$patient_search <- patient_search()
  output$chart_review <- chart_review()
  
  ## Run everything all the time
  ### Certain observers won't fire correctly without this set and if they are located on a tab that isn't in focus
  outputOptions(output, 'homepage', suspendWhenHidden = F)
  outputOptions(output, 'setup', suspendWhenHidden = F)
  outputOptions(output, 'patient_search', suspendWhenHidden = F)
  outputOptions(output, 'chart_review', suspendWhenHidden = F)
  
  ## Render Main UI
  output$main_ui <- renderUI({
    tabItems(
      tabItem(tabName = 'welcome', uiOutput('homepage'), class = 'active'), #https://stackoverflow.com/questions/36817407/content-doesnt-show-up-in-the-dashboard-body-if-the-sidebar-menu-is-dynamically/36819190#36819190
      tabItem(tabName = 'setup', uiOutput('setup')),
      tabItem(tabName = 'patient_search', uiOutput('patient_search')),
      tabItem(tabName = 'chart_review', uiOutput('chart_review'))
    )
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
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Add Setup Modules Here!!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  ## Database Module Setup
  ### Collect Database Setup Variables
  database_setup <- reactiveValues(bigquery = shinyBigQuery::bigquery_setup_server(id = 'bq-setup-namespace'),
                                   postgresql = shinyPostgreSQL::postgresql_setup_server(id = 'pg-setup-namespace')
                                   )
  
  ## Abstraction Module Setup
  ## in this instance, the REDCap instrument needs these variables too. Eventually, will need mod selector for abstraction instruments
  rc_setup_vars <- shinyREDCap::redcap_setup_server(id = 'rc-setup', reset = rc_instrument_vars$reset) 
  ## Collect Abstraction Variables
  abstraction_setup <- reactiveValues(redcap = rc_setup_vars)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  
  ## Setup Tab Selector Modules
  database_vars <- mod_selector_server('db-selector', database_setup)
  abstract_vars <- mod_selector_server('abs-selector', abstraction_setup)
  
  ## Database Detection Module
  ### Module
  datamodel_vars <- mod_datamodel_detection_server('data-model', database_vars)
  ### Output for Patient Search
  output$datamodel_message <- renderText({
    req(datamodel_vars$message)
    datamodel_vars$message
    })
  
  # Patient Navigation Module ----
  ## Patient Navigation
  navigation_vars <- navigation_server(id = 'pt-navigation', database_vars, datamodel_vars, abstract_vars, session)
  # subject_info <- callModule(patient_search_logic, 'patient_search_ns', table_map$table_map, db_connection_vars$db_connection, table_map$db_disconnect, subject_selection_vars$previous_sub, subject_selection_vars$next_sub, subject_selection_vars$subject_id, parent=session)
  # 
  # Chart Review Modules ----
  ## Abstraction Instrument
  subject_id <- reactive({ input$subject_id }) ## Pass to instrument function
  rc_instrument_vars <- shinyREDCap::redcap_instrument_server(id = 'rc-setup', redcap_vars = rc_setup_vars, subject_id = subject_id)
  # ### Load Chart Review Modules
  # subject_selection_vars <- callModule(patient_nav_logic, 'chart_review', subject_info$patient_table, subject_info$selected_patient, parent = session)
  # callModule(subject_info_logic, 'chart_review', instrumentData$previous_data, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection, subject_info$selected_patient, subject_info$selected_patient_info)
  # callModule(omop_chart_review_logic, 'chart_review', table_map$table_map, db_connection_vars$db_connection, subject_info$selected_patient)
  # callModule(mimic_chart_review_logic, 'chart_review', table_map$table_map, db_connection_vars$db_connection, subject_info$selected_patient)
  # callModule(chart_review_ui_logic, 'chart_review', abstraction_vars, table_map, instrument_selection)
  # 
  # ### Call Chart Abstraction Modules
  # instrumentData <- callModule(redcap_instrument_logic, 'chart_review_abstraction', abstraction_vars$rc_con, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection, rc_project_vars$rc_instrument, rc_config_vars$rc_identifier , rc_config_vars$rc_reviewer, rc_config_vars$rc_selected_reviewer, subject_info$selected_patient, upload$abstraction_save_btn_press, abstraction_vars$rc_press)
  # upload <- callModule(instrument_complete_logic, 'chart_review_upload', rc_project_vars$rc_instrument, instrumentData$instrument_data, instrumentData$previous_data, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection, subject_info$selected_patient)
  # callModule(upload_redcap_logic, 'chart_review_abstraction', abstraction_vars$rc_con, rc_project_vars$rc_record_id, rc_project_vars$rc_instrument, instrumentData$instrument_data, instrumentData$previous_data, instrumentData$current_subject, upload$abstraction_save_btn_press, upload$abstraction_complete, upload$abstraction_complete_val, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection)
}
