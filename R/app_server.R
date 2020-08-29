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
  output$welcome_tab <- homepage() 
  output$setup_tab <- setup()
  output$patient_search_tab <- patient_search()
  # output$chart_review_tab <- chart_review()
  
  ## Run everything all the time
  ### Certain observers won't fire correctly without this set and if they are located on a tab that isn't in focus
  outputOptions(output, 'setup_tab', suspendWhenHidden = F)
  outputOptions(output, 'patient_search_tab', suspendWhenHidden = F)
  # outputOptions(output, 'chart_review_tab', suspendWhenHidden = F)
  
  ## Render Main UI
  output$main_ui <- renderUI({
    tabItems(
      tabItem(tabName = 'welcome', uiOutput('welcome_tab'), class = 'active'), #https://stackoverflow.com/questions/36817407/content-doesnt-show-up-in-the-dashboard-body-if-the-sidebar-menu-is-dynamically/36819190#36819190
      tabItem(tabName = 'setup', uiOutput('setup_tab')),
      tabItem(tabName = 'patient_search', uiOutput('patient_search_tab'))
      # tabItem(tabName = 'chart_review', uiOutput('chart_review_tab'))
    )
  })
  
  # Setup Modules ----
  ## Database Module Setup
  bq_setup_vars <- shinyBigQuery::bigquery_setup_server(id = 'bq-setup-namespace')
  pg_setup_vars <- shinyPostgreSQL::postgresql_setup_server(id = 'pg-setup-namespace')
  ## DB Var Config
  database_setup <- reactiveValues(bigquery = bq_setup_vars,
                                   postgresql = pg_setup_vars
                                   )
  
  ## Abstraction Module Setup
  rc_setup_vars <- shinyREDCap::redcap_setup_server(id = 'rc-setup', reset = rc_instrument_vars$reset)
  
  ## Abstraction Instrument
  subject_id <- reactive({ input$subject_id }) ## Pass to instrument function
  rc_instrument_vars <- shinyREDCap::redcap_instrument_server(id = 'rc-setup', redcap_vars = rc_setup_vars, subject_id = subject_id)
  
  ## Abstraction Var Config
  abs_setup <- reactiveValues(redcap = rc_setup_vars)
  
  ## Setup Tab Selector Modules
  database_vars <- mod_selector_server('db-selector', database_setup)
  abstract_vars <- mod_selector_server('abs-selector', abs_setup)
  
  ## Database Detection Module
  ### Module
  data_model_vars <- mod_data_model_detection_server('data-model', database_vars)
  ### Output for Patient Search
  output$data_model_message <- renderText({
    req(data_model_vars$message )
    data_model_vars$message
    })
  
  # Call Patient Search Tab Modules ----
  ## Patient Search Module
  # subject_info <- callModule(patient_search_logic, 'patient_search_ns', table_map$table_map, db_connection_vars$db_connection, table_map$db_disconnect, subject_selection_vars$previous_sub, subject_selection_vars$next_sub, subject_selection_vars$subject_id, parent=session)
  # 
  # ## Call ReviewR Chart Review Tab Modules ----
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
  
  ## Main UI observers ---- 
  ## Quit ReviewR Observer 
  ### Close Application when "Leave ReviewR" button is clicked
  observeEvent(input$quit, {
    browser()
    # stopApp()
  })
  ## BigQuery Redirect Observer. 
  ### When leaving the application after authenticating with BigQuery, take the user back to the Setup Tab to complete setup.
  observeEvent(database_vars()$user_info, {
    if (is.null(database_vars()$user_info ) ) { # Only redirect when user information is present
      return(NULL)
    } else {
      updateTabItems(session, 'main_tabs', selected = 'setup')
    }
  })
}
