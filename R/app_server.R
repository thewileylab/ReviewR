#' @import shiny
app_server <- function(input, output, session) {
  ## Call ReviewR Setup Tab Modules ----
  ## Patient Database Setup
  db_type <- callModule(db_select_logic, 'db_setup_ns')
  ### Database
  db_connection_vars <- callModule(db_connect_logic, 'db_setup_ns', db_type$db_selection, table_map$db_disconnect )
  ### Data Model Detection
  table_map <- callModule(data_model_detection_logic, 'model_ns', db_connection_vars$db_connection, db_connection_vars$connect_press, db_type$db_selection)
  
  ## Chart Abstraction Setup
  abstraction <- callModule(chart_abstraction_select_logic, 'abstraction_ns')
  abstraction_vars <- callModule(chart_abstraction_setup_logic, 'abstraction_ns', abstraction$abstraction_selection, rc_connected_vars)
  
  ## REDCap Configuration
  instrument_selection <- callModule(redcap_instrument_select_logic, 'abstraction_ns', abstraction_vars$rc_press, abstraction_vars$rc_con)
  rc_project_vars <- callModule(redcap_instrument_config_logic, 'abstraction_ns', abstraction_vars$rc_con, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection)
  rc_connected_vars <- callModule(redcap_connected_logic, 'abstraction_ns', abstraction_vars$rc_press, rc_project_vars$rc_project_info)
  rc_config_vars <- callModule(redcap_instrument_config_reviewer_logic, 'abstraction_ns', rc_project_vars$rc_instrument, rc_project_vars$rc_identifier, abstraction_vars$rc_con)
  rc_reconfig <- callModule(rc_instrument_configured_logic, 'abstraction_ns', rc_config_vars, instrument_selection$rc_instrument_selection)
  
  
  ## Call Patient Search Tab Modules ----
  ### Patient Search Module
  subject_info <- callModule(patient_search_logic, 'patient_search_ns', table_map$table_map, db_connection_vars$db_connection, table_map$db_disconnect, subject_selection_vars$previous_sub, subject_selection_vars$next_sub, subject_selection_vars$subject_id, parent=session, db_connection_vars$connect_press, rc_config_vars$rc_configure_btn_press, instrumentData$rc_identifier_field, instrumentData$review_status)

  ## Call ReviewR Chart Review Tab Modules ----
  ### Load Chart Review Modules
  subject_selection_vars <- callModule(patient_nav_logic, 'chart_review', subject_info$patient_table, subject_info$selected_patient, parent = session)
  callModule(subject_info_logic, 'chart_review', instrumentData$previous_data, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection, subject_info$selected_patient, subject_info$selected_patient_info)
  callModule(omop_chart_review_logic, 'chart_review', table_map$table_map, db_connection_vars$db_connection, subject_info$selected_patient)
  callModule(mimic_chart_review_logic, 'chart_review', table_map$table_map, db_connection_vars$db_connection, subject_info$selected_patient)
  callModule(chart_review_ui_logic, 'chart_review', abstraction_vars, table_map, instrument_selection, rc_reconfig, instrumentData$review_status)
  
  ### Call Chart Abstraction Modules
  instrumentData <- callModule(redcap_instrument_logic, 'chart_review_abstraction', abstraction_vars$rc_con, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection, rc_project_vars$rc_instrument, rc_config_vars$rc_identifier , rc_config_vars$rc_reviewer, rc_config_vars$rc_selected_reviewer, subject_info$selected_patient, upload$abstraction_save_btn_press, upload_vars$modal_continue_button, abstraction_vars$rc_press, rc_reconfig)
  upload <- callModule(instrument_complete_logic, 'chart_review_upload', rc_project_vars$rc_instrument, instrumentData$instrument_data, instrumentData$previous_data, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection, subject_info$selected_patient)
  upload_vars <- callModule(upload_redcap_logic, 'chart_review_abstraction', abstraction_vars$rc_con, rc_project_vars$rc_record_id, rc_project_vars$rc_instrument, instrumentData$instrument_data, instrumentData$previous_data, instrumentData$current_subject, upload$abstraction_save_btn_press, upload$abstraction_complete, upload$abstraction_complete_val, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection)
  
  ## Define Main UI observers ---- 
  ## Close Application when "Leave ReviewR" button is clicked
  observeEvent(input$quit, {
    stopApp()
  })
  ### BigQuery Redirect Observer. When leaving the application after authenticating with BigQuery, take the user back to the Setup Tab to complete setup.
  observeEvent(db_connection_vars$bq_token(), {
    if (is.null(db_connection_vars$bq_token() ) ) { # Only redirect when the authorization token is present
      return(NULL)
    } else {
      updateTabItems(session, 'main_tabs', selected = 'setup')
    }
  })
  ## Define Main UI Outputs ----
  ### Main UI
  ### Define a dynamic application menu
  output$application_menu <- renderMenu({
    sidebarMenu(id = 'main_tabs',
                menuItem(tabName = 'welcome', text = 'Welcome',icon = icon('home')),
                menuItem(tabName = 'setup', text = 'Setup', icon = icon('cog')),
                menuItem(tabName = 'patient_search', text = 'Patient Search', icon = icon('users')),
                menuItem(tabName = 'chart_review', text ='Chart Review', icon = icon('table'))
    )
  })
  ### Render Application Menu Outputs
  output$welcome_tab <- welcome_tab() 
  output$setup_tab <- setup_tab()
  output$patient_search_tab <- patient_search_tab()
  output$chart_review_tab <- chart_review_tab()
  outputOptions(output, 'chart_review_tab', suspendWhenHidden = F)

  ## Render the main UI ----
  output$main_ui <- renderUI({
    tabItems(
      tabItem(tabName = 'welcome', uiOutput('welcome_tab'), class = 'active'), #https://stackoverflow.com/questions/36817407/content-doesnt-show-up-in-the-dashboard-body-if-the-sidebar-menu-is-dynamically/36819190#36819190
      tabItem(tabName = 'setup', uiOutput('setup_tab')),
      tabItem(tabName = 'patient_search', uiOutput('patient_search_tab')),
      tabItem(tabName = 'chart_review', uiOutput('chart_review_tab'))
    )
  })
  
  
  # Setup UI
  ## Define Setup Tab UI observers which animate the ReviewR Setup Elements based on user interaction ----
  ### Hide/show the db_setup ui
  observeEvent(db_connection_vars$connect_press(), {
    shinyjs::hide('db_setup_div',anim = TRUE,animType = 'fade')
    shinyjs::show('data_model_div',anim = TRUE,animType = 'slide')
  })
  
  observeEvent(table_map$db_disconnect(), {
    shinyjs::show('db_setup_div',anim = TRUE,animType = 'slide')
    shinyjs::hide('data_model_div',anim = TRUE,animType = 'fade')
    shinyjs::reset('db_setup_div')
  })
  
  ### Hide/show the REDCap Setup ui
  observeEvent(abstraction_vars$rc_press(), ignoreInit = TRUE, {
    if(nrow(rc_project_vars$rc_project_info()) > 0 ) {
      shinyjs::hide('chart_abstraction_setup_div',anim = TRUE,animType = 'fade')
      shinyjs::show('redcap_instrument_config_div',anim = TRUE,animType = 'slide')
      shinyjs::show('rc_connected_div',anim = TRUE,animType = 'slide')
    }
  })
  
  observeEvent(rc_connected_vars$rc_disconnect(), {
    shinyjs::show('chart_abstraction_setup_div',anim = TRUE,animType = 'slide')
    shinyjs::hide('redcap_instrument_config_div',anim = TRUE,animType = 'fade')
    shinyjs::hide('rc_connected_div',anim = TRUE, animType = 'slide')
    shinyjs::reset('chart_abstraction_setup_div')
  })
  
  ### Hide/show the REDCap Configuration ui
  observeEvent(rc_config_vars$rc_configure_btn_press(), {
    shinyjs::show('rc_configured_div',anim = TRUE,animType = 'slide')
    shinyjs::hide('redcap_instrument_config_choices_div',anim = TRUE,animType = 'fade')
  })
  
  observeEvent(rc_reconfig$rc_reconfig(), {
    shinyjs::hide('rc_configured_div',anim = TRUE,animType = 'fade')
    shinyjs::show('redcap_instrument_config_choices_div',anim = TRUE,animType = 'slide')
    shinyjs::reset('redcap_instrument_config_choices_div')
  })
  
  ## Define Setup Tab UI Outputs, to be controlled by above observers ----
  ### db_setup Outputs
  output$db_setup <- renderUI({ 
    div(id = 'db_setup_div',
        db_setup_ui('db_setup_ns') 
    ) 
  })
  output$model <- renderUI({ 
    div(id = 'data_model_div',
        data_model_detection_ui('model_ns')
    )
  })
  
  ### redcap_setup Outputs
  output$rc_setup <- renderUI({
    div(id = 'chart_abstraction_setup_div',
        chart_abstraction_setup_ui('abstraction_ns')
    )
  })
  output$rc_connected <- renderUI({
    shinyjs::hidden(
      div(id = 'rc_connected_div',
          redcap_connected_ui('abstraction_ns')
      )
    )
  })
  
  ### redcap_config Outputs
  output$rc_config <- renderUI({
    div(id = 'redcap_instrument_config_choices_div',
        redcap_instrument_config_ui('abstraction_ns'))
  })
  output$rc_configured_ui <- renderUI({
    shinyjs::hidden(
      div(id = 'rc_configured_div',
          rc_instrument_configured_ui('abstraction_ns')
      )
    )
  })
  output$rc_config_ui<- renderUI({
    shinyjs::hidden(
      div(id = 'redcap_instrument_config_div',
          box(
            #Box Setup
            title = 'Configure REDCap Instrument',
            width = '100%',
            status = 'danger',
            solidHeader = F,
            #Box Contents
            uiOutput('rc_config'),
            uiOutput('rc_configured_ui')
          )
      )
    )
  })
  
  
  # Patient Search Tab UI
  ## Define Patient Search Tab UI observers ----
  
  ## Patient Search Data Table Observer: open chart review tab when patient id is clicked
  observeEvent(subject_info$dt_selection_info(), {
    selection <- subject_info$dt_selection_info
    if (is.null(selection()$value ) || selection()$col != 0) { # Only redirect if cell contains value and is in column 0 (Subject ID)
      return(NULL)
    } else {
      updateTabItems(session, 'main_tabs', selected = 'chart_review')
    }
  })
  ## Define Patient Search Tab UI Outputs ----
  outputOptions(output, 'patient_search_tab', suspendWhenHidden = F)
  
  ### Create UI element from data detection module on setup tab
  output$data_model <- renderText({
    req(table_map$data_model_text() )
    table_map$data_model_text() 
  })
}
