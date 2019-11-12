#
# This file contains all elements that are needed to configure ReviewR and render the Setup Tab
#

# Source required setup modules ----

## Patient Database Setup
source('modules/db_setup_module.R')
db_type <- callModule(db_select_logic, 'db_setup_ns')
### Database
db_connection_vars <- callModule(db_connect_logic, 'db_setup_ns', db_type$db_selection, table_map$db_disconnect )
### Data Model Detection
source('modules/data_model_detection_module.R',keep.source = F)
table_map <- callModule(data_model_detection_logic, 'model_ns', db_connection_vars$db_connection, db_connection_vars$connect_press, init_data$supported_models, db_type$db_selection)

## Chart Abstraction Setup
source('modules/chart_abstraction_setup_module.R')
abstraction <- callModule(chart_abstraction_select_logic, 'abstraction_ns')
abstraction_vars <- callModule(chart_abstraction_setup_logic, 'abstraction_ns', abstraction$abstraction_selection)

## REDCap Configuration
instrument_selection <- callModule(redcap_instrument_select_logic, 'abstraction_ns', abstraction_vars$rc_press, abstraction_vars$rc_con)
rc_project_vars <- callModule(redcap_instrument_config_logic, 'abstraction_ns', abstraction_vars$rc_con, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection, init_data$redcap_widget_map)
rc_config_vars <- callModule(redcap_instrument_config_reviewer_logic, 'abstraction_ns', rc_project_vars$rc_instrument, rc_project_vars$rc_identifier)

## BigQuery Redirect Observer. When leaving the application after authenticating with BigQuery, take the user back to the Setup Tab to complete setup. ----
observeEvent(db_connection_vars$bq_token(), {
  if (is.null(db_connection_vars$bq_token() ) ) { # Only redirect when the authorization token is present
    return(NULL)
  } else {
    updateTabItems(session, 'main_tabs', selected = 'setup')
  }
})

## Hide/show the db_setup ui ----
observeEvent(db_connection_vars$connect_press(), {
  shinyjs::hide('db_setup_div',anim = TRUE,animType = 'fade')
  shinyjs::show('data_model_div',anim = TRUE,animType = 'slide')
})

observeEvent(table_map$db_disconnect(), {
  shinyjs::show('db_setup_div',anim = TRUE,animType = 'slide')
  shinyjs::hide('data_model_div',anim = TRUE,animType = 'fade')
})

## Outputs ----
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

output$setup_tab <- renderUI({
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
        chart_abstraction_setup_ui('abstraction_ns')
      ),
    box(
      #Box Setup
      title = 'Configure REDCap Instrument',
      width = '100%',
      status = 'danger',
      solidHeader = F,
      redcap_instrument_config_ui('abstraction_ns')
      #Box Contents
      )
    )
  )
)
})

