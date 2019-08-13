#
# This file contains all elements that are needed to configure ReviewR and render the Setup Tab
#

# Source required setup modules----------

## Patient Database Setup
source('modules/db_setup_module.R')
db_type <- callModule(db_select_logic, 'db_setup_ns')
### Database
db_connection_vars <- callModule(db_connect_logic, 'db_setup_ns', db_type$db_selection )
### DB Detection
source('modules/data_model_detection_module.R',keep.source = F)
table_map <- callModule(data_model_detection_logic, 'model_ns', db_connection_vars$db_connection)

## Chart Abstraction Setup
source('modules/chart_abstraction_setup_module.R')
abstraction <- callModule(chart_abstraction_select_logic, 'abstraction_ns')
abstraction_vars <- callModule(chart_abstraction_setup_logic, 'abstraction_ns', abstraction$abstraction_selection)


output$setup_tab <- renderUI({
# Define Setup Tab UI ----------
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
       db_setup_ui('db_setup_ns'),
       data_model_detection_ui('model_ns')
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
      solidHeader = F
      #Box Contents
      )
    )
  )
)
})