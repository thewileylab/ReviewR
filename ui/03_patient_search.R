#
# This file contains all elements that are needed to render the Patient Search Tab
#

# Source Patient Search Modules
source('modules/patient_search_module.R')
callModule(patient_search_logic, 'patient_search_ns', table_map$table_map, db_connection_vars$db_connection)
#callModule(patient_search_error_logic, 'patient_search_ns', table_map$table_map, db_connection_vars$db_connection)
callModule(data_model_detection_logic, 'patient_search_ns', db_connection_vars$db_connection)

# Define Patient Search Tab UI
tagList(
  fluidPage(
    box(
      #Box Setup
      title = h2('Select a patient to view'),
      width = '100%', 
      status = 'success', 
      solidHeader = F,
      #Box Contents
      patient_search_ui('patient_search_ns'),
      data_model_detection_ui('patient_search_ns')
      )
  )
)