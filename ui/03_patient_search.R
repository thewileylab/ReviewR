#
# This file contains all elements that are needed to render the Patient Search Tab
#

# Source Patient Search Modules
source('modules/patient_search_module.R')
callModule(patient_search_logic, 'patient_search_ns', db_connection$db_connection, db_setup_vars$data_model, db_connection_vars$bq_dataset)

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
      patient_search_ui('patient_search_ns')
      )
  )
)