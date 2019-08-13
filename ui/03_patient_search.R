#
# This file contains all elements that are needed to render the Patient Search Tab
#

# Source Patient Search Modules
source('modules/patient_search_module.R', keep.source = F)
patient_info <- callModule(patient_search_logic, 'patient_search_ns', table_map$table_map, db_connection_vars$db_connection)
callModule(data_model_detection_logic, 'patient_search_ns', db_connection_vars$db_connection)

## Patient Search Data Table Observer: open chart review tab when patient id is clicked.
observeEvent(patient_info$selected_patient(), {
    patient_id <- patient_info$selected_patient
    if (is.null(patient_id()$value ) || patient_id()$col != 0) { # Only redirect if cell contains value and is in column 0 (ID)
        return(NULL)
    } else {
      updateTabItems(session, 'main_tabs', selected = 'chart_review')
      }
})

# Define Patient Search Tab UI

output$patient_search_tab <- renderUI({
tagList(
  fluidPage(
    box(
      #Box Setup
      title = h2('Select a patient to view'),
      width = '100%', 
      status = 'success', 
      solidHeader = F,
      #Box Contents
      div('To select a patient, please click the desired Subject ID in the table below:'),
      br(),
      patient_search_ui('patient_search_ns'),
      data_model_detection_ui('patient_search_ns')
      )
  )
)
})
