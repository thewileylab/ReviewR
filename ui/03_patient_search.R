#
# This file contains all elements that are needed to render the Patient Search Tab
#

# Source Patient Search Modules ----
source('modules/patient_search_module.R', keep.source = F)
subject_info <- callModule(patient_search_logic, 'patient_search_ns', table_map$table_map, db_connection_vars$db_connection, table_map$db_disconnect, subject_selection_vars$previous_sub, subject_selection_vars$next_sub, subject_selection_vars$subject_id, parent=session)

## Patient Search Data Table Observer: open chart review tab when patient id is clicked ----
observeEvent(subject_info$dt_selection_info(), {
    selection <- subject_info$dt_selection_info
    if (is.null(selection()$value ) || selection()$col != 0) { # Only redirect if cell contains value and is in column 0 (Subject ID)
        return(NULL)
    } else {
      updateTabItems(session, 'main_tabs', selected = 'chart_review')
      }
    })


## Create UI element from data detection module on setup tab ----
output$data_model <- renderText({
  req(table_map$data_model_text() )
  table_map$data_model_text() 
  })

# Define Patient Search Tab UI ----

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
        patient_search_ui('patient_search_ns'),
        uiOutput('data_model')
        )
      )
    )
  })

outputOptions(output, 'patient_search_tab', suspendWhenHidden = F)
