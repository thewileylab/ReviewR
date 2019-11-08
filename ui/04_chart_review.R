#
# This file contains all elements that are needed to render the Chart Review Tab
#

# Source Chart Review Tab Modules ----
source('modules/patient_nav_module.R')
source('modules/patient_chart_module.R', keep.source = F)
source('modules/save_abstraction_module.R')
# Load Chart Review Modules ----
subject_selection_vars <- callModule(patient_nav_logic, 'chart_review', subject_info$patient_table, subject_info$selected_patient, parent = session)
callModule(subject_info_logic, 'chart_review', subject_info$selected_patient)
callModule(omop_chart_review_logic, 'chart_review', table_map$table_map, db_connection_vars$db_connection, subject_info$selected_patient)
callModule(mimic_chart_review_logic, 'chart_review', table_map$table_map, db_connection_vars$db_connection, subject_info$selected_patient)

# Call Chart Abstraction Modules ----
instrumentData <- callModule(redcap_instrumment_logic, 'chart_review_abstraction', abstraction_vars$rc_con, instrument_selection$rc_instruments, instrument_selection$rc_instrument_selection, rc_project_vars$rc_instrument, rc_config_vars$rc_identifier , rc_config_vars$rc_reviewer, subject_info$selected_patient, upload$abstraction_save_btn_press, abstraction_vars$rc_press)
upload <- callModule(instrument_complete_logic, 'chart_review_upload', rc_project_vars$rc_instrument, instrumentData$instrument_data)
callModule(upload_redcap_logic, 'chart_review_abstraction', abstraction_vars$rc_con, rc_project_vars$rc_record_id, rc_project_vars$rc_instrument, instrumentData$instrument_data, instrumentData$previous_data, instrumentData$current_subject, upload$abstraction_save_btn_press)

# # RC Test observer
# observeEvent(upload$abstraction_save_btn_press(), {
#   browser()
#   })

## Outputs ----
output$abstraction <- renderUI({ redcap_instrument_ui('chart_review_abstraction') })

## Change layout based on presence or absence of abstraction connection info
output$chart_review <- renderUI({ 
  req(abstraction_vars$rc_url(), abstraction_vars$rc_url(), table_map$table_map())
  ## Revisit -- conditions should be dependent on valid information being provided.
  if(abstraction_vars$rc_url() == '' | abstraction_vars$rc_token() == '' ) { ## No Abstraction ----
    box(width = '100%',
        ## Select patient chart ui based on data model
        if(table_map$table_map()$data_model == 'omop') {
          omop_chart_review_ui('chart_review')
        } else if (table_map$table_map()$data_model == 'mimic3') {
          mimic_chart_review_ui('chart_review')
        } else {return(NULL)}
        ) 
    } else { ## Abstraction ----
      fluidRow(
        column(
          width = 9,
          box(width = '100%',
              status = 'primary',
              ## Select patient chart ui based on data model
              if(table_map$table_map()$data_model == 'omop') {
                omop_chart_review_ui('chart_review')
                } else if (table_map$table_map()$data_model == 'mimic3') {
                  mimic_chart_review_ui('chart_review')
                  } else {return(NULL)}
              )
          ),
        column(
          width = 3,
          box(
            title = instrument_selection$rc_instrument_selection(),
            width = '100%',
            status = 'danger',
            uiOutput('abstraction'),
            ## CSS to scroll the abstraction instrument, if necessary
            tags$head(
              tags$style("#abstraction{color:black; font-size:12px; font-style:italic; overflow-y:scroll; max-height: 675px; background: ghostwhite;}")
              )
            ),
          box(
            title = 'Save Abstraction',
            width = '100&',
            status = 'danger',
            instrument_complete_ui('chart_review_upload')
          )
          )
        )
      }
  })

# Define Chart Review Tab UI ----
output$chart_review_tab <- renderUI({
tagList(
  fluidPage(
    fluidRow(
      column(
        #Column Setup
        width = 9,
        box(
          #Box Setup
          title = 'Subject Information',
          width = '100%',
          #height = '130px',
          status = 'success', 
          solidHeader = F,
          #Box Contents
          subject_info('chart_review')
        )
      ),
      column(
        #Column Setup
        width = 3,
        box(
          #Box Setup
          width = '100%',
          #height = '130px',
          status = 'success', 
          solidHeader = F,
          #Box Contents
          patient_nav_ui('chart_review')
          )
        )
      ),
    fluidRow(
          uiOutput('chart_review')
    )
    )
  )
})
outputOptions(output, 'chart_review_tab', suspendWhenHidden = F)
