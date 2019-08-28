#
# This file contains all elements that are needed to render the Chart Review Tab
#

source('modules/patient_nav_module.R')
source('modules/patient_chart_module.R')
subject_selection_vars <- callModule(patient_nav_logic, 'chart_review', subject_info$patient_table, subject_info$selected_patient, parent = session)
callModule(subject_info_logic, 'chart_review', subject_info$selected_patient)
callModule(chart_review_logic, 'chart_review', table_map$table_map, db_connection_vars$db_connection, subject_info$selected_patient)

# Define Chart Review Tab UI
output$chart_review_tab <- renderUI({
tagList(
  fluidPage(
    fluidRow(
      column(
        #Column Setup
        width = 9,
        box(
          #Box Setup
          title = h3('Subject Information'),
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
      box(title = 'Patient Stuff Here!',
          width = '100%',
          chart_review_ui('chart_review')
          )
    )
    )
  )
})
outputOptions(output, 'chart_review_tab', suspendWhenHidden = F)
