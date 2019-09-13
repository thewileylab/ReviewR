#
# This file contains all elements that are needed to render the Chart Review Tab
#

# Source Chart Review Modules ----
source('modules/patient_nav_module.R')
source('modules/patient_chart_module.R', keep.source = F)
subject_selection_vars <- callModule(patient_nav_logic, 'chart_review', subject_info$patient_table, subject_info$selected_patient, parent = session)
callModule(subject_info_logic, 'chart_review', subject_info$selected_patient)
callModule(chart_review_logic, 'chart_review', table_map$table_map, db_connection_vars$db_connection, subject_info$selected_patient)

## Outputs ----
## Change layout based on presence or absence of abstraction connection info
output$chart_review <- renderUI({
  req(abstraction_vars$rc_url(), abstraction_vars$rc_url() )
  if(abstraction_vars$rc_url() == '' | abstraction_vars$rc_token() == '' ) { ## Revisit -- conditions should be dependent on valid information being provided.
    box(title = 'No Abstraction',
        width = '100%',
        chart_review_ui('chart_review')
        ) 
    } else {
      fluidRow(
        column(
          width = 9,
          box(
            title = 'Chart Review',
            width = '100%',
            chart_review_ui('chart_review')
            )
          ),
        column(
          width = 3,
          box(
            title = 'Chart Abstraction',
            width = '100%'
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
          uiOutput('chart_review')
    )
    )
  )
})
outputOptions(output, 'chart_review_tab', suspendWhenHidden = F)
