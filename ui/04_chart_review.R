#
# This file contains all elements that are needed to render the Chart Review Tab
#

source('modules/chart_review_module.R')
test <- callModule(patient_nav_logic, 'chart_review', subject_info$patient_table, subject_info$dt_selection_info, parent = session)
callModule(subject_info_logic, 'chart_review', test$subject_id)

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
          title = h2('Subject Information'),
          width = '100%',
          height = '130px',
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
          height = '130px',
          status = 'success', 
          solidHeader = F,
          #Box Contents
          patient_nav_ui('chart_review')
          )
        )
      )
    )
  )
})