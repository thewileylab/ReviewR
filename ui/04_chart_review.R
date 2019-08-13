#
# This file contains all elements that are needed to render the Chart Review Tab
#

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
          title = h2('Record ID -'),
          width = '100%',
          height = '125px',
          status = 'success', 
          solidHeader = F
          #Box Contents
        )
      ),
      column(
        #Column Setup
        width = 3,
        box(
          #Box Setup
          width = '100%',
          height = '125px',
          status = 'success', 
          solidHeader = F,
          #Box Contents
          selectInput(inputId = 'subject_id',
                      label = 'Record ID', 
                      choices = patient_info$patient_table() %>% 
                        select(ID) %>% 
                        deframe()
                      )
          )
        )
      )
    )
  )
})