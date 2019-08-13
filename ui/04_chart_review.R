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
          title = h2(paste('Subject ID -',patient_info$selected_patient()$value)),
          width = '100%',
          height = '130px',
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
          height = '130px',
          status = 'success', 
          solidHeader = F,
          #Box Contents
          selectInput(inputId = 'subject_id',
                      label = 'Subject ID', 
                      choices = patient_info$patient_table() %>% 
                        select(ID) %>% 
                        deframe(),
                      selected = patient_info$selected_patient()$value
                      ),
          actionButton(inputId = 'previous',label = '<--Previous',width = '155px'),
          actionButton(inputId = 'next',label = 'Next-->',width = '155px')
          )
        )
      )
    )
  )
})