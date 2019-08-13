

output$welcome_tab <- renderUI({
fluidRow(
  box(status = 'primary', solidHeader = F, width = 12,
      h2("Welcome to ReviewR", style='text-align: center;')
      ),
  box(status = 'primary', solidHeader = F, width = 12,
      div("ReviewR is a portable tool to help you explore data across different data models.  Within ReviewR, you can browse patient data stored in either the OMOP or MIMIC-III data model."),
      br(),
      div("In addition to viewing patient data, you may also connect to a REDCap project to perform a chart review"),
      br(),
      div("To get started, please complete the 'Setup' step (found in the left navigation menu)")
      )
  )
})