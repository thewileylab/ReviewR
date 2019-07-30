# Define Patient Search DataTable

patient_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns('patient_search_dt')) %>% withSpinner()
  )
}

patient_search_logic <- function(input, output, session, db_connection, data_model, dataset) {
  library(tibble)
  library(DT)
  ns <- session$ns
  
  # Extract patients based on presence of connection info and data model
  patient_search_tbl <- reactive({
    if(is.null( db_connection() )) {
      return(NULL)
    } else if (!is.null( db_connection() ) & data_model() == 'OMOP') {
      patients <- paste0(dataset(), '.person')
      tbl(db_connection(), patients) %>% 
        collect()
    }
  })
  
  output$patient_search_dt <- renderDataTable({ 
    patient_search_tbl() %>% 
      datatable(options = list(searchHighlight = TRUE, 
                               scrollX = TRUE, 
                               scrollY = '600px', 
                               search = list(regex = TRUE, 
                                             caseInsensitive = TRUE),
                               pageLength = 25),
                rownames = F, 
                selection = 'none',
                escape = F,
                filter = 'top')
    })
}