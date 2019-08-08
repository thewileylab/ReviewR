# Define Patient Search DataTable

patient_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns('patient_search_dt')) %>% withSpinner() 
    # uiOutput(ns('patient_search_error'))
  )
}

patient_search_logic <- function(input, output, session, table_map, db_connection) {
  library(tibble)
  library(DT)
  ns <- session$ns
  
  # Extract patients based on presence of connection info and data model
  patient_search_tbl <- reactive({
    req(table_map(), db_connection() )
    if (table_map()$count_filtered != 0 & table_map()$data_model == 'omop') {
      ## OMOP Patient Search
      patients_omop <- 'person'
      id_omop <- 'person_id'
      patients_table <- table_map()$model_match[[1]] %>% 
        filter(table == patients_omop) %>% 
        distinct(table, .keep_all = T) %>% 
        select(user_database_table) %>% 
        pluck(1)
      subject_field <- table_map()$model_match[[1]] %>% 
        filter(table == patients_omop & field == id_omop) %>% 
        select(user_fields) %>% 
        pluck(1)
      tbl(db_connection(), patients_table) %>% 
        rename(ID = subject_field) %>% 
        collect()
    } else if(table_map()$count_filtered != 0 & table_map()$data_model == 'mimic3') {
      ## MIMIC Patient Search
      patients_mimic <- 'patients'
      id_mimic <- 'subject_id'
      patients_table <- table_map()$model_match[[1]] %>% 
        filter(table == patients_mimic) %>% 
        distinct(table, .keep_all = T) %>% 
        select(user_database_table) %>% 
        pluck(1)
      subject_field <- table_map()$model_match[[1]] %>% 
        filter(table == patients_mimic & field == id_mimic) %>% 
        select(user_fields) %>% 
        pluck(1)
      tbl(db_connection(), patients_table) %>%
        rename(ID = subject_field) %>% 
        collect()
    } else {
      return(NULL)
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

# patient_search_error_logic <- function(input, output, session, table_map, db_connection) {
#   patient_search_error <- reactive({
#     if (!is.null( db_connection() ) & table_map()$count_filtered == 0) {
#         tagList(
#           div('This database does not seem to be in OMOP or MIMIC III format?')
#           )
#     } else {
#       return(NULL)
#       }
#   })
#   
#   output$patient_search_error <- renderUI({ patient_search_error() })
# }
