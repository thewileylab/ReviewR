# Define Patient Search DataTable

patient_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns('patient_search_dt')) %>% withSpinner() 
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
      ## Determine user table from table map
      patients_table <- table_map()$model_match[[1]] %>% 
        filter(table == patients_omop) %>% 
        distinct(table, .keep_all = T) %>% 
        select(user_database_table) %>% 
        pluck(1)
      ## Determine petient id field from table map
      subject_field <- table_map()$model_match[[1]] %>% 
        filter(table == patients_omop & field == id_omop) %>% 
        select(user_fields) %>% 
        pluck(1)
      ## Connect to table
      tbl(db_connection(), patients_table) %>% 
        rename(ID = subject_field) %>% 
        select(ID, everything()) %>% 
        arrange(ID) %>% 
        collect()
    } else if(table_map()$count_filtered != 0 & table_map()$data_model == 'mimic3') {
      ## MIMIC Patient Search
      patients_mimic <- 'patients'
      id_mimic <- 'subject_id'
      ## Determine user table from table map
      patients_table <- table_map()$model_match[[1]] %>% 
        filter(table == patients_mimic) %>% 
        distinct(table, .keep_all = T) %>% 
        select(user_database_table) %>% 
        pluck(1)
      ## Determine petient id field from table map
      subject_field <- table_map()$model_match[[1]] %>% 
        filter(table == patients_mimic & field == id_mimic) %>% 
        select(user_fields) %>% 
        pluck(1)
      ## Connect to table
      tbl(db_connection(), patients_table) %>%
        rename(ID = subject_field) %>% 
        select(ID, everything()) %>%
        arrange(ID) %>% 
        collect()
    } else {
      return(NULL)
    }
  })
  
  ## Render Patient Search Data Table
  output$patient_search_dt <- renderDataTable({
    req(patient_search_tbl())
    patient_search_tbl() %>% 
      datatable(options = list(searchHighlight = TRUE, 
                               scrollX = TRUE, 
                               scrollY = '600px', 
                               search = list(regex = TRUE, 
                                             caseInsensitive = TRUE),
                               pageLength = 25
                               ),
                rownames = F, 
                selection = 'single',
                escape = F,
                filter = 'top',
                class = 'cell-border strip hover'
                ) %>% 
      formatStyle('ID', color = '#0000EE', cursor = 'pointer') # Format the ID column to appear blue and change the mouse to a pointer
    })
  ## Extract the selected patient id from the patient data table when clicked and store as a reactive
  selected_patient <- reactive({ input$patient_search_dt_cell_clicked })
  
  return(list(
    'patient_table' = patient_search_tbl,
    'selected_patient' = selected_patient
  ))
}

