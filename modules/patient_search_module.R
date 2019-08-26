# Define Patient Search DataTable

patient_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns('patient_search_dt')) %>% withSpinner() 
  )
}

patient_search_logic <- function(input, output, session, table_map, db_connection, disconnect, prev_sub, next_sub, selected_sub, parent) {
  library(tibble)
  library(DT)
  library(lubridate)
  ns <- session$ns
  
  #Replace Patient Search Table when table map changes
  observeEvent(table_map(), {
    reloadData(proxy = patient_search_proxy,
               resetPaging = T,
               clearSelection = T)
  })
  
  # Extract patients based on presence of connection info and data model
  patient_search_tbl <- eventReactive(table_map(), {
    req(db_connection() )
    if (table_map()$count_filtered != 0 & table_map()$data_model == 'omop') {
      source('lib/omop_tables.R',keep.source = F)
      all_patients_table_omop(table_map, db_connection)
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
    # The next time you think about implementing FixedColumns, check the status of this issue first: https://github.com/rstudio/DT/issues/275
    patient_search_tbl() %>% 
      rename('Subject ID' = ID) %>% 
      datatable(extensions = list('Scroller' = NULL
                                  ),
                options = list(scrollX = TRUE,
                               deferRender = TRUE,
                               scrollY = '600px',
                               scroller = TRUE,
                               searchHighlight = TRUE, 
                               search = list(regex = TRUE, 
                                             caseInsensitive = TRUE)
                               ),
                rownames = F, 
                selection = 'single',
                escape = F,
                filter = 'top',
                class = 'cell-border strip hover'
                ) %>% 
      formatStyle('Subject ID', 
                  color = '#0000EE', 
                  cursor = 'pointer',  # Format the ID column to appear blue and change the mouse to a pointer
                  textAlign = 'left'
                  )
    })
  outputOptions(output, 'patient_search_dt', suspendWhenHidden = F) #This output needs to run all the time, so that it can receive data from the Setup tab
  
  ## Create a DT Proxy to keep DT selection up to date with Patient Nav on Chart Review Tab
  patient_search_proxy <- DT::dataTableProxy(outputId = ns('patient_search_dt'), session = parent)
  
  ## On Previous Subject Button Press, update selected row in DT
  observeEvent(prev_sub(), {
    req(patient_search_tbl(), input$patient_search_dt_rows_selected )
    if(input$patient_search_dt_rows_selected == 1){ ## Special case when at the beginning of the list, cycle to last
      DT::selectRows(patient_search_proxy, nrow(patient_search_tbl() ))
    } else { DT::selectRows(patient_search_proxy, input$patient_search_dt_rows_selected - 1)
        }
  })
  ## On Next Subject Button Press, updated selected row in DT
  observeEvent(next_sub(), {
    req(patient_search_tbl(), input$patient_search_dt_rows_selected )
    if(input$patient_search_dt_rows_selected == nrow(patient_search_tbl() )){ ## Special case when at the end of the list, cycle to beginning
      DT::selectRows(patient_search_proxy, 1)
    } else { DT::selectRows(patient_search_proxy, input$patient_search_dt_rows_selected + 1)
    }
  })
  outputOptions(output, 'patient_search_dt', suspendWhenHidden = F)
  
  ## When a choice is made from the patient nav dropdown, update the selected row in DT
  observeEvent(selected_sub(), {
    req(patient_search_tbl(), selected_sub(), input$patient_search_dt_rows_selected )
    sub_row_id <- patient_search_tbl() %>%
      rowid_to_column(var = 'row_id') %>%
      filter(ID == selected_sub() ) %>%
      select(row_id) %>%
      slice(1)
    DT::selectRows(patient_search_proxy, sub_row_id)
    })
  
  ## Extract the selected patient id from the patient data table when clicked and store as a reactive
  select_patient_click <- reactive({ input$patient_search_dt_cell_clicked })
  selected_patient <- reactive({ 
    req(patient_search_tbl(), input$patient_search_dt_rows_selected )
    patient_search_tbl() %>% 
      select(ID) %>% 
      slice(input$patient_search_dt_rows_selected)
    })
  
  return(list(
    'patient_table' = patient_search_tbl,
    'dt_selection_info' = select_patient_click,
    'selected_patient' = selected_patient
  ))
}

