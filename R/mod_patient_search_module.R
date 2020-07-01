#' Patient Search Module
#'
#' This module will render the datatable on the 'Patient Search' tab containing all patients in the cohort. The selected patient in the DT is kept in sync with the 'Chart Review' tab.
#' 
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_patient_search_module
#' 
#' @keywords internal
#' @export
#' @import shiny 
#' @importFrom shinycssloaders withSpinner
#' 
patient_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('patient_search_dt')) %>% withSpinner() 
  )
}

#' @param table_map tibble containing a the cdm that most closely matches the user's database and a map of standard tables to user tables
#' @param db_connection Connection info received from the database setup module
#' @param disconnect disconnect button press
#' @param prev_sub previous subject button press
#' @param next_sub next subject button press
#' @param selected_sub the selected subject
#' @param parent the parent environment of this module
#' @param rc_con REDCap project connection info
#'
#' @rdname mod_patient_search_module
#' 
#' @keywords internal
#' @export
#' @import shiny 
#' @importFrom DT reloadData formatStyle selectRows dataTableProxy
#' @importFrom dplyr rename slice filter select pull mutate_at
#' @importFrom tibble rowid_to_column
#' @importFrom rlang .data
patient_search_logic <- function(input, output, session, table_map, db_connection, disconnect, prev_sub, next_sub, selected_sub, parent, db_connect, rc_config, rc_identifier, review_status) {
  ns <- session$ns
  
  observeEvent(c(table_map(),rc_config(), status_test(), review_status() ), {
    req(table_map() )
    DT::reloadData(proxy = patient_search_proxy,
               resetPaging = T,
               clearSelection = T)
  })
  
  # Extract patients based on presence of connection info and data model
  patient_search_tbl <- eventReactive(table_map(), {
    req(db_connection() )
    # browser()
    if (table_map()$count_filtered != 0 & table_map()$data_model == 'omop') {
      omop_table_all_patients(table_map, db_connection)
    } else if(table_map()$count_filtered != 0 & table_map()$data_model == 'mimic3') {
      ## MIMIC Patient Search
      mimic_table_all_patients(table_map, db_connection)
    } else {
      return(NULL)
    }
  })
  status_test <- reactive({
    tryCatch({
      review_status()
      return('abstraction')
    },
    error=function(error_condition) {
      return('no_abstraction')
    })
  })
  patient_search_output <- reactive({
    if(status_test() == 'no_abstraction') {
      patient_search_tbl() 
    } else if ( status_test() == 'abstraction') {
      patient_search_tbl() %>% 
            left_join(review_status(), by = c('ID' = rc_identifier() )) %>%
            mutate_at(vars(contains('REDCap')), replace_na, 'Review Not Started')
    } else {return(NULL) }
    
  })
  ## Render Patient Search Data Table
  output$patient_search_dt <-DT::renderDataTable({
    patient_search_output() %>% 
      rename('Subject ID' = .data$ID) %>%
      reviewr_datatable() %>%
      formatStyle('Subject ID',
                  color = '#0000EE',
                  cursor = 'pointer',  # Format the ID column to appear blue and change the mouse to a pointer
                  textAlign = 'left'
      )
    })
 
    # The next time you think about implementing FixedColumns, check the status of this issue first: https://github.com/rstudio/DT/issues/275
    
  outputOptions(output, 'patient_search_dt', suspendWhenHidden = F)

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

  ## When a choice is made from the patient nav dropdown, update the selected row in DT
  observeEvent(selected_sub(), {
    req(patient_search_tbl(), selected_sub(), input$patient_search_dt_rows_selected )
    sub_row_id <- patient_search_tbl() %>%
      rowid_to_column(var = 'row_id') %>%
      filter(.data$ID == selected_sub() ) %>%
      select(.data$row_id) %>%
      slice(1)
    DT::selectRows(patient_search_proxy, sub_row_id)
    })
  
  ## Extract the selected patient id from the patient data table when clicked and store as a reactive
  select_patient_click <- reactive({ input$patient_search_dt_cell_clicked })
  selected_patient <- reactive({ 
    req(patient_search_tbl(), input$patient_search_dt_rows_selected )
    patient_search_tbl() %>% 
      slice(input$patient_search_dt_rows_selected) %>% 
      pull(.data$ID)
    })
  
  selected_patient_info <- reactive({ 
    req(patient_search_output(), input$patient_search_dt_rows_selected )
    patient_search_output() %>% 
      slice(input$patient_search_dt_rows_selected)
    })
  
  return(list(
    'patient_table' = patient_search_output,
    'dt_selection_info' = select_patient_click,
    'selected_patient' = selected_patient,
    'selected_patient_info' = selected_patient_info
  ))
}

