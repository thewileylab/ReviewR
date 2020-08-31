# Helper Functions ---- 
#' ReviewR Datatable
#'
#' @param .data A local tibble or dataframe to be rendered in the ReviewR UI
#'
#' @return return a DT with custom options
#' @keywords internal
#' @export 
#' @importFrom DT datatable
#'
reviewr_datatable <- function(.data) {
  DT::datatable(data = .data,
                extensions = list('Scroller' = NULL),
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
  )
}

# UI ----
#' Patient Navigation
#'
#' This module will render the datatable on the 'Patient Search' tab containing all patients in the cohort. The selected patient in the DT is kept in sync with the 'Chart Review' tab.
#' 
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_navigation
#' 
#' @keywords internal
#' @export
#' @import shiny 
#' @importFrom shinycssloaders withSpinner
#' 
all_patient_search_dt <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('all_patient_search_dt')) %>% withSpinner() 
    )
  }

chart_review_navigation <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(inputId = ns('subject_id'),
                   width = '100%',
                   label = 'Jump to Subject ID:',
                   choices = NULL,
                   selected = NULL
                   ),
    fluidRow( 
        actionButton(inputId = ns('prev_subject'), label = '<-- Previous', width = '125px'), 
        actionButton(inputId = ns('next_subject'), label = 'Next -->', width = '125px'),
        style = 'display:inline;margin:auto;width:50%'
        )
    )
}

# Server ---- 
#' @param database_vars Database variables returned from user selected database setup module
#' @param datamodel_vars Datamodel variablese returned from mod_datamodel_setup
#' @param abstract_vars Abstraction variables reeturned from user selected abstraction module
#' @param parent_session the parent environment of this module
#'
#' @rdname mod_navigation
#' 
#' @keywords internal
#' @export
#' @import shiny 
#' @importFrom DT reloadData formatStyle selectRows dataTableProxy
#' @importFrom dplyr rename slice filter select pull
#' @importFrom tibble rowid_to_column
#' @importFrom rlang .data exec
navigation_server <- function(id, database_vars, datamodel_vars, abstract_vars, parent_session) {
  moduleServer(
    id,
    # function(input, output, session, table_map, db_connection, disconnect, prev_sub, next_sub, selected_sub, parent) {
    function(input, output, session) {
      
      ns <- session$ns
      
      # Navigation Vars ----
      navigation_vars <- reactiveValues(
        dt_proxy = NULL,
        all_patients = NULL
        )
      
      observeEvent(datamodel_vars$table_functions, ignoreNULL = F, {
        # req(datamodel_vars$table_functions)
        # browser()
        if(is.null(datamodel_vars$table_functions) == FALSE) {
          navigation_vars$dt_proxy <- DT::dataTableProxy(outputId = ns('all_patient_search_dt'), session = parent_session)
          all_patients_args <- list(table_map = datamodel_vars$table_map, 
                                    db_connection = database_vars()$db_con
          )
          navigation_vars$all_patients <- rlang::exec(datamodel_vars$table_functions %>% 
                                                        filter(table_name == 'all_patients') %>% 
                                                        extract2('function_name'),
                                                      !!!all_patients_args
                                                      )
          } else {
            navigation_vars$dt_proxy <- NULL
            navigation_vars$all_patients <- NULL
            }
        })
      
      # Patient Search Data Table ----
        output$all_patient_search_dt <- DT::renderDataTable({
          req(database_vars()$is_connected == 'yes')
          if(is.null(navigation_vars$all_patients)) {
            tibble::tibble(.rows = 0) %>% 
              reviewr_datatable()
          } else {
            # The next time you think about implementing FixedColumns, check the status of this issue first: https://github.com/rstudio/DT/issues/275
            navigation_vars$all_patients %>%
              rename('Subject ID' = .data$ID) %>%
              reviewr_datatable() %>%
              formatStyle('Subject ID',
                          color = '#0000EE',
                          cursor = 'pointer',  # Format the ID column to appear blue and change the mouse to a pointer
                          textAlign = 'left'
                          )
            }
          })
      
    #   #Replace Patient Search Table when table map changes
    #   observeEvent(table_map(), {
    #     req(table_map() )
    #     DT::reloadData(proxy = patient_search_proxy,
    #                    resetPaging = T,
    #                    clearSelection = T)
    #     })
    #   
    #   # Extract patients based on presence of connection info and data model
    #   patient_search_tbl <- eventReactive(table_map(), {
    #     req(db_connection() )
    #     if (table_map()$count_filtered != 0 & table_map()$data_model == 'omop') {
    #       omop_table_all_patients(table_map, db_connection)
    #       } else if(table_map()$count_filtered != 0 & table_map()$data_model == 'mimic3') {
    #         ## MIMIC Patient Search
    #         mimic_table_all_patients(table_map, db_connection)
    #         } else {
    #           return(NULL)
    #           }
    #     })
    #   
    #   ## Render Patient Search Data Table
    #   output$patient_search_dt <- DT::renderDataTable({
    #     req(patient_search_tbl())
    #     # The next time you think about implementing FixedColumns, check the status of this issue first: https://github.com/rstudio/DT/issues/275
    #     patient_search_tbl() %>% 
    #       rename('Subject ID' = .data$ID) %>% 
    #       reviewr_datatable() %>% 
    #       formatStyle('Subject ID', 
    #                   color = '#0000EE', 
    #                   cursor = 'pointer',  # Format the ID column to appear blue and change the mouse to a pointer
    #                   textAlign = 'left'
    #                   )
    #     })
    #   
    #   outputOptions(output, 'patient_search_dt', suspendWhenHidden = F)
    #   
    #   ## Create a DT Proxy to keep DT selection up to date with Patient Nav on Chart Review Tab
    #   patient_search_proxy <- DT::dataTableProxy(outputId = ns('patient_search_dt'), session = parent)
    #   
    #   ## On Previous Subject Button Press, update selected row in DT
    #   observeEvent(prev_sub(), {
    #     req(patient_search_tbl(), input$patient_search_dt_rows_selected )
    #     if(input$patient_search_dt_rows_selected == 1){ ## Special case when at the beginning of the list, cycle to last
    #       DT::selectRows(patient_search_proxy, nrow(patient_search_tbl() ))
    #       } else { DT::selectRows(patient_search_proxy, input$patient_search_dt_rows_selected - 1)}
    #     })
    #   
    #   ## On Next Subject Button Press, updated selected row in DT
    #   observeEvent(next_sub(), {
    #     req(patient_search_tbl(), input$patient_search_dt_rows_selected )
    #     if(input$patient_search_dt_rows_selected == nrow(patient_search_tbl() )){ ## Special case when at the end of the list, cycle to beginning
    #       DT::selectRows(patient_search_proxy, 1)
    #       } else { DT::selectRows(patient_search_proxy, input$patient_search_dt_rows_selected + 1) }
    #     })
    #   
    #   ## When a choice is made from the patient nav dropdown, update the selected row in DT
    #   observeEvent(selected_sub(), {
    #     req(patient_search_tbl(), selected_sub(), input$patient_search_dt_rows_selected )
    #     sub_row_id <- patient_search_tbl() %>%
    #       rowid_to_column(var = 'row_id') %>%
    #       filter(.data$ID == selected_sub() ) %>%
    #       select(.data$row_id) %>%
    #       slice(1)
    #     DT::selectRows(patient_search_proxy, sub_row_id)
    #     })
    #   
    #   ## Extract the selected patient id from the patient data table when clicked and store as a reactive
    #   select_patient_click <- reactive({ input$patient_search_dt_cell_clicked })
    #   selected_patient <- reactive({ 
    #     req(patient_search_tbl(), input$patient_search_dt_rows_selected )
    #     patient_search_tbl() %>% 
    #       slice(input$patient_search_dt_rows_selected) %>% 
    #       pull(.data$ID)
    #     })
    #   
    #   selected_patient_info <- reactive({ 
    #     req(patient_search_tbl(), input$patient_search_dt_rows_selected )
    #     patient_search_tbl() %>% 
    #       slice(input$patient_search_dt_rows_selected)
    #     })
    #   
    #   return(list(
    #     'patient_table' = patient_search_tbl,
    #     'dt_selection_info' = select_patient_click,
    #     'selected_patient' = selected_patient,
    #     'selected_patient_info' = selected_patient_info
    #     ))
      # Return ----
      return(navigation_vars)
      }
    )
  }
