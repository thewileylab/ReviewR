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

chart_review_subject_info <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('subject_info'))
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
#' @importFrom glue glue
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
        all_patients = NULL,
        subject_choices = list('<empty' = ''),
        selected_subject = NULL,
        selected_subject_id = NULL
        )
      
      observeEvent(datamodel_vars$table_functions, ignoreNULL = F, ignoreInit = T, {
        if(is.null(datamodel_vars$table_functions) == TRUE) {
          message('Removing "all patients" table')
          navigation_vars$dt_proxy <- NULL
          navigation_vars$all_patients <- NULL
          navigation_vars$selected_subject <- NULL
          } else {
            message('Retrieving "all patients" table...')
            navigation_vars$dt_proxy <- DT::dataTableProxy(outputId = ns('all_patient_search_dt'), session = parent_session)
            all_patients_args <- list(table_map = datamodel_vars$table_map, 
                                      db_connection = database_vars()$db_con
            )
            navigation_vars$all_patients <- rlang::exec(datamodel_vars$table_functions %>% 
                                                          filter(.data$table_name == 'all_patients') %>% 
                                                          extract2('function_name'),
                                                        !!!all_patients_args
            )
            navigation_vars$all_patients_max_rows <- nrow(navigation_vars$all_patients)
            message('Complete')
            }
        })
      
      # All Patients DT ----
        output$all_patient_search_dt <- DT::renderDataTable({
          if(is.null(navigation_vars$all_patients)) {
            tibble::tibble(.rows = 0) %>% 
              reviewr_datatable()
            } else {
            ## Future Developer: 
            ##  Salutations. If you are ever asked about implementing FixedColumns, 
            ## check the status of this issue first: https://github.com/rstudio/DT/issues/275
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
      
      ## Populate Chart Review dropdown with ID column from 'all_patients' table
      observeEvent(navigation_vars$all_patients, ignoreNULL = FALSE, ignoreInit = T, {
        # browser()
        navigation_vars$subject_choices <- if(is.null(navigation_vars$all_patients) ) {
          list('<empty>' = '')
        } else { 
          navigation_vars$all_patients$ID }
        ### Update Chart Review Dropdown Choices
        updateSelectizeInput(session = session, 
                             inputId = 'subject_id',
                             choices = navigation_vars$subject_choices,
                             server = T
                             )
        })
      
      ## Subject Info
      ### Extract Subject Info Based on which row is clicked in the DT
      observeEvent(input$all_patient_search_dt_rows_selected, {
        navigation_vars$selected_subject <- navigation_vars$all_patients %>%
          slice(input$all_patient_search_dt_rows_selected) 
        navigation_vars$selected_subject_id <- navigation_vars$selected_subject %>%
          pull(.data$ID)
        ### Update Chart Review Dropdown Selection
        updateSelectizeInput(session = session, 
                             inputId = 'subject_id',
                             choices = navigation_vars$subject_choices,
                             selected = navigation_vars$selected_subject_id,
                             server = T)
        })
      
      ## Subject Info Header
      ### Create Subject Info Header UI
      output$subject_info <- renderUI({
        # req(navigation_vars$selected_subject)
        if(is.null(navigation_vars$selected_subject)) {
          tagList(
            h3('Connect to Patient Database to Begin')
          )
        } else {
          tagList(
            div(h3(glue::glue('Subject ID: {navigation_vars$selected_subject_id}'), 
                   style='padding:0px;'
                   ), 
                style='display:inline-block;vertical-align:middle'
                ),
            # tags$div(status_indicator(), style='display:inline-block;vertical-align:middle'),
            renderTable(navigation_vars$selected_subject %>% 
                          mutate_all(as.character) %>% 
                          select(-.data$ID), 
                        width = '100%', align = 'l', digits = 0)
            )
          }
        })
      
      # Navigation Inputs ----
      ## When DT loads, select the first row
      observeEvent(input$all_patient_search_dt_rows_all, {
        req(input$all_patient_search_dt_rows_all)
        DT::selectRows(navigation_vars$dt_proxy, 1)
        })
      
      ## When ID column is clicked, head to the Chart Review Tab
      observeEvent(input$all_patient_search_dt_cell_clicked, {
        ### Only redirect if clicked cell contains value and is in column 0 (Subject ID)
        req(input$all_patient_search_dt_cell_clicked$value, input$all_patient_search_dt_cell_clicked$col == 0)
        updateTabItems(parent_session, 'main_tabs', selected = 'chart_review')
        })
      
      ## Previous Subject
      ### On Previous Subject Button Press, update selected row in DT
      observeEvent(input$prev_subject, {
        req(input$all_patient_search_dt_rows_selected)
        ## Special case when at the beginning of the list, cycle to last
        if(input$all_patient_search_dt_rows_selected == 1){ 
          DT::selectRows(navigation_vars$dt_proxy, navigation_vars$all_patients_max_rows )
          ## Else, select the previous row in the table
          } else { DT::selectRows(navigation_vars$dt_proxy, input$all_patient_search_dt_rows_selected - 1) }
        })
      
      ## Next Subject
      ### On Next Subject Button Press, updated selected row in DT
      observeEvent(input$next_subject, {
        req(input$all_patient_search_dt_rows_selected)
        ## Special case when at the end of the list, cycle to beginning
        if(input$all_patient_search_dt_rows_selected == navigation_vars$all_patients_max_rows){ 
          DT::selectRows(navigation_vars$dt_proxy, 1 )
          ## Else, select the next row in the table
        } else { DT::selectRows(navigation_vars$dt_proxy, input$all_patient_search_dt_rows_selected + 1) }
      })
      
      ## When a choice is made from the patient nav dropdown, update the selected row in DT
        observeEvent(input$subject_id, {
          req(input$all_patient_search_dt_rows_selected, input$subject_id != '', navigation_vars$all_patients)
          dropdown_row <- navigation_vars$all_patients %>%
            rowid_to_column(var = 'row_id') %>%
            filter(.data$ID == input$subject_id ) %>%
            pull(row_id)
          if(dropdown_row != input$all_patient_search_dt_rows_selected) {
            DT::selectRows(navigation_vars$dt_proxy, dropdown_row) }
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
    
      # Return ----
      return(navigation_vars)
      }
    )
  }
