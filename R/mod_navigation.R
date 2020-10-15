# Helper Functions ---- 
#' ReviewR Datatable
#'
#' @param .data A local tibble or dataframe to be rendered in the ReviewR UI
#' @param dom Define the table control elements to appear on the page and in what order
#' @param column_filter Where to display individual column filters. Valid entries are: 'top','bottom','none'
#' @param search_term A string or regular expression used as a filter for patient data
#' 
#' @return return a DT with custom options
#' @keywords internal
#' @export 
#' @importFrom DT datatable
#'
reviewr_datatable <- function(.data, dom = 'ftip', column_filter = 'top', search_term = '') {
  DT::datatable(data = .data,
                extensions = list('Scroller' = NULL),
                options = list(dom = dom,
                               scrollX = TRUE,
                               deferRender = TRUE,
                               scrollY = '550px',
                               scroller = TRUE,
                               searchHighlight = TRUE, 
                               search = list(regex = TRUE, 
                                             caseInsensitive = TRUE,
                                             search = search_term)
                               ),
                rownames = F, 
                selection = 'single',
                escape = F,
                filter = column_filter,
                class = 'cell-border strip hover'
                )
  }

# UI ----
#' Patient Navigation
#'
#' This module will render the datatable on the 'Patient Search' tab containing all patients in the cohort. 
#' The selected patient in the DT is kept in sync with the 'Chart Review' tab.
#' 
#' @param id The namespace id for the UI output
#'
#' @rdname mod_navigation
#' 
#' @keywords internal
#' @export
#' @import shiny 
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets pickerInput
#' 

navigation_message <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('nav_message'))
    )
  }

all_patient_search_dt <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('all_patient_search_dt')) %>% withSpinner() ,
    uiOutput(ns('datamodel_message'))
    )
  }

chart_review_subject_info <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('subject_info')) %>% withSpinner(type = 7, proxy.height = 100, size = .5)
  )
}

chart_review_navigation <- function(id) {
  ns <- NS(id)
  tagList(
      div(id = ns('jump_no_abstraction_div'),
          selectizeInput(inputId = ns('subject_id'),
                         width = '100%',
                         label = 'Jump to Subject ID:',
                         choices = NULL,
                         selected = NULL,
                         options = list(create = FALSE,
                                        placeholder = '<empty>'
                                        )
                         )
          ),
      shinyjs::hidden(
        div(id = ns('jump_abstraction_div'),
            pickerInput(inputId = ns('subject_id_2'),
                        label = 'Jump to Subject ID:',
                        choices = NULL,
                        selected = NULL,
                        choicesOpt = list(content = NULL),
                        options = list(size = 5,
                                       title = '<empty>'
                                       )
                        )
            )
        ),
    fluidRow(
      actionButton(inputId = ns('prev_subject'), label = '<-- Previous', width = '120px'), 
      actionButton(inputId = ns('next_subject'), label = 'Next -->', width = '120px'),
      style = 'display:flex;justify-content:center;flex-wrap:wrap;'
      ),
    uiOutput(ns('review_progress'))
    )
}

# Server ---- 
#' @param database_vars Database variables returned from user selected database setup module
#' @param datamodel_vars Datamodel variables returned from mod_datamodel_setup
#' @param abstract_vars Abstraction variables returned from user selected abstraction module
#' @param parent_session the parent environment of this module
#'
#' @rdname mod_navigation
#' 
#' @keywords internal
#' @export
#' @import shiny 
#' @importFrom DT reloadData formatStyle selectRows dataTableProxy
#' @importFrom glue glue
#' @importFrom dplyr last_col left_join mutate_at pull rename slice filter select 
#' @importFrom glue glue
#' @importFrom magrittr %>% extract2
#' @importFrom tibble rowid_to_column tibble
#' @importFrom tidyr replace_na
#' @importFrom rlang .data exec is_empty
#' @importFrom shinyjs disable hide enable show
#' @importFrom shinyWidgets progressBar updatePickerInput
#' @importFrom utils tail
mod_navigation_server <- function(id, database_vars, datamodel_vars, abstract_vars, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Navigation Vars ----
      navigation_vars <- reactiveValues(
        dt_proxy = NULL,
        all_patients = NULL,
        all_patients_max_rows = NULL, 
        row_ids = NULL,
        subject_ids = NULL,
        selected_row = 0,
        message = HTML('Please complete Setup to connect to a patient database.'),
        review_progress = 0
        )
      
      # Navigation Message ----
      output$nav_message <- renderUI({ navigation_vars$message })
      
      # Subject Vars ----
      subject_vars <- reactiveValues(
        selected_subject_id = NULL,
        selected_subject_status = NULL
        )
      
      observeEvent(datamodel_vars$table_functions, ignoreNULL = F, ignoreInit = T, {
        ## When disconnecting, reset module to initial state
        if(is.null(datamodel_vars$table_functions) == TRUE) {
          message('Resetting patient navigation.')
          ## Clear Navigation Vars
          navigation_vars$dt_proxy <- NULL
          navigation_vars$all_patients <- NULL
          navigation_vars$all_patients_max_rows = NULL
          navigation_vars$row_ids = NULL
          navigation_vars$subject_ids = NULL
          navigation_vars$selected_row = 0
          navigation_vars$message = HTML('Please complete Setup to connect to a patient database.')
          # Update Chart Review Dropdown Choices
          updateSelectizeInput(session = session, 
                               inputId = 'subject_id',
                               choices = navigation_vars$subject_ids,
                               server = T
                               )
          updatePickerInput(session = session,
                            inputId = 'subject_id_2',
                            choices = navigation_vars$subject_ids
                            )
          ## Clear Subject Info Vars
          subject_vars$selected_subject_info <- NULL
          subject_vars$selected_subject_id = NULL
          ## When connecting, collect necessary vars
          } else {
            message('Retrieving "all patients" table...')
            ## Create DT Proxy
            navigation_vars$dt_proxy <- DT::dataTableProxy(outputId = ns('all_patient_search_dt'), session = parent_session)
            ## Retrieve "all patients" table
            all_patients_args <- list(table_map = datamodel_vars$table_map, 
                                      db_connection = database_vars()$db_con
                                      )
            navigation_vars$all_patients <- rlang::exec(datamodel_vars$all_patients_table %>% 
                                                          pull('function_name'),
                                                        !!!all_patients_args
                                                        )
            ## Determine "all patients" table stats
            navigation_vars$all_patients_max_rows <- nrow(navigation_vars$all_patients)
            navigation_vars$row_ids <- navigation_vars$all_patients %>% 
              rowid_to_column(var = 'row_id') %>% 
              pull(.data$row_id)
            navigation_vars$subject_ids <- setNames(navigation_vars$row_ids, navigation_vars$all_patients$ID)
            ## Update Chart Review Dropdown Choices
            updateSelectizeInput(session = session, 
                                 inputId = 'subject_id',
                                 choices = navigation_vars$subject_ids,
                                 server = T
                                 )
            navigation_vars$message <- HTML('To select a patient, please click the desired Subject ID from the table below:') 
            message('Complete')
            }
        })
      
      # Create All Patients DT ----
        output$all_patient_search_dt <- DT::renderDataTable({
          if(is.null(navigation_vars$all_patients)) {
            tibble::tibble(.rows = 0) %>% 
              reviewr_datatable()
            } else if (abstract_vars()$is_configured == 'yes') {
              ## Future Developer: 
              ## Salutations. If you are ever asked about implementing FixedColumns, 
              ## check the status of this issue first: https://github.com/rstudio/DT/issues/275
              navigation_vars$all_patients %>%
                left_join(abstract_vars()$all_review_status) %>% 
                ## The last two columns will contain 'review status'
                dplyr::mutate_at(vars(dplyr::last_col(1), dplyr::last_col()), tidyr::replace_na, '<em>Review Not Started</em>') %>% 
                rename('Subject ID' = .data$ID) %>%
                reviewr_datatable() %>%
                  # Format the ID column to appear blue and change the mouse to a pointer
                  formatStyle('Subject ID',
                              color = '#0000EE',
                              cursor = 'pointer',
                              textAlign = 'left'
                              )
                } else {
                ## Future Developer: 
                ## Salutations. If you are ever asked about implementing FixedColumns, 
                ## check the status of this issue first: https://github.com/rstudio/DT/issues/275
                navigation_vars$all_patients %>%
                  rename('Subject ID' = .data$ID) %>%
                  reviewr_datatable() %>%
                  # Format the ID column to appear blue and change the mouse to a pointer
                  formatStyle('Subject ID',
                              color = '#0000EE',
                              cursor = 'pointer',
                              textAlign = 'left'
                              )
                  }
          })
      # outputOptions(output, 'all_patient_search_dt', suspendWhenHidden = F)
      
      ## When DT loads, select the first row
      observeEvent(input$all_patient_search_dt_rows_all, {
        req(input$all_patient_search_dt_rows_all)
        if(navigation_vars$selected_row == 0){
          navigation_vars$selected_row <- 1
          }
        DT::selectRows(navigation_vars$dt_proxy, navigation_vars$selected_row)
        })
      
      # Datamodel ----
      ## Render Datamodel Output
      output$datamodel_message <- renderText({
        req(database_vars()$is_connected == 'yes')
        datamodel_vars$message
      })
      
      # Subject Info ----
      ### Determine Abstraction Status
      observeEvent(abstract_vars()$previous_selected_instrument_complete_val, ignoreInit = T, {
        if (abstract_vars()$previous_selected_instrument_complete_val %>% rlang::is_empty() == T ) {
          subject_vars$selected_subject_status <- NULL
        } else {
          subject_vars$selected_subject_status <- if(abstract_vars()$previous_selected_instrument_complete_val == 0) { img(src = 'www/status_incomplete.png', style='width: 20px')
            } else if(abstract_vars()$previous_selected_instrument_complete_val == 1) { img(src = 'www/status_unverified.png', style='width: 20px')
              } else if(abstract_vars()$previous_selected_instrument_complete_val == 2) { img(src = 'www/status_complete.png', style='width: 20px') 
                } else {return(NULL)}
          }
        })
      ### Create Subject Info Header UI
      output$subject_info <- renderUI({
        if(is.null(subject_vars$selected_subject_info)) {
          tagList(
            HTML('Please complete Setup to connect to a patient database.')
            )
          } else if (abstract_vars()$is_configured == 'yes') {
            tagList(
              div(h3(glue::glue('Subject ID: {subject_vars$selected_subject_id}'), 
                     style='padding:0px;'
                     ), 
                  style='display:inline-block;vertical-align:middle'
                  ),
              div(subject_vars$selected_subject_status, 
                  style='display:inline-block;vertical-align:middle'
                  ),
              div(renderTable(subject_vars$selected_subject_info %>% 
                                left_join(abstract_vars()$all_review_status) %>% 
                                ## The last two columns will contain 'review status'
                                select(-dplyr::last_col() ) %>% 
                                # dplyr::mutate_at(vars(dplyr::last_col(1), dplyr::last_col()), tidyr::replace_na, '<em>Review Not Started</em>') %>%
                                dplyr::mutate_at(vars(dplyr::last_col()), tidyr::replace_na, '<em>Review Not Started</em>') %>% 
                                mutate_all(as.character) %>% 
                                select(-.data$ID), 
                              width = '100%', align = 'l', digits = 0,
                              sanitize.text.function=identity
                              ),
                  style='height:115px; overflow-y: scroll; scrollbar-width: thin;'
                  )
              )
            } else {
              tagList(
                div(h3(glue::glue('Subject ID: {subject_vars$selected_subject_id}'), 
                       style='padding:0px;'
                       ), 
                    style='display:inline-block;vertical-align:middle'
                    ),
                renderTable(subject_vars$selected_subject_info %>% 
                              mutate_all(as.character) %>% 
                              select(-.data$ID), 
                            width = '100%', align = 'l', digits = 0
                            )
                )
            }
        })
      
      # Monitor DT ----
      observeEvent(input$all_patient_search_dt_rows_selected, {
        req(input$all_patient_search_dt_rows_selected != navigation_vars$selected_row)
        navigation_vars$selected_row <- input$all_patient_search_dt_rows_selected
        })
      ## When ID column is clicked, head to the Chart Review Tab
      observeEvent(input$all_patient_search_dt_cell_clicked, {
        ### Only redirect if clicked cell contains value and is in column 0 (Subject ID)
        req(input$all_patient_search_dt_cell_clicked$value, input$all_patient_search_dt_cell_clicked$col == 0)
        updateTabItems(parent_session, 'main_tabs', selected = 'chart_review')
        })
      
      # Update DT ----
      observeEvent(navigation_vars$selected_row, {
        req(navigation_vars$all_patients)
        ## Update Chart Review Dropdown Selection
        updateSelectizeInput(session = session, 
                             inputId = 'subject_id',
                             choices = navigation_vars$subject_ids,
                             selected = navigation_vars$selected_row,
                             server = T)
        if(abstract_vars()$is_configured == 'yes'){
          updatePickerInput(session = session,
                            inputId = 'subject_id_2',
                            choices = navigation_vars$subject_ids,
                            selected = navigation_vars$selected_row,
                            choicesOpt = list(content = navigation_vars$individual_review_status %>% 
                                                unite(col = 'picker_html', sep = '<br>') %>% 
                                                pull(.data$picker_html)
                                              )
                            )
          }
        ## Update Subject Info
        subject_vars$selected_subject_info <- navigation_vars$all_patients %>%
          slice(navigation_vars$selected_row) 
        subject_vars$selected_subject_id <- subject_vars$selected_subject_info %>%
          pull(.data$ID)
        ## Update DT Selection
        DT::selectRows(navigation_vars$dt_proxy, navigation_vars$selected_row)
        Sys.sleep(2)
        shinyjs::enable('prev_subject')
        shinyjs::enable('next_subject')
        shinyjs::enable('subject_id')
        shinyjs::enable('subject_id_2')
        shinyjs::enable('all_patient_search_dt_rows_selected')
        })
      
      # Navigation Inputs ----
      ## On Previous Subject Button Press, update selected row in DT
      observeEvent(input$prev_subject, {
        shinyjs::disable('prev_subject')
        shinyjs::disable('next_subject')
        shinyjs::disable('subject_id')
        shinyjs::disable('all_patient_search_dt_rows_selected')
        temp <- navigation_vars$selected_row - 1
        if(temp < 1) {
          navigation_vars$selected_row <- navigation_vars$all_patients_max_rows
          } else {
            navigation_vars$selected_row <- temp
            }
        })
      
      ## On Next Subject Button Press, updated selected row in DT
      observeEvent(input$next_subject, {
        shinyjs::disable('prev_subject')
        shinyjs::disable('next_subject')
        shinyjs::disable('subject_id')
        shinyjs::disable('all_patient_search_dt_rows_selected')
        temp <- navigation_vars$selected_row + 1
        if(temp > navigation_vars$all_patients_max_rows) {
          navigation_vars$selected_row <- 1
          } else {
            navigation_vars$selected_row <- temp
            }
        })
      
      ## When a choice is made from the chart review dropdown, update the selected row in DT
        observeEvent(input$subject_id, ignoreInit = T, {
          req(input$subject_id != '')
          if(as.integer(input$subject_id) != navigation_vars$selected_row) {
            shinyjs::disable('prev_subject')
            shinyjs::disable('next_subject')
            shinyjs::disable('subject_id')
            shinyjs::disable('all_patient_search_dt_rows_selected')
            navigation_vars$selected_row <- as.integer(input$subject_id) }
          })
        
        ## When a choice is made from the abstraction chart review dropdown, update the selected row in DT
        observeEvent(input$subject_id_2, ignoreInit = T, {
          req(input$subject_id_2 != '')
          if(as.integer(input$subject_id_2) != navigation_vars$selected_row) {
            shinyjs::disable('prev_subject')
            shinyjs::disable('next_subject')
            shinyjs::disable('subject_id_2')
            shinyjs::disable('all_patient_search_dt_rows_selected')
            navigation_vars$selected_row <- as.integer(input$subject_id_2) }
        })
      
      # Review Status ----
        observeEvent(abstract_vars()$is_configured, {
          if(abstract_vars()$is_configured == 'yes') {
            shinyjs::hide(id = 'jump_no_abstraction_div')
            shinyjs::show(id = 'jump_abstraction_div')
            } else if (abstract_vars()$is_configured == 'no') {
              shinyjs::hide(id = 'jump_abstraction_div')
              shinyjs::show(id = 'jump_no_abstraction_div')
            }
          })
        observeEvent(abstract_vars()$all_review_status, {
          # browser()
          navigation_vars$individual_review_status  <- navigation_vars$all_patients %>% 
            left_join(abstract_vars()$all_review_status) %>% 
            select(c(.data$ID, 'Review Status' = dplyr::last_col())) %>% 
            dplyr::mutate_at(vars(dplyr::last_col()), tidyr::replace_na, '<em>Review Not Started</em>')
          updatePickerInput(session = session,
                            inputId = 'subject_id_2',
                            choices = navigation_vars$subject_ids,
                            selected = navigation_vars$selected_row,
                            choicesOpt = list(content = navigation_vars$individual_review_status %>% 
                                                unite(col = 'picker_html', sep = '<br>') %>% 
                                                pull(.data$picker_html)
                                              )
          )
          
          navigation_vars$review_progress <- navigation_vars$individual_review_status %>% 
            filter(.data$`Review Status` != '<em>Review Not Started</em>') %>% 
            nrow()
          })
        
        ## Review Progress Bar
        review_progress <- reactive({
          req(abstract_vars()$is_configured == 'yes')
          shinyWidgets::progressBar(id = ns("review_progress"), 
                                    title = "Review Progress:",
                                    value = navigation_vars$review_progress, 
                                    total = nrow(navigation_vars$all_patients), 
                                    status = "info", 
                                    display_pct = TRUE, 
                                    striped = TRUE )
          })
        output$review_progress <- renderUI({ review_progress() })
        
      # Return ----
      return(subject_vars)
      }
    )
  }
