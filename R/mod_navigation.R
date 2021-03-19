# Helper Functions ---- 
#' ReviewR DataTable
#' 
#' @description 
#' This is a wrapper function around [DT::datatable] which applies common extensions,
#' options and default values used throughout the ReviewR application.
#'
#' @param .data A local tibble or data frame to be rendered in the ReviewR UI
#' @param dom Define the table control elements to appear on the page and in what order. See:
#' \url{https://datatables.net/reference/option/dom}
#' @param column_filter Where to display individual column filters. Valid entries are: 'top','bottom','none'
#' @param search_term A string or regular expression used as a filter for patient data
#' 
#' @return A [DT::datatable]
#' @keywords internal
#'
#' @importFrom DT datatable
#'
reviewr_datatable <- function(.data, dom = 'ftip', column_filter = 'top', search_term = '') {
  DT::datatable(data = .data,
                extensions = list('Scroller' = NULL),
                options = list(dom = dom,
                               scrollX = TRUE,
                               deferRender = TRUE,
                               scrollY = '600px',
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

# Module Documentation ----
#' Patient Navigation Module
#' 
#' @description 
#' This module will render the "all patients" dataTable (DT) located on the 'Patient Search' 
#' tab of ReviewR and will display demographic information about subjects in the 
#' connected database. The subject id of the selections made on this tab are extracted and 
#' passed to other ReviewR modules. As selections are made using the DT or the navigation 
#' buttons on the 'Chart Review' tab, the selected patient in the DT is kept in sync by 
#' this module. 
#' 
#' Additionally, demographic and (optionally) abstraction status information about the 
#' selected patient are extracted and placed into a header on the 'Chart Review' tab.
#' 
#' This module consists of the following components:
#' 
#' ## Module UI functions
#' 
#' \itemize{
#' \item{`navigation_message`}: A uiOutput to display a placeholder message when no 
#' database is connected.
#' \item{`all_patient_search_dt`}: A uiOutput containing the "all patients" dataTable 
#' with patient demographic information from the connected database.
#' \item{`chart_review_subject_info`}: A uiOutput containing the selected subject's 
#' demographic information to display on the 'Chart Review' tab.
#' \item{`chart_review_navigation`}: A uiOutput containing the "Jump to Subject ID' 
#' dropdown and previous and next buttons used to navigate through patient data on the
#' 'Chart Review' tab.
#' }
#' ## Module Server function
#' \itemize{
#' \item{navigation_server}: Provides all of the logic assosciated with displaying 
#' patient demographic information, selecting a patient to review, and navigating 
#' through the connected patient database. Returns user selected patient information
#' for use by other modules.
#' }
#' 
#' ## Keyboard Shortcuts
#' 
#' This module also provides keyboard shortcuts to assist with navigating through 
#' patient data. The "meta" key refers to "ctrl" on Windows and "Cmd" on Mac.
#' 
#' * Next Patient: “alt + meta + >” 
#' * Prev Patient: “alt + meta + <” 
#' 
#' @param id The Module namespace
#' @name mod_navigation
#' 
#' @return 
#' *navigation_message*:
#' \item{tagList}{A uiOutput to display a placeholder message when no 
#' database is connected.}
#' *all_patient_search_dt*:
#' \item{tagList}{A uiOutput containing the "all patients" dataTable 
#' with patient demographic information from the connected database.}
#' *chart_review_subject_info*:
#' \item{tagList}{A uiOutput containing the selected subject's 
#' demographic information to display on the 'Chart Review' tab.}
#' *chart_review_navigation*:
#' \item{tagList}{A uiOutput containing the "Jump to Subject ID' 
#' dropdown and previous and next buttons used to navigate through patient data on the
#' 'Chart Review' tab.}
#' *navigation_server*:
#' \item{reactiveValues}{
#' \itemize{
#' \item{*selected_subject_id*}: A character representing the currently selected subject in
#' the currently connected database.
#' \item{*selected_subject_info*}: A [dplyr::tibble] containing demographic information about
#' the selected subject.
#' \item{*selected_subject_status*}: A character containing the abstraction status of the 
#' selected subject, when an abstraction module is configured and abstraction data is
#' present.
#' }}
NULL
#> NULL

# UI ----
#' @rdname mod_navigation
#' 
#' @keywords internal
#'
#' @import shiny 
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets pickerInput pickerOptions

navigation_message <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('nav_message'))
    )
  }

#' @rdname mod_navigation
#' 
#' @keywords internal
all_patient_search_dt <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('all_patient_search_dt')) %>% withSpinner() ,
    uiOutput(ns('data_model_message'))
    )
  }

#' @rdname mod_navigation
#' 
#' @keywords internal
chart_review_subject_info <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('subject_info')) %>% withSpinner(type = 7, proxy.height = 100, size = .5)
    )
  }

#' @rdname mod_navigation
#' 
#' @keywords internal
chart_review_navigation <- function(id) {
  ns <- NS(id)
  
  ## Create Inputs from Keyboard Left and Right Arrows!
  arrowed <- paste0(
    "$(document).on('keydown', function(event){",
    "  var key = event.which;",
    "  if(event.metaKey && event.altKey && key === 188){",
    "    Shiny.setInputValue('",id,"-arrowLeft', true, {priority: 'event'});",
    "  } else if(event.metaKey && event.altKey && key === 190){",
    "    Shiny.setInputValue('",id,"-arrowRight', true, {priority: 'event'});",
    "  }",
    "});"
    )
  
  tagList(
    tags$head(tags$script(HTML(arrowed))),
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
                      options = pickerOptions(title = '<empty>',
                                              virtualScroll = TRUE
                                              )
                      )
          )
      ),
    fluidRow(
      actionButton(inputId = ns('prev_subject'), label = '<-- Previous', width = '120px'), 
      actionButton(inputId = ns('next_subject'), label = 'Next -->', width = '120px'),
      style = 'display:flex;justify-content:center;flex-wrap:wrap;'
      )
    )
}

# Server ---- 
#' @rdname mod_navigation
#' 
#' @keywords internal
#'
#' @param database_vars A reactiveValues object as returned by \link[ReviewR]{mod_database_setup}.
#' @param data_model_vars A reactiveValues object as returned by \link[ReviewR]{mod_data_model_detection}.
#' @param abstract_vars A reactiveValues object as returned by \link[ReviewR]{mod_abstraction_setup}.
#' @param parent_session The session information from the parent environment of this module.
#'
#' @import shiny 
#' @importFrom DT reloadData formatStyle selectRows dataTableProxy
#' @importFrom glue glue
#' @importFrom dplyr last_col left_join mutate_at pull rename slice filter row_number select tibble
#' @importFrom glue glue
#' @importFrom magrittr %>% extract2
#' @importFrom tidyr replace_na
#' @importFrom rlang .data exec is_empty
#' @importFrom shinyjs disable hide enable show
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom utils tail
navigation_server <- function(id, database_vars, data_model_vars, abstract_vars, parent_session) {
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
        message = HTML('Please complete Setup to connect to a patient database.')
        )
      
      # Navigation Message ----
      output$nav_message <- renderUI({ navigation_vars$message })
      
      # Subject Vars ----
      subject_vars <- reactiveValues(
        selected_subject_id = NULL,
        selected_subject_status = NULL
        )
      
      observeEvent(data_model_vars$table_functions, ignoreNULL = F, ignoreInit = T, {
        ## When disconnecting, reset module to initial state
        if(is.null(data_model_vars$table_functions) == TRUE) {
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
            all_patients_args <- list(table_map = data_model_vars$table_map, 
                                      db_connection = database_vars()$db_con
                                      )
            navigation_vars$all_patients <- rlang::exec(data_model_vars$all_patients_table %>% 
                                                          pull('function_name'),
                                                        !!!all_patients_args
                                                        )
            ## Determine "all patients" table stats
            navigation_vars$all_patients_max_rows <- nrow(navigation_vars$all_patients)
            navigation_vars$row_ids <- navigation_vars$all_patients %>% 
              mutate(row_id = row_number()) %>% 
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
            dplyr::tibble(.rows = 0) %>% 
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

      ## When DT loads, select the first row
      observeEvent(input$all_patient_search_dt_rows_all, {
        req(input$all_patient_search_dt_rows_all)
        if(navigation_vars$selected_row == 0){
          navigation_vars$selected_row <- 1
          }
        DT::selectRows(navigation_vars$dt_proxy, navigation_vars$selected_row)
        })
      
      # Data model ----
      ## Render Datam Model Output
      output$data_model_message <- renderText({
        req(database_vars()$is_connected == 'yes')
        data_model_vars$message
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
      observeEvent(input$prev_subject,  {
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
      
      ## On Keyboard Arrow Left Button Press, update selected row in DT
      observeEvent(input$arrowLeft, {
        req(navigation_vars$all_patients)
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
      
      ## On Keyboard Arrow Right Button Press, update selected row in DT
      observeEvent(input$arrowRight, {
        req(navigation_vars$all_patients)
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
        observeEvent(c(navigation_vars$all_patients, abstract_vars()$all_review_status), {
          req(navigation_vars$all_patients, abstract_vars()$all_review_status)
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
          })
        
      # Return ----
      return(subject_vars)
      }
    )
  }
