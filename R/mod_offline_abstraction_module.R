#' Offline Abstraction Module
#'
#' This module contains all of the Offline setup and instrumnet upload/download components.
#' 
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
#' @import shiny
#' 
offline_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('setup_ui')),
    uiOutput(ns('offline_config_btn'))
  )
}

#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
offline_setup_select_logic <-function(input, output, session) {
  ns <- session$ns
  
  choices <- reactive({
    c('Shiny Contest Demo' = 'demo',
      'New Session' = 'new')
  })
  
  setup_ui <- reactive({
    tagList(
      selectInput(inputId = ns('offline_session'),
                  label = 'Start a new offline session, or connect to an existing session:',
                  choices = choices()
                  )
    )
  })
  output$setup_ui <- renderUI({ setup_ui() })
  offline_selection <- reactive({ input$offline_session })
  return(list(
    'offline_selection' = offline_selection
  ))
}

#' @param input internal
#' @param output internal
#' @param session internal
#' @param selection Existing or new session?
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
offline_setup_logic <- function(input, output, session, selection) {
  ns <- session$ns
  
  config_button <- reactive({
    req(selection() )
   if (selection()  == 'new') {
     tagList(
      fileInput(inputId = ns('new'),
                label = 'Upload Abstraction Instrument CSV:',
                buttonLabel = 'Browse',
                multiple = F,
                accept =  c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
      actionButton(inputId = ns('connect'),
                   label = 'Connect',
                   icon = icon('notes-medical')) 
     )
   } else  {
     actionButton(inputId = ns('connect'),
                  label = 'Connect',
                  icon = icon('notes-medical')) 
     }
  })
  
  connect <- reactive({ input$connect })
  new <- reactive({ input$new })

  output$offline_config_btn <- renderUI({ config_button() })
  return(list(
    'existing_session_btn' = connect,
    'existing_session' = selection,
    'new_session' = new
    )
  )
}

#' @param id The namespace id for the UI output
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export

offline_abs_connected_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('offline_connected_ui'))
  )
}

#' @param input internal
#' @param output internal
#' @param session internal
#' @param abstraction_vars Abstraction setup variables 
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
offline_connected_logic <- function(input, output, session, abstraction_vars) {
  ns <- session$ns
  # 
  # observeEvent(abstraction_vars$offline_press(), {
  #   browser()
  # })
   offline_info <- reactive({
     if(abstraction_vars$offline_existing_session() == 'demo') {
       'Shiny Contest Demo Abstraction'
     } else {
       abstraction_vars$offline_new_session()$name
     }
   })

   offline_connected_message <- eventReactive(abstraction_vars$offline_press(), {
     req(offline_info() )
     HTML(paste('<H3>Success!!</H3>',
                'You have connected to the', offline_info(), ' Instrument.',
                '<br>',
                '<br>',
                '<b>Please configure the Offline Instrument in the box below before continuing.</b>',
                '<br><br>'))
   })
  
  output$offline_connected_ui <- renderUI({
    req(offline_connected_message() )
    tagList(
      offline_connected_message(),
      actionButton(inputId = ns('offline_disconnect'),label = 'Disconnect')
    )
  })
  
  offline_disconnect <- reactive({ input$offline_disconnect })
  
  return(list(
    'offline_instrument' = offline_info,
    'offline_disconnect' = offline_disconnect
  ))
}


#' @param id The namespace id for the UI output
#'
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
#' @importFrom readr read_csv

offline_abs_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('offline_config_ui')), 
    uiOutput(ns('offline_reviewer')), 
    uiOutput(ns('offline_current_reviewer')),
    uiOutput(ns('offline_configure_btn'))
  )
}

offline_abs_config_logic <- function(input, output, session, abstraction_vars) {
  ns <- session$ns
  
  instrument <- reactive({
    if(abstraction_vars$offline_existing_session() == 'demo') {
      read_csv(app_sys('extdata/shiny_contest_instrument.csv'))
    } else {
      read_csv(abstraction_vars$offline_new_session()$datapath)
    }
  })
  
  offline_instrument_patient_id <- reactive({
    req(instrument() )
    tagList(
      selectInput(inputId = ns('offline_identifier_field'),
                  label = 'Which variable contains your record identifier (e.g., MRN, subject ID)?',
                  choices = instrument() %>%
                    filter(.data$field_type == 'text') %>% 
                    select(.data$field_label) %>%
                    deframe()
      )
    )
  })
  
  offline_identifier <- reactive({ input$offline_identifier_field })
  
  output$offline_config_ui <- renderUI({offline_instrument_patient_id() })
  
  return(list(
    'offline_instrument' = instrument,
    'offline_identifier' = offline_identifier
  ))
}

offline_abs_config_reviewer_logic <- function(input, output, session, offline_instrument, offline_identifier) {
  ns <- session$ns
  
  offline_reviewer_id <- reactive({
    req(offline_instrument(), offline_identifier() )
    selectInput(inputId = ns('offline_reviewer_field'), 
                label = 'Which variable contains your reviewer identifier?',
                choices = append('(Not Applicable)', 
                                 offline_instrument() %>%
                                   filter(.data$field_type == 'text' & .data$field_label != offline_identifier() ) %>% 
                                   select(.data$field_label) %>%
                                   deframe()
                )
    )
  })
  offline_reviewer <- reactive({ input$offline_reviewer_field })
  
  output$offline_reviewer <- renderUI({offline_reviewer_id() })
  
  offline_previous_reviewers <- reactive({
    req(offline_instrument(), offline_reviewer() )
    list('Bill' = 'bill',
         'Ted' = 'ted')
  })
  
  offline_current_reviewer_question <- reactive({
    req(offline_reviewer())
    if(offline_reviewer() == '(Not Applicable)' ) {
      return(NULL)
    } else {
      selectizeInput(inputId = ns('offline_current_reviewer'),
                     label = 'Select your name from the list, or enter a new one:',
                     choices = append('',
                                      offline_previous_reviewers()
                     ), 
                     options = list(create = TRUE,
                                    placeholder = 'New Reviewer'))
    }
  })
  
  output$offline_current_reviewer <- renderUI( offline_current_reviewer_question() )
  output$offline_configure_btn <- renderUI({ actionButton(inputId = ns('offline_configure'), label = 'Configure Offline Instrument') })
  offline_selected_reviewer <- reactive({ input$offline_current_reviewer })
  offline_configure_btn_press <- reactive({ input$offline_configure})
  
  return(list(
    'offline_identifier' = offline_identifier,
    'offline_reviewer' = offline_reviewer,
    'offline_selected_reviewer' = offline_selected_reviewer,
    'offline_configure_btn_press' = offline_configure_btn_press
  ))
}

offline_instrument_configured_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('offline_configured_ui'))
    )
}

offline_instrument_configured_ui_logic <- function(input, output, session, offline_config_vars, offline_project_vars) {
  ns <- session$ns
  
  offline_configured_message <- eventReactive(offline_config_vars$offline_configure_btn_press(), {
    HTML(paste('<H3>Success!!</H3>', 
               'You have configured the Offline Instrument.',
               '<br>',
               '<br>',
               '<H4>Instrument Information:</H4>',
               '<b>Instrument Name:</b>', offline_project_vars$offline_instrument(),
               '<br>',
               '<b>Identifier Field:</b>', offline_config_vars$offline_identifier(),
               '<br>',
               '<b>Reviewer Field:</b>', offline_config_vars$offline_reviewer(),
               '<br>',
               '<b>Reviewer Name:</b>', offline_config_vars$offline_selected_reviewer(),
               '<br><br>',
               '<b>You may now proceed to record review. Have fun and watch out for bugs!</b>',
               '<br><br>'))
  })
  
  output$offline_configured_ui <- renderUI({
    req(offline_configured_message() )
    tagList(
      offline_configured_message(),
      actionButton(inputId = ns('offline_reconfig'),label = 'Reconfigure Instrument')
    )
  })
  offline_reconfig <- reactive({input$offline_reconfig})
  return(list(
    'offline_reconfig' = offline_reconfig
  ))
}


## Offline Instrument ----
#' @rdname mod_offline_abs_module
#' 
#' @keywords internal
#' @export
offline_instrument_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('offline_instrument')) %>% withSpinner(type = 5, color = '#e83a2f') ## 'danger red'
  )
}

#' @param rc_reviewer The REDCap field that holds reviewer names
#' @param rc_selected_reviewer The selected reviewer from the list of previous reviewers (if present)
#' @param subject_id The currently selected patient identifier
#' @param reviewr_upload_btn A button press of the REDCap upload button
#' @param reviewr_connect_btn A button press of the REDCap Connect button
#'
#' @rdname mod_redcap_module
#' 
#' @keywords internal
#' @export
#' @importFrom magrittr extract2
#' @importFrom redcapAPI exportFieldNames
#' @importFrom purrr flatten_dfr map map2 pmap
#' @importFrom tidyr as_tibble replace_na pivot_wider pivot_longer contains separate everything
#' @importFrom dplyr mutate_all case_when
#' @importFrom rlang is_empty :=
#' @importFrom tibble add_row
offline_instrument_logic <- function(input, output, session, db_connection, instruments, instrument_selection, offline_instrument, offline_identifier, offline_reviewer, rc_selected_reviewer, subject_id, reviewr_upload_btn, offline_connect_btn) {
  ns <- session$ns
  observeEvent(offline_connect_btn(), {
    browser()
  })
  ## On redcap connection or subsequent upload, determine if there is any default data that needs to be displayed
  offline_identifier_field <- reactive({
    req(offline_instrument(), offline_identifier() )
    offline_instrument() %>% 
      select(.data$field_name, .data$field_label) %>% 
      filter(.data$field_label == offline_identifier() ) %>% 
      extract2(1)
  })
  offline_reviewer_field <- reactive({
    req(offline_instrument(), offline_reviewer() )
    offline_instrument() %>% 
      select(.data$field_name, .data$field_label) %>% 
      filter(.data$field_label == offline_reviewer() ) %>% 
      extract2(1)
  })
  # selected_instrument <- reactive({
  #   req(instruments(), instrument_selection() )
  #   instruments() %>% 
  #     filter(.data$instrument_label == instrument_selection() ) %>% 
  #     extract2(1)
  # })
  ## Init Offline Storage
  checkbox_instrument_columns <- instrument %>% 
    left_join(ReviewR::redcap_widget_map, by = c("field_type" = "redcap_field_type", "text_validation_type_or_show_slider_number" = "redcap_field_val")) %>% 
    select(field_name,select_choices_or_calculations,reviewr_redcap_widget_function) %>%
    filter(reviewr_redcap_widget_function == 'reviewr_checkbox') %>% 
    mutate(choices = stringr::str_split(select_choices_or_calculations,'\\|')) %>% 
    unnest(cols = choices) %>% 
    separate(choices, into = c('Values', 'Names'), sep = ',') %>% 
    dplyr::mutate_at(vars('Values':'Names'), str_trim) %>% 
    mutate(all_columns = paste0(field_name,'___',Values))
  other_instrument_columns <- instrument %>% 
    left_join(ReviewR::redcap_widget_map, by = c("field_type" = "redcap_field_type", "text_validation_type_or_show_slider_number" = "redcap_field_val")) %>% 
    select(field_name,select_choices_or_calculations,reviewr_redcap_widget_function) %>%
    filter(reviewr_redcap_widget_function != 'reviewr_checkbox') %>%
    mutate(all_columns = field_name)
  all_instrument_columns <- dplyr::bind_rows(checkbox_instrument_columns, other_instrument_columns)
  complete_what <- glue::glue('{offline_instrument()}_complete')
  all_instrument_columns %<>% add_row(all_columns = complete_what)
  offline_storage <- tibble(!!!all_instrument_columns$all_columns,.rows = 0)
  colnames(offline_storage) <- all_instrument_columns$all_columns
  offline_abs_storage <- offline_storage %>% 
    tibble::add_column('timestamp_utc') %>% 
    rename('timestamp_utc' = '"timestamp_utc"') %>% 
    dbWriteTable(conn = db_connection, name = 'offline_abs_storage')
    # write_csv(file.path('offline_redcap_data',config$redcap_offline_session,config$redcap_offline_filename))
  
  ## Determine if there is any previous data to show. If a reviewer field is specified, make sure to filter to data belonging to that reviewer.
  previous_data <- reactive({
    req(rc_connection(), rc_identifier_field(), selected_instrument(), subject_id() )
    if(nrow(tbl(db_connection, 'offline_abs_storage')) == 0) { ## Special case, when the REDCap Instrument has no previous data
      redcapAPI::exportFieldNames(rc_connection() ) %>% 
        select(.data$export_field_name, .data$choice_value) %>% 
        mutate(choice_value = map(.x = .data$choice_value, ~ NA)) %>% 
        pivot_wider(names_from = .data$export_field_name, values_from = .data$choice_value) %>% 
        flatten_dfr() %>% 
        remove_missing(na.rm = TRUE)
    } else if (redcapAPI::exportNextRecordName(rc_connection()) != 1 & is_empty(rc_reviewer_field() ) == T ) { ## Export existing Records, filtering to the subject and reviewer in context
      redcapAPI::exportRecords(rcon = rc_connection(), factors = F, labels = F ) %>% 
        as_tibble() %>% 
        mutate_all(as.character) %>% 
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        filter(!!as.name(rc_identifier_field() ) == subject_id() )
    } else {
      redcapAPI::exportRecords(rcon = rc_connection(), factors = F, labels = F ) %>% 
        as_tibble() %>% 
        mutate_all(as.character) %>% 
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        filter(!!as.name(rc_identifier_field() ) == subject_id() & !!as.name(rc_reviewer_field()) == rc_selected_reviewer() )
    }
  })
  current_subject <- reactive({
    req(previous_data() )
    
    if(nrow(previous_data() ) > 0 ){
      previous_data() %>% 
        # Turn wide data from RedCAP to long, collapsing checkbox type quesitions along the way
        pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>% 
        separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
        mutate(checkbox_value = map2_chr(.x = .data$checkbox_value, .y = .data$value_present, ~ case_when(.y == 0 ~ '',
                                                                                                          TRUE ~ .x)
        )
        ) %>%   
        select(-.data$value_present) %>% # remove value presence variable
        pivot_wider(names_from = .data$checkbox_questions, values_from = .data$checkbox_value, values_fn = list(checkbox_value = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
        pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'default_value', values_ptypes = list(default_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
    } else if(nrow(previous_data() ) == 0 & is_empty(rc_reviewer_field()) ) {
      previous_data() %>% 
        add_row(!!rc_identifier_field() := subject_id() ) %>% # Add default data, without reviewer info, if present
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>% 
        separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
        select(-.data$checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
        pivot_wider(names_from = .data$checkbox_questions, values_from = .data$value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
        pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'default_value', values_ptypes = list(default_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
    } else {
      previous_data() %>% 
        add_row(!!rc_identifier_field() := subject_id(), !!rc_reviewer_field() := rc_selected_reviewer() ) %>% # Add default data, with reviewer info, if present
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>% 
        separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
        select(-.data$checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
        pivot_wider(names_from = .data$checkbox_questions, values_from = .data$value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
        pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'default_value', values_ptypes = list(default_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
    }
  })
  
  # observeEvent(reviewr_upload_btn(), {
  #   browser()
  # })
  
  ## Create a Shiny tagList for each question type present in the instrument
  rc_instrument_ui <- reactive({
    req(rc_instrument(), current_subject() )
    rc_instrument() %>% 
      left_join(current_subject() ) %>% # add current subject info, if present, to the mix
      mutate( ## mutate shiny tags/inputs
        shiny_header = map(.data$section_header, h3),
        shiny_field_label = case_when(is.na(.data$required_field) ~ .data$field_label,
                                      TRUE ~ paste(.data$field_label,"<br/><font color='#FC0020'>* must provide value</font>")
        ),
        shiny_input = pmap(list(reviewr_type = .data$reviewr_redcap_widget_function, 
                                field_name = ns(.data$shiny_inputID), 
                                field_label = .data$shiny_field_label, 
                                required = .data$required_field,
                                choices = .data$select_choices_or_calculations,
                                current_subject_data = .data$default_value
        ), 
        render_redcap
        ),
        shiny_note = map(.data$field_note, tags$sub),
        shiny_taglist = pmap(list(.data$shiny_header,
                                  .data$shiny_input,
                                  .data$shiny_note
        ),
        tagList
        )
      )
  })
  
  ## Create Instrument Output
  output$redcap_instrument <- renderUI ({ 
    req(rc_instrument_ui() )
    div(
      id = ns('redcap_form'),
      rc_instrument_ui()$shiny_taglist
    )
  })
  
  # Collect Instrument data
  redcap_module_inputs <- reactive({reactiveValuesToList(input)})
  instrumentData <- reactive({
    tibble(inputID = names(redcap_module_inputs() ),
           values = unname(redcap_module_inputs() )
    )
  })
  
  # #Pause, verify collected data
  # observeEvent(reviewr_upload_btn(), {
  #   browser()
  # })
  return(list(
    'instrument_data' = instrumentData,
    'previous_data' = previous_data,
    'current_subject' = current_subject
  )) # Send to the Upload module
}