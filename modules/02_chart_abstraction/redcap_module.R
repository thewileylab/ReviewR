## REDCap Connection  ----
redcap_connect_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('redcap_setup')),
    uiOutput(ns('redcap_connect'))
    )
}

redcap_connect_logic <- function(input, output, session) { 
  ns <- session$ns
  
  redcap_setup <- reactive({
      tagList(
        textInput(inputId = ns('redcap_url'),label = 'REDCap URL:',value = 'https://'),
        passwordInput(inputId = ns('redcap_token'),label = 'REDCap API Token:')
      )
        })
  
  output$redcap_setup <- renderUI({ redcap_setup() })
  
  rc_url <- reactive({input$redcap_url})
  rc_token <- reactive({input$redcap_token})

  return(list(
    'rc_url' = rc_url,
    'rc_token' = rc_token
  ))
}

redcap_initialize_logic <- function(input, output, session, rc_url, rc_token) {
  library(tidyverse)
  library(redcapAPI)
  
  ns <- session$ns
  rc_connect <- reactive({
    req(rc_url(),rc_token())
    if(rc_url() == '' | rc_token() == '') {
      return(NULL)
    } else {
    actionButton(inputId = ns('rc_connect'),label = "Connect to REDCap",icon = icon('notes-medical'))
      }
  })
  
  rc_con <- reactive({
    req(rc_url(), rc_token())
    redcapAPI::redcapConnection(url = rc_url(), token = rc_token() )
  })
  
  output$redcap_connect <- renderUI({ rc_connect() })
  
  rc_connect_press <- reactive({ input$rc_connect })
  
  return(list(
    'rc_con' = rc_con,
    'rc_connect_press' = rc_connect_press
  ))
  
}

## REDCap Configuration ----
redcap_instrument_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('rc_select')),
    uiOutput(ns('rc_configure')),
    uiOutput(ns('rc_reviewer'))
  )
}

redcap_instrument_select_logic <- function(input, output, session, rc_connect_press, rc_connection) {
  ns <- session$ns
  
  # List the available instruments
  instruments <- reactive({
    req(rc_connect_press(), rc_connection() )
    redcapAPI::exportInstruments(rc_connection() )
  })
  
  redcap_instrument_select <- reactive({
    tagList(
      selectInput(inputId = ns('rc_instrument'),
                  label = 'Available Instruments:',
                  choices = instruments() %>% 
                    select(instrument_label) %>% 
                    deframe()
                  )
      )
  })
  instrument_selection <- reactive({ input$rc_instrument })
  
  output$rc_select <- renderUI({ 
    tagList(
      redcap_instrument_select()
      )
    })
  
  return(list(
    'rc_instruments' = instruments,
    'rc_instrument_selection' = instrument_selection
  ))
  
}

redcap_instrument_config_logic <- function(input, output, session, rc_connection, instruments, instrument_selection, redcap_widget_map, rc_upload) {
  ns <- session$ns
  
  ## Source REDCap functions
  source('lib/render_redcap.R', keep.source = F)
  
  ## Store the REDCap Instrument as a reactive variable. Process a bit to assist in rendering the instrument later.
  instrument <- reactive({
    req(instruments(), instrument_selection() )
    ## Create an instrument filter, if multiple instruments are present
    instrument_filter <- instruments() %>% 
      filter(instrument_label == instrument_selection() ) %>% 
      select(instrument_name) %>% 
      pluck(1)
    ## Extract and process the REDCap Instrument
    redcapAPI::exportMetaData(rcon = rc_connection()) %>%
      slice(-1) %>%   # We drop the first row, as it most likely is the auto-increment field used in REDCap
      filter(str_to_lower(form_name) == instrument_filter ) %>% # Select the instrument based on the user selection
      rownames_to_column() %>% 
      filter(!field_type %in% c('slider','calc','descriptive')) %>% 
      # If some information is not defined within REDCap, it will convert those to logical types by default.  We are
      # assuming that they will be all character values, so we need to perform explicit casting to continue with that
      # assumption.
      mutate_if(is.logical, as.character) %>% 
      left_join(redcap_widget_map, 
                by = c('field_type' = 'redcap_field_type', 'text_validation_type_or_show_slider_number' = 'redcap_field_val')
                ) %>% 
      unite(col = 'shiny_inputID', field_name, reviewr_redcap_widget_function, sep = '_', remove = F) %>% 
      mutate(section_header = coalesce(section_header, ''),
             field_note = coalesce(field_note, '')
             )
    })
  
  ## Store the record identifier field
  rc_record_id <- reactive({
    # record identifier will always be the first row of the first instrument in the project
    req(rc_connection() )
    redcapAPI::exportMetaData(rcon = rc_connection()) %>%
      select(field_name) %>% 
      slice(1)
  })
  
  ## Gather project information for display WIP
  rc_project_info <- reactive({
    req(rc_connection() )
    redcapAPI::exportProjectInformation(rc_connection())
    })
  
  ## Create a select input for potential patient identifier columns
  redcap_instrument_patient_id <- reactive({
    req(instrument() )
    tagList(
      selectInput(inputId = ns('rc_identifier_field'),
                  label = 'Which variable contains your record identifier (e.g., MRN, subject ID)?',
                  choices = instrument() %>%
                    filter(field_type == 'text') %>% 
                    select(field_label) %>%
                    deframe()
                  )
      )
  })
  rc_identifier <- reactive({ input$rc_identifier_field })
  
  output$rc_configure <- renderUI({redcap_instrument_patient_id() })
  
  return(list(
    'rc_instrument' = instrument,
    'rc_record_id' = rc_record_id,
    'rc_project_info' = rc_project_info,
    'rc_identifier' = rc_identifier ## Pass this variable to the next module
  ))

}

redcap_instrument_config_reviewer_logic <- function(input, output, session, rc_instrument, rc_identifier) {
  ns <- session$ns
  
  ## Create a select input for potential reviewer identifier coluimns. Remove patient identifier column and append 'Not applicable'
  rc_reviewer_id <- reactive({
    req(rc_instrument(), rc_identifier() )
    selectInput(inputId = ns('rc_reviewer_field'), 
              label = 'Which variable contains your reviewer identifier?',
              choices = append('(Not Applicable)', 
                               rc_instrument() %>%
                                 filter(field_type == 'text' & field_label != rc_identifier() ) %>% 
                                 select(field_label) %>%
                                 deframe()
                               )
              )
  })
  rc_reviewer <- reactive({ input$rc_reviewer_field })
  
  output$rc_reviewer <- renderUI({rc_reviewer_id() })
  
  return(list(
    'rc_identifier' = rc_identifier,
    'rc_reviewer' = rc_reviewer
  ))
}

## REDCap Instrument ----
redcap_instrument_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('redcap_instrument')) %>% withSpinner(type = 5, color = '#e83a2f') ## 'danger red'
  )
}

redcap_instrumment_logic <- function(input, output, session, rc_connection, instruments, instrument_selection, rc_instrument, rc_identifier, rc_reviewer, reviewr_inputs, subject_id, reviewr_upload_btn, reviewr_connect_btn) {
  ns <- session$ns
  
  ## On redcap connection or subsequent upload, determine if there is any default data that needs to be displayed
  rc_identifier_field <- reactive({
    req(rc_instrument(), rc_identifier() )
    rc_instrument() %>% 
      select(field_name, field_label) %>% 
      filter(field_label == rc_identifier() ) %>% 
      extract2(1)
    })
  rc_reviewer_field <- reactive({
    req(rc_instrument(), rc_reviewer() )
    rc_instrument() %>% 
      select(field_name, field_label) %>% 
      filter(field_label == rc_reviewer() ) %>% 
      extract2(1)
  })
  selected_instrument <- reactive({
    req(instruments(), instrument_selection() )
    instruments() %>% 
      filter(instrument_label == instrument_selection() ) %>% 
      extract2(1)
  })
  
  ## Determine if there is any previous data to show. If a reviewer field is specified, make sure to filter to data belonging to that reviewer.
  previous_data <- reactive({
    req(rc_connection(), rc_identifier_field(), selected_instrument(), subject_id() )
    redcapAPI::exportRecords(rcon = rc_connection(), factors = F, labels = F ) %>% 
      as_tibble() %>% 
      mutate_all(as.character) %>% 
      mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
      filter(!!as.name(rc_identifier_field() ) == subject_id() )
  })
  current_subject <- reactive({
    req(previous_data() )
    
    if(nrow(previous_data() ) > 0 ){
      previous_data() %>% 
      # Turn wide data from RedCAP to long, collapsing checkbox type quesitions along the way
      pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>% 
      filter(value_present == 1) %>% # Remove checkbox questions with no box checked
      separate(checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
      select(-value_present) %>% # remove value presence variable
      pivot_wider(names_from = checkbox_questions, values_from = checkbox_value, values_fn = list(checkbox_value = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
      pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'default_value', values_ptypes = list(default_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
    } else if(nrow(previous_data() ) == 0 & is_empty(rc_reviewer_field()) ) {
      previous_data() %>% 
        add_row(!!rc_identifier_field() := subject_id() ) %>% # Add default data, without reviewer info, if present
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>% 
        separate(checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
        select(-checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
        pivot_wider(names_from = checkbox_questions, values_from = value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
        pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'default_value', values_ptypes = list(default_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
    } else {
      previous_data() %>% 
        add_row(!!rc_identifier_field() := subject_id(), !!rc_reviewer_field() := rc_reviewer() ) %>% # Add default data, with reviewer info, if present
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>% 
        separate(checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
        select(-checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
        pivot_wider(names_from = checkbox_questions, values_from = value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
        pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'default_value', values_ptypes = list(default_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
        }
    })
  
  ## Create a Shiny tagList for each question type present in the instrument
  rc_instrument_ui <- reactive({
    req(rc_instrument(), current_subject() )
    rc_instrument() %>% 
      left_join(current_subject() ) %>% # add current subject info, if present, to the mix
      mutate( ## mutate shiny tags/inputs
        shiny_header = map(section_header, h3),
        shiny_field_label = case_when(is.na(required_field) ~ field_label,
                                      TRUE ~ paste(field_label,'*')),
        shiny_input = pmap(list(reviewr_type = reviewr_redcap_widget_function, 
                                field_name = ns(shiny_inputID), 
                                field_label = shiny_field_label, 
                                choices = select_choices_or_calculations,
                                current_subject_data = default_value
                                ), 
                           render_redcap
                           ),
        shiny_note = map(field_note, tags$sub),
        shiny_taglist = pmap(list(shiny_header,
                                  shiny_input,
                                  shiny_note
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
  
  ## Collect Instrument data
  instrumentData <- reactive({
    tibble(inputID = names(reviewr_inputs()), values = unname(reviewr_inputs())) %>% 
      mutate(class = str_detect(string = inputID, pattern = regex(pattern = '_reviewr((?!_reset).)*$',ignore_case = T))) %>% ## Find instrument inputs -- not resets
      filter(class == T) %>% 
      select(-class) 
  })
  
  # ##Pause, verify collected data
  # observeEvent(reviewr_upload_btn(), {
  #   browser()
  # })
  return(list(
    'instrument_data' = instrumentData,
    'previous_data' = previous_data,
    'current_subject' = current_subject
    )) # Send to the Upload module
}

## REDCap Data Upload ----
upload_redcap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('rc_upload'))
  )
}

upload_redcap_logic <- function(input, output, session, rc_con, rc_recordID, rc_instrument, instrumentData, previousData, currentSubject) {
  ns <- session$ns
  
  rc_upload_btn <- reactive({ actionButton(inputId = ns('upload_rc'),label = "Store Abstraction") })
  rc_upload_btn_press <- reactive({ input$upload_rc })
  
  output$rc_upload <- renderUI({rc_upload_btn() })
  
  ## Process Shiny RedCAP inputs to match expected RedCAP API input
  rc_id <- reactive({
    req(rc_con(), rc_upload_btn_press() ) ## Add rc_upload_btn_press() as a req to force refresh of next record id on submit
    rc_recordID_field <- rc_recordID() %>% flatten_chr()
    if(nrow(previousData()) > 0) {
      previousData() %>% 
        select(rc_recordID_field)
    } else {
      tibble(!!rc_recordID_field := exportNextRecordName(rc_con() ))
      }
    })
  # observeEvent(rc_upload_btn_press(), {
  #   browser()
  # })
  
  rc_uploadData <- reactive({
    req(rc_instrument(), instrumentData(), rc_id() )
    rc_recordID_field <- rc_recordID() %>% extract2(1)
    rc_instrument() %>% 
      mutate(shiny_inputID = ns(shiny_inputID)) %>% ## Namespace
      select(shiny_inputID, field_name, select_choices_or_calculations) %>% ## Include select_choices_or_calculations so that all columns can be sent back to REDCap. This allows for overwriting old data with blank ''
      add_row(field_name = rc_recordID() %>% flatten()) %>% ## Add REDCap record ID field back into the instrument, so it can be joined with any previous data.
      left_join(instrumentData(), by = c('shiny_inputID' = 'inputID')) %>% ## Join the instrument inputs with the selected instrument. This ensures inputs are collected only for the active instrument
      modify_depth(2, as.character) %>% ## the input values are all lists at this moment. Dive into each list (depth = 2) and make sure that the values within the list are coded as characters
      separate_rows(select_choices_or_calculations,sep = '\\|') %>% ## Expand select_choices_or_calculations
      mutate(select_choices_or_calculations = str_trim(select_choices_or_calculations)) %>% ## Trim
      separate(select_choices_or_calculations, into = c('rc_val','rc_label'), sep = ',') %>% ## Separate
      ## This mutate adds additional column names to hold values for checkbox questions
      mutate(rc_label = str_trim(rc_label), ## Trim
             col_names = pmap(list(x = shiny_inputID, y = field_name, z = rc_val ),  function(x,y,z) case_when(str_detect(string = x, pattern = 'reviewr_checkbox') ~ paste0(y, '___', z), ## Create additional column names for inputs where multiple inputs are allowed
                                                                                                             TRUE ~ y)
                              ),
             values = pmap(list(x = shiny_inputID, y = rc_val, z = values), function(x,y,z) case_when(str_detect(string = x, pattern = 'reviewr_checkbox') & y == z ~ '1',
                                                                                                      str_detect(string = x, pattern = 'reviewr_checkbox') & y != z ~ '',
                                                                                                      TRUE ~ z)
                           ),
             col_names = flatten_chr(col_names)
             ) %>% 
      select(col_names, values) %>% 
      unnest(cols = values, keep_empty = T) %>% ## in the case that all checkbox questions are de-selected, this keeps empty values, but stores them as NA. 
      ## This mutate modifys values to blanks, except for the special case when the record ID is NA. We would like to trop this value, if NA.
      mutate(values = map2_chr(.x = col_names, .y = values, ~ case_when(str_detect(string = .x,pattern = !!rc_recordID_field) & is.na(.y) ~ .y,
                                                                        is.na(.y) ~ '',
                                                                        TRUE ~ .y)
                               )
             ) %>%  
      arrange(desc(values)) %>% 
      distinct(col_names,.keep_all = T) %>% 
      remove_missing() %>% 
      pivot_wider(names_from = col_names, values_from = values) %>% 
      bind_cols(rc_id() ) %>% 
      select(!!rc_recordID_field, everything() ) %>% ## RedCAP API likes the record identifier in the first column
      flatten_dfr()
  })
  
  ## Verify data before upload. Compare current entries with previous entries. Isolate and display differences
  # Detrmine if the data exists in REDCap by extracting the REDCap record identifier. If blank ('') we assume new record.
  continue_val <- reactive({
    req(currentSubject() )
    currentSubject() %>% 
      filter(field_name == rc_recordID() %>% flatten_chr()) %>% 
      extract2(1,2)
  })
  # Create a DT to display in modal, with Question, Previous Value, and New Value
  modal_data <- eventReactive(rc_upload_btn_press(), {
    instrumentData() %>% 
      mutate(inputID = str_replace(string = inputID, pattern = regex(pattern = '_reviewr_.*'),replacement = '')) %>% 
      left_join(currentSubject() %>% 
                  mutate(field_name = ns(field_name)), 
                by = c('inputID' = 'field_name')) %>% 
      mutate_all(replace_na, replace = '') %>% 
      modify_depth(.depth = 2, as.character) %>% 
      mutate_all(as.character) %>% 
      mutate(diff = case_when(values == default_value ~ 0,
                              TRUE ~ 1)
             ) %>%
      filter(diff == 1) %>% 
      left_join(rc_instrument() %>% 
                  select(field_name, field_label) %>% 
                  mutate(field_name = ns(field_name)), 
                by =c('inputID' = 'field_name')) %>% 
      select('Question' = field_label, 'Previous Value' = default_value, 'New Value' = values) %>% 
      filter(Question != '')
  })
  output$confirm_modal_dt <- renderDataTable(modal_data() %>% 
                                      datatable(
                                        extensions = list('Scroller' = NULL),
                                        options = list(scrollX = TRUE,
                                                       deferRender = TRUE,
                                                       scroller = TRUE,
                                                       sDom  = '<"top">lrt<"bottom">ip'
                                                       ),
                                        rownames = F, 
                                        escape = F,
                                        class = 'cell-border strip hover'
                                        ))
  # If data already exists in REDCap and there are differences, show modal which highlights differences
  observeEvent(rc_upload_btn_press(), {
    if(continue_val() != '' & nrow(modal_data()) > 0) {
      showModal(
        modalDialog(
          title = 'Warning: Overwriting previous REDCap data. Continue?',
          dataTableOutput(ns('confirm_modal_dt')),
          actionButton(inputId = ns('continue'), label = 'Continue'),
          actionButton(inputId = ns('go_back'), label = 'Go Back'), 
          easyClose = FALSE,
          fade = TRUE,
          footer = NULL,
          size = 'l'
          )
        )
      message('Warning: Potentially overwriting existing REDCap data. Continue?')
      #browser()
      # else, send data to REDCap
      } else {
        message('Uploading data to REDCap.')
        #browser()
        redcapAPI::importRecords(rcon = rc_con(), data = rc_uploadData(), overwriteBehavior = 'overwrite', returnContent = 'ids' )
      }
  })
  # If confirming changes, send new data to REDCap
  observeEvent(input$continue, {
    removeModal()
    redcapAPI::importRecords(rcon = rc_con(), data = rc_uploadData(), overwriteBehavior = 'overwrite', returnContent = 'ids')
  })
  # If dismissing new changes, remove modal and go back to ReviewR
  observeEvent(input$go_back, {
    removeModal()
  })
  
  return(list(
    'rc_upload_btn_press' = rc_upload_btn_press,
    'rc_uploadData' = rc_uploadData
  ))
  
}


