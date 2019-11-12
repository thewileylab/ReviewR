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

redcap_connected_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('rc_connected_ui'))
  )
}

redcap_connected_logic <- function(input, output, session, connect, rc_project_info) {
  ns <- session$ns
  # observeEvent(connect(), {
  #   browser()
  # })
  
  rc_connected_message <- eventReactive(connect(), {
    if (nrow(rc_project_info() ) > 0) {
      HTML(paste('<H3>Success!!</H3>', 
                 'You have connected to the', rc_project_info()$project_title, 'Project in REDCap.',
                 '<br>',
                 '<br>',
                 '<H4>Project Information:</H4>',
                 '<b>Project ID:</b>', rc_project_info()$project_id,
                 '<br>',
                 '<b>Created:</b>', rc_project_info()$creation_time,
                 '<br>',
                 '<b>Production Status:</b>', rc_project_info()$in_production,
                 '<br><br>',
                 '<b>Please configure a REDCap Instrument in the box below before continuing.</b>',
                 '<br><br>'))
    } else {HTML(paste('No REDCap Projects were found. Please connect to a different REDCap Project.',
                       '<br><br>'))
    }
    
  })
  
  output$rc_connected_ui <- renderUI({
    req(rc_connected_message() )
    tagList(
      rc_connected_message(),
      actionButton(inputId = ns('rc_disconnect'),label = 'Disconnect')
    )
  })
  
  rc_disconnect <- reactive({ input$rc_disconnect })
  
  return(list(
    'rc_disconnect' = rc_disconnect
  ))
}

## REDCap Configuration ----
redcap_instrument_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('rc_select')),
    uiOutput(ns('rc_configure')),
    uiOutput(ns('rc_reviewer')),
    uiOutput(ns('current_reviewer')),
    uiOutput(ns('rc_configure_btn'))
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
    req(instruments(), instrument_selection(), rc_connection() )
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

redcap_instrument_config_reviewer_logic <- function(input, output, session, rc_instrument, rc_identifier, rc_connection) {
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
  
  rc_previous_reviewers <- reactive({
    req(rc_reviewer(), rc_connection() )
    if(rc_reviewer() =='(Not Applicable)') {
      return(NULL) 
      } else {
        reviewer_field <- rc_instrument() %>% 
          filter(field_label == rc_reviewer()) %>% 
          pull(field_name) 
        redcapAPI::exportRecords(rc_connection() ) %>% 
          select(reviewer_field) %>% 
          distinct() %>% 
          remove_missing(na.rm = T)
        }
  })
  
  rc_current_reviewer_question <- reactive({
    if(is.null(rc_previous_reviewers()) == T ) {
      return(NULL)
    } else {
      selectizeInput(inputId = ns('rc_current_reviewer'),
                     label = 'Select your name from the list, or enter a new one:',
                     choices = rc_previous_reviewers(), 
                     options = list(create = TRUE))
    }
  })
  
  output$current_reviewer <- renderUI( rc_current_reviewer_question() )
  
  output$rc_configure_btn <- renderUI({ actionButton(inputId = ns('rc_configure'), label = 'Configure REDCap Instrument') })
  
  rc_selected_reviewer <- reactive({ input$rc_current_reviewer })
  rc_configure_btn_press <- reactive({ input$rc_configure})
  
  return(list(
    'rc_identifier' = rc_identifier,
    'rc_reviewer' = rc_reviewer, 
    'rc_selected_reviewer' = rc_selected_reviewer,
    'rc_configure_btn_press' = rc_configure_btn_press
  ))
}

rc_instrument_configured_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('rc_configured_ui'))
  )
}

rc_instrument_configured_logic <- function(input, output, session, rc_instrument_info, selected_instrument) {
  ns <- session$ns
  
  rc_configured_message <- eventReactive(rc_instrument_info$rc_configure_btn_press(), {
      HTML(paste('<H3>Success!!</H3>', 
                 'You have configured the REDCap Instrument.',
                 '<br>',
                 '<br>',
                 '<H4>Instrment Information:</H4>',
                 '<b>Instrument Name:</b>', selected_instrument(),
                 '<br>',
                 '<b>Identifier Field:</b>', rc_instrument_info$rc_identifier(),
                 '<br>',
                 '<b>Reviewer Field:</b>', rc_instrument_info$rc_reviewer(),
                 '<br>',
                 '<b>Reviewer Name:</b>', rc_instrument_info$rc_selected_reviewer(),
                 '<br><br>',
                 '<b>You may now proceed to record review. Have fun and watch out for bugs!</b>',
                 '<br><br>'))
  })
  output$rc_configured_ui <- renderUI({
    req(rc_configured_message() )
    tagList(
      rc_configured_message(),
      actionButton(inputId = ns('rc_reconfig'),label = 'Reconfigure Instrument')
    )
  })
  rc_reconfig <- reactive({input$rc_reconfig})
  return(list(
    'rc_reconfig' = rc_reconfig
  ))
}

## REDCap Instrument ----
redcap_instrument_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('redcap_instrument')) %>% withSpinner(type = 5, color = '#e83a2f') ## 'danger red'
  )
}

redcap_instrumment_logic <- function(input, output, session, rc_connection, instruments, instrument_selection, rc_instrument, rc_identifier, rc_reviewer, subject_id, reviewr_upload_btn, reviewr_connect_btn) {
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
    if(redcapAPI::exportNextRecordName(rc_connection()) == 1) { ## Special case, when the REDCap Instrument has no previous data
      redcapAPI::exportFieldNames(rc_connection() ) %>% 
        select(export_field_name, choice_value) %>% 
        mutate(choice_value = map(.x = choice_value, ~ NA)) %>% 
        pivot_wider(names_from = export_field_name, values_from = choice_value) %>% 
        flatten_dfr() %>% 
        remove_missing(na.rm = TRUE)
      } else { ## Export existing Records, filtering to the subject and reviewer in context
      redcapAPI::exportRecords(rcon = rc_connection(), factors = F, labels = F ) %>% 
        as_tibble() %>% 
        mutate_all(as.character) %>% 
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        filter(!!as.name(rc_identifier_field() ) == subject_id() )
    }
  })
  current_subject <- reactive({
    req(previous_data() )
    
    if(nrow(previous_data() ) > 0 ){
      previous_data() %>% 
      # Turn wide data from RedCAP to long, collapsing checkbox type quesitions along the way
      pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>% 
      separate(checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
      mutate(checkbox_value = map2_chr(.x = checkbox_value, .y = value_present, ~ case_when(.y == 0 ~ '',
                                                                                            TRUE ~ .x)
                                       )
             ) %>%   
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
  
  # observeEvent(reviewr_upload_btn(), {
  #   browser()
  # })

  ## Create a Shiny tagList for each question type present in the instrument
  rc_instrument_ui <- reactive({
    req(rc_instrument(), current_subject() )
    rc_instrument() %>% 
      left_join(current_subject() ) %>% # add current subject info, if present, to the mix
      mutate( ## mutate shiny tags/inputs
        shiny_header = map(section_header, h3),
        shiny_field_label = case_when(is.na(required_field) ~ field_label,
                                      TRUE ~ paste(field_label,"<br/><font color='#FC0020'>* must provide value</font>")
                                      ),
        shiny_input = pmap(list(reviewr_type = reviewr_redcap_widget_function, 
                                field_name = ns(shiny_inputID), 
                                field_label = shiny_field_label, 
                                required = required_field,
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

upload_redcap_logic <- function(input, output, session, rc_con, rc_recordID, rc_instrument, instrumentData, previousData, currentSubject, rc_upload_btn_press, abstraction_complete, abstraction_complete_val, all_instruments, instrument_selection) {
  ns <- session$ns
  
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
  
  rc_complete <- reactive({
    req(rc_upload_btn_press() )
    selected_instrument_name <- all_instruments() %>% 
      filter(instrument_label == instrument_selection() ) %>% 
      extract2(1,1)
    instrument_complete_field <- paste0(selected_instrument_name,'_complete')
    if(is.null(abstraction_complete_val() ) | abstraction_complete() == F) {
      tibble(!!instrument_complete_field := 0)
    } else {
      tibble(!!instrument_complete_field := abstraction_complete_val() )
    }
  })
  # observeEvent(rc_upload_btn_press(), {
  #   browser()
  # })
  # 
  rc_uploadData <- reactive({
    req(rc_instrument(), instrumentData(), rc_id() )
    rc_recordID_field <- rc_recordID() %>% extract2(1)
    rc_instrument() %>% 
      mutate(shiny_inputID = shiny_inputID) %>% ## Namespace
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
      remove_missing(na.rm = TRUE) %>% 
      pivot_wider(names_from = col_names, values_from = values) %>% 
      bind_cols(rc_id(), rc_complete() ) %>% 
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
  ## Process the instrumnet slightly
  exploded_instrument  <- reactive({
    rc_instrument() %>%
    select(shiny_inputID, field_type, select_choices_or_calculations) %>% 
    mutate(select_choices_or_calculations = case_when(field_type == 'yesno' ~ '1, Yes | 0, No',
                                                      field_type == 'truefalse' ~ '1, True | 0, False',
                                                      TRUE ~ select_choices_or_calculations
                                                      )
           ) %>% 
    separate_rows(select_choices_or_calculations, sep = '\\|') %>% 
    separate(select_choices_or_calculations, into = c('values','choice_labels'), sep = ',') %>% 
    mutate_all(str_trim) %>% 
    mutate_all(replace_na, replace = '') %>% 
    mutate(inputID = shiny_inputID) %>%
    select(inputID, everything(), -shiny_inputID)
  })
  
  ## Process current data, matching values with choice labels for display
  current_data <- reactive({
    instrumentData() %>% 
    modify_depth(.depth = 2, as.character) %>% 
    unnest(cols = values) %>% 
    mutate(current_values = values)
    })
  
  new_data_w_labels <- reactive({
    exploded_instrument() %>% 
    full_join(current_data() ) %>% 
    mutate(inputID = str_replace(string = inputID, pattern = regex(pattern = '_reviewr_.*'),replacement = '')) %>% 
    mutate_all(replace_na, replace = '') %>% 
    filter(current_values != '') %>% 
    mutate(current_values_2 = case_when(choice_labels == '' ~ current_values,
                                        TRUE ~ choice_labels
                                        )
           ) %>% 
    select(inputID, 'current_values' = current_values_2) %>% 
    group_by(inputID) %>% 
    mutate(current_values = paste(current_values, collapse = '<br><br>')) %>% 
    ungroup() %>% 
    distinct(inputID, .keep_all = T)
  })
  
  ## Process old (existing) data, matching values with choice labels for display
  old_data <- reactive({
    currentSubject() %>% 
    modify_depth(.depth = 2, as.character) %>% 
    unnest(cols = default_value) %>% 
    mutate(inputID = field_name) %>% 
    select(inputID, previous_values = default_value,-field_name) %>% 
    mutate(values = previous_values) %>% 
    select(inputID, values, previous_values)
  })
  
  old_data_w_labels <- reactive({
    exploded_instrument() %>% 
    mutate(inputID = str_replace(string = inputID, pattern = regex(pattern = '_reviewr_.*'),replacement = '')) %>% 
    full_join(old_data() ) %>% 
    mutate_all(replace_na, replace = '') %>% 
    filter(previous_values != '') %>% 
    mutate(previous_values_2 = case_when(choice_labels == '' ~ previous_values,
                                         TRUE ~ choice_labels
                                         )
           ) %>% 
    select(inputID, 'previous_values' = previous_values_2) %>% 
    group_by(inputID) %>% 
    mutate(previous_values = paste(previous_values, collapse = '<br><br>')) %>% 
    ungroup() %>% 
    distinct(inputID, .keep_all = T)
  })
  
  ## Modal Representation
  modal_data <- eventReactive(rc_upload_btn_press(), {
  new_data_w_labels() %>% 
  full_join(old_data_w_labels() ) %>% 
    mutate_all(replace_na, replace = '') %>%
    mutate(diff = case_when(current_values != previous_values ~ 1,
                            TRUE ~ 0
    )
    ) %>% 
    filter(diff == 1) %>% 
    left_join(rc_instrument() %>% 
                select(field_name, field_label, select_choices_or_calculations), 
              by =c('inputID' = 'field_name')) %>% 
    select('Question' = field_label, 'Previous Values' = previous_values, 'New Values' = current_values) %>% 
    remove_missing(vars = 'Question', na.rm = TRUE) 
  })

  output$confirm_modal_dt <- renderDataTable(
    modal_data() %>% 
      datatable(
        extensions = list('Scroller' = NULL),
        options = list(scrollX = TRUE,
                       deferRender = TRUE,
                       scrollY = '450px',
                       scroller = TRUE,
                       sDom  = '<"top">lrt<"bottom">ip'
                       ),
        rownames = F, 
        escape = F,
        class = 'cell-border strip hover'
        )
    )
  # If data already exists in REDCap and there are differences, show modal which highlights differences
  observeEvent(rc_upload_btn_press(), {
    rc_recordID_field <- rc_recordID() %>% flatten_chr()
    if(continue_val() != '' & nrow(modal_data()) > 0) {
      showModal(
        modalDialog(
          title = 'Warning: Overwriting existing REDCap data.',
          dataTableOutput(ns('confirm_modal_dt')),
          actionButton(inputId = ns('continue'), label = 'Continue overwriting existing REDCap values'),
          actionButton(inputId = ns('go_back'), label = 'Go Back'), 
          easyClose = FALSE,
          fade = TRUE,
          footer = NULL,
          size = 'l'
          )
        )
      message('Warning: Potentially overwriting existing REDCap data.')
      #browser()
      # else, send data to REDCap
      } else {
        message('Uploading data to REDCap.')
        # browser()
        upload_status <- redcapAPI::importRecords(rcon = rc_con(), data = rc_uploadData(), overwriteBehavior = 'overwrite', returnContent = 'ids' )
        upload_message <- paste('REDCap', rc_recordID_field, upload_status %>% tibble::enframe(name = NULL) %>% separate(col = value,into = c('id','value'), sep = '\n') %>% select(value) %>% extract2(1), 'Uploaded Successfully.')
        # Display message after sending data to REDCap
        showNotification(ui = upload_message,
                         duration = 5,
                         closeButton = T,
                         type = 'default')
      }
  })
  
  # If confirming changes, send new data to REDCap
  observeEvent(input$continue, {
    removeModal()
    rc_recordID_field <- rc_recordID() %>% flatten_chr()
    overwrite_status <- redcapAPI::importRecords(rcon = rc_con(), data = rc_uploadData(), overwriteBehavior = 'overwrite', returnContent = 'ids')
    overwrite_message <- paste('REDCap', rc_recordID_field, overwrite_status %>% tibble::enframe(name = NULL) %>% separate(col = value,into = c('id','value'), sep = '\n') %>% select(value) %>% extract2(1), 'Modified Successfully.')

    # Display message after sending data to REDCap
    showNotification(ui = overwrite_message,
                     duration = 5,
                     closeButton = T,
                     type = 'warning')
  })
  # If dismissing new changes, remove modal and go back to ReviewR
  observeEvent(input$go_back, {
    removeModal()
  })
  
}
