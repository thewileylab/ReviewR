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
    req(instruments(), instrument_selection() )
    instrument_filter <- instruments() %>% 
      filter(instrument_label == instrument_selection() ) %>% 
      select(instrument_name) %>% 
      pluck(1)
    redcapAPI::exportMetaData(rcon = rc_connection()) %>%
      filter(str_to_lower(form_name) == instrument_filter ) %>% 
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
    uiOutput(ns('redcap_instrument'))
  )
}

redcap_instrumment_logic <- function(input, output, session, rc_connection, rc_instrument, rc_identifier, rc_reviewer, reviewr_inputs, subject_id, reviewr_upload_btn, reviewr_connect_btn) {
  ## On redcap connection or subsequent upload, determine if there is any default data that needs to be displayed
  current_subject <- reactive({
    req(reviewr_connect_btn(), reviewr_upload_btn() )
    redcapAPI::exportRecords(rcon = rc_connection() ) %>% 
      filter(rc_identifier() == subject_id() )
    browser()
    })
  ## Create a Shiny tagList for each question type present in the instrument
  rc_instrument_ui <- reactive({
    req(rc_instrument() )
    rc_instrument() %>% 
      mutate( ## mutate shiny tags/inputs
        shiny_header = map(section_header, h3),
        shiny_field_label = case_when(is.na(required_field) ~ field_label,
                                      TRUE ~ paste(field_label,'*')),
        shiny_input = pmap(list(reviewr_type = reviewr_redcap_widget_function, 
                                field_name = shiny_inputID, 
                                field_label = shiny_field_label, 
                                choices = select_choices_or_calculations
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
    rc_instrument_ui()$shiny_taglist
    })
  
  ## Collect Instrument data
  instrumentData <- reactive({
    tibble(inputID = names(reviewr_inputs()), values = unname(reviewr_inputs())) %>% 
      mutate(class = str_detect(string = inputID, pattern = regex(pattern = '_reviewr((?!_reset).)*$',ignore_case = T))) %>% ## Find instrument inputs -- not resets
      filter(class == T) %>% 
      select(-class) %>% 
      pivot_wider(names_from = inputID, values_from = values)
  })
  
  return('instrument_data' = instrumentData) # Send to the Upload module
}

## REDCap Data Upload ----
upload_redcap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('rc_upload'))
  )
}

upload_redcap_logic <- function(input, output, session, rc_instrument) {
  ns <- session$ns
  
  rc_upload_btn <- reactive({ actionButton(inputId = ns('upload_rc'),label = "Store Abstraction") })
  rc_upload_btn_press <- reactive({ input$upload_rc })
  
  output$rc_upload <- renderUI({rc_upload_btn() })
  
  
  return(list(
    'rc_upload_btn_press' = rc_upload_btn_press
  ))
  
}


