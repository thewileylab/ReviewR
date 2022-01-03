# Adapted from: shinyREDCap: https://github.com/thewileylab/shinyREDCap/
# Helper Functions ----

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyWidgets useShinydashboard useSweetAlert
#' @importFrom shinyjs useShinyjs
#' @noRd
src_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyWidgets::useShinydashboard(),
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs()
  )
}

#' REDCap Connection
#' 
#' @description 
#' 
#' ## Overview
#' A 'safe' wrapper for [redcapAPI::redcapConnection()]. Will return diagnostic 
#' error codes in case incorrect URL or token are provided instead of failing
#' outright.
#' 
#' ## REDCap API Security
#' It is good practice to ensure that SSL certs are validated when utilizing the REDCap API. 
#' To ensure this happens, set the CURLOPT_SSL_VERIFYPEER' option to TRUE to avoid potential 
#' man in the middle attacks.
#' 
#' The redcapAPI package utilizes the httr package to perform operations using the REDCap API. 
#' Configuration options can be passed directly to httr via the config option in the 
#' [redcapAPI::redcapConnection] function. Here, we set 'ssl_verifypeer = 1L' to ensure
#' cert checking is enabled.
#' * \url{https://www.rdocumentation.org/packages/redcapAPI/versions/2.3/topics/redcapConnection}
#' * \url{https://httr.r-lib.org/reference/httr_options.html}
#'
#' @param url A string containing the https URL for your institution's REDCap API.
#' @param token A string containing the API token for your REDCap project.
#' @keywords internal
#' 
#' @importFrom redcapAPI exportProjectInformation redcapConnection
#' @importFrom stringr str_detect regex
#' @importFrom httr config
#'
#' @return A redcapAPI connection object if the URL and API token are correct 
#' ( See: [redcapAPI::redcapConnection] ). Else, return diagnostic error. 
#' 

redcap_connection <- function(url, token) { 
  connection_status <- tryCatch({
    project_info <- redcapAPI::exportProjectInformation(redcapAPI::redcapConnection(url, token, config = httr::config( ssl_verifypeer = 1L )))
    if(nrow(project_info) == 1) {
      return(redcapAPI::redcapConnection(url,token, config = httr::config( ssl_verifypeer = 1L )))
    } else {
      return('redcap_unknown_error')
    }
  }, 
  error=function(error_cond) {
    if(str_detect(as.character(error_cond), pattern = regex('Could not resolve host:', ignore_case = T)) ) {
      message("Incorrect REDCap API URL. If Macbook is 2015-2020 model year, check for stuck keys. Otherwise, make sure you used the correct URL.")
      return('redcap_url_error') 
    } else if (str_detect(as.character(error_cond), pattern = regex('You do not have permissions to use the API', ignore_case = T)) ) {
      message('Incorrect API key. Please ensure you have enabled API access to your project and/or double check your API credentials.')
      return('redcap_token_error')
    } else {
      message("An unexpected server response was received, please verify that a REDCap Instance exists at the specified URL.")
      return('redcap_unknown_error')
    }
  })
  return(connection_status)
}

## REDCap Safe Export

#' REDCap Safe Export Records
#' 
#' @description 
#' A safe wrapper around [redcapAPI::exportRecords] that does not fail when records
#' are requested from an empty REDCap project. In the event of an empty project, 
#' field names are used to create an empty data structure.
#'
#' @keywords internal
#' @param rc_con A REDCap API Connection Object
#' @param rc_field_names The field names for a REDCap instrument
#' 
#' @importFrom redcapAPI exportRecords
#' @importFrom dplyr as_tibble select mutate mutate_all
#' @importFrom magrittr %>% 
#' @importFrom purrr flatten_dfr
#' @importFrom rlang .data
#' @importFrom tidyr drop_na pivot_wider
#' 
#' @return A data frame containing existing REDCap records, or an empty data 
#' frame with the structure of what the records would look like.
#' 
safe_exportRecords <- function(rc_con, rc_field_names) {
  tryCatch({
    redcapAPI::exportRecords(rc_con, factors = F, labels = F) %>% 
      dplyr::as_tibble() %>% 
      mutate_all(as.character) %>% 
      mutate_all(replace_na, replace = '')
  }, error = function(durrrrr) {
    rc_field_names %>% 
      select(.data$export_field_name, .data$choice_value) %>% 
      mutate(choice_value = map(.x = .data$choice_value, ~ NA)) %>% 
      pivot_wider(names_from = .data$export_field_name, values_from = .data$choice_value) %>% 
      flatten_dfr() %>%
      mutate_all(as.character) %>% 
      tidyr::drop_na()
  })
}

# REDCap Instrument Render Functions ----

## Render REDCap Instrument ----
#' Render REDCap Instrument
#'
#' @description 
#' This function will select the appropriate shiny widget translation function
#' based on the provided parameters. Used to loop over REDCap project information
#' to create an entire data collection instrument which may consist of multiple
#' questions/question types.
#' 
#' @param shinyREDCap_type A string indicating a supported shinyREDCap question type. 
#' Valid options include: "shinyREDCap_text", "shinyREDCap_date", "shinyREDCap_dropdown",
#' "shinyREDCap_truefalse", "shinyREDCap_yesno", "shinyREDCap_radio", "shinyREDCap_checkbox",
#' "shinyREDCap_notes", "shinyREDCap_integer" 
#' @param id A string, containing a globally unique REDCap question identifier. Used to 
#' create a valid Shiny inputID.
#' @param field_label A string containing the question being asked. May contain
#' html formatting.
#' @param required A string, "yes" or "no". Is this a required REDCap question type?
#' @param choices REDCap choices for the question.
#' @param current_subject_data Previously saved REDCap data on the current subject.
#' @param ... Any additional parameters to pass to shiny widget inputs.
#' 
#' @keywords internal
#' 
#' @return A shiny input widget for the UI
#' 

render_redcap_instrument <- function(shinyREDCap_type, id, field_label, required, choices, current_subject_data = NULL, ... ) {
  if(shinyREDCap_type == 'shinyREDCap_text') {                    ## Text: textInput 
    shinyREDCap_textInput(id = id, field_label = field_label, value = current_subject_data, ...)
  } else if(shinyREDCap_type == 'shinyREDCap_date') {             ## Date: dateInput 
    shinyREDCap_dateInput(id = id, field_label = field_label, value = current_subject_data, ...)
  } else if (shinyREDCap_type == 'shinyREDCap_dropdown') {        ## DropDown: selectInput
    shinyREDCap_dropdown(id = id, field_label = field_label, required, choices = choices, value = current_subject_data, ...)
  } else if (shinyREDCap_type == 'shinyREDCap_truefalse') {       ## TrueFalse: radioButtoms 
    shinyREDCap_truefalse(id = id, field_label = field_label, required, value = current_subject_data, ...)
  } else if (shinyREDCap_type == 'shinyREDCap_yesno') {           ## YesNo: radioButtons 
    shinyREDCap_yesno(id = id, field_label = field_label, required, value = current_subject_data, ...)
  } else if (shinyREDCap_type == 'shinyREDCap_radio') {           ## Radio: radioButtons 
    shinyREDCap_radio(id = id, field_label = field_label, required, choices = choices, value = current_subject_data, ...)
  } else if (shinyREDCap_type == 'shinyREDCap_checkbox') {        ## Checkbox: checkboxGroupInput 
    shinyREDCap_checkbox(id = id, field_label = field_label, choices = choices, value = current_subject_data, ...)
  } else if (shinyREDCap_type == 'shinyREDCap_notes') {           ## Notes: textAreaInput 
    shinyREDCap_notes(id = id, field_label = field_label, value = current_subject_data, ...)
  } else if (shinyREDCap_type == 'shinyREDCap_integer') {         ## Integer: numericInput 
    shinyREDCap_integer(id = id, field_label = field_label, value = current_subject_data, ...)
  } else {                                                        ## Unsupported input 
    HTML(glue::glue('<font color="#ababab"><strong>{field_label}</strong><br>This is an unsupported field type.</font>'))
  }
}

## Widget Translation Functions ----
#' Shiny Widget Translation
#' 
#' @description 
#' A collection of functions to map REDCap question types as exported by the REDCap API
#' to native Shiny widgets. 
#' 
#' @inheritParams render_redcap_instrument
#' @param value Default value or previous data if question has previously been answered 
#' @param placeholder Placeholder text to help a reviewer decide how to answer the question
#' @name shiny_widget_translation
#' 
#' @import shiny
#' @importFrom dplyr mutate_all select if_else
#' @importFrom glue glue
#' @importFrom purrr flatten
#' @importFrom tibble add_row tibble
#' @importFrom tidyr separate_rows separate
#' @importFrom rlang .data
#' @importFrom stringr str_trim
#' 
#' @return A shiny input widget for the UI
#' 
NULL
#> NULL

#' @rdname shiny_widget_translation
#' 
#' @keywords internal
shinyREDCap_textInput <- function(id, field_label, value = NULL, placeholder = NULL, ...) {
  textInput(inputId = id ,label = HTML(field_label), value = value , placeholder = placeholder)
}

#' @rdname shiny_widget_translation
#' 
#' @keywords internal
shinyREDCap_dateInput <- function(id, field_label, value = NULL, ...) {
  dateInput(inputId = id, label = HTML(field_label), value = value)
}

#' @rdname shiny_widget_translation
#' 
#' @keywords internal
shinyREDCap_dropdown <- function(id, field_label, required, choices, value = NULL, ...) {
  ## Create selectable choices
  required_choice <- if_else(is.na(required), '[Leave Blank]', '[Not Yet Answered]')
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim) %>% 
    mutate_all(as.character) %>% 
    add_row(Values = '', Names = required_choice)
  dropdown_choices <- temp$Values
  names(dropdown_choices) <- temp$Names
  dropdown_choices = dropdown_choices
  selectInput(inputId = id, label = HTML(field_label), choices = dropdown_choices, selected = value)
}

#' @rdname shiny_widget_translation
#' 
#' @keywords internal
shinyREDCap_truefalse <- function(id, field_label, required, value = NULL, ...) {
  if(is.na(required) ) {
    radio_names <- list('True', 'False', HTML("<font color='grey'>[Leave Blank]</font>"))
  } else {
    radio_names <- list('True', 'False', HTML("<font color='grey'>[Not Yet Answered]</font>"))
  }
  radio_values <- c(1, 0, '')
  radioButtons(inputId = id, label = HTML(field_label), choiceNames = radio_names, choiceValues = radio_values, selected = value)
}

#' @rdname shiny_widget_translation
#' 
#' @keywords internal
shinyREDCap_yesno <- function(id, field_label, required, value = NULL, ...) {
  if(is.na(required) ) {
    radio_names <- list('Yes', 'No', HTML("<font color='grey'>[Leave Blank]</font>"))
  } else {
    radio_names <- list('Yes', 'No', HTML("<font color='grey'>[Not Yet Answered]</font>"))
  }
  radio_values <- c(1, 0, '')
  radioButtons(inputId = id, label = HTML(field_label), choiceNames = radio_names, choiceValues = radio_values, selected = value)
}

#' @rdname shiny_widget_translation
#' 
#' @keywords internal
shinyREDCap_radio <- function(id, field_label, required, choices, value = NULL, ...) {
  ## Create selectable choices
  if(is.na(required) ) {
    append_val <- list(HTML("<font color='grey'>[Leave Blank]</font>"))
  } else {
    append_val <- list(HTML("<font color='grey'>[Not Yet Answered]</font>"))
  }
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim) %>% 
    mutate_all(as.character)
  radio_names <- temp %>% 
    select(.data$Names) %>% 
    flatten() %>% 
    append(append_val)
  radio_values <- temp %>% 
    select(.data$Values) %>% 
    flatten() %>% 
    append(list(''))
  radioButtons(inputId = id, label = HTML(field_label), choiceNames = radio_names, choiceValues = radio_values, selected = value)
}

#' @rdname shiny_widget_translation
#' 
#' @keywords internal
shinyREDCap_checkbox <- function(id, field_label, choices, value = NULL, ...) {
  ## Create selectable choices
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim)
  checkbox_choices <- temp$Values
  names(checkbox_choices) <- temp$Names
  checkboxGroupInput(inputId = id, label = HTML(field_label), choices = checkbox_choices, selected = value)
}

#' @rdname shiny_widget_translation
#' 
#' @keywords internal
shinyREDCap_notes <- function(id, field_label, value = NULL, ...) {
  textAreaInput(inputId = id, label = HTML(field_label), value = value)
}

#' @rdname shiny_widget_translation
#' 
#' @keywords internal
shinyREDCap_integer <- function(id, field_label, value = NULL, ...) {
  numericInput(inputId = id, label = HTML(field_label), value = value)
}

# Datasets ----
#' REDCap Survey Complete
#'
#' A dataset containing valid REDCap "Survey Complete" Values. 
#'
#' @docType data
#'
#' @format A data frame with 2 rows and 2 variables:
#' \describe{
#'   \item{redcap_survey_complete_names}{The human readable "Survey Complete" Responses}
#'   \item{redcap_survey_complete_values}{REDCap API values for "Survey Complete" Responses}
#'   ...
#' }
"redcap_survey_complete"

#' REDCap Widget Map
#'
#' A dataset that maps REDCap question types and common validations
#'  to native shiny widgets through custom functions. 
#'
#' @docType data
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{redcap_field_type}{A REDCap Question Type}
#'   \item{redcap_field_validation}{Custom REDCap Question Type Validation}
#'   \item{shinyREDCap_widget_function}{shinyREDCap function to use when mapping to native Shiny widget}
#'   ...
#' }
"redcap_widget_map"

# Module Documentation
#' REDCap Abstraction Module
#' 
#' @description 
#' This module allows users to interact with REDCap Projects from within a Shiny application. 
#' REDCap instruments are translated into native Shiny controls/widgets and allow for the 
#' capture of abstracted information from within the R Shiny environment. Additionally, error 
#' prone fields such as MRN and reviewer information are populated automatically, based on 
#' user configured information, thus reducing the potential for error in abstracted 
#' information.
#' 
#' This module consists of the following components:
#' 
#' ## Module UI functions
#' 
#' \itemize{
#' \item{`redcap_setup_ui`}: The REDCap setup/configuration UI
#' \item{`redcap_instrument_ui`}: A shiny representation of a REDCap 
#' Instrument
#' }
#' ## Module Server function
#' \itemize{
#' \item{`redcap_server`}: The logic 
#' }
#' 
#' ## Keyboard Shortcuts
#' 
#' This module also provides a keyboard shortcut to assist with saving
#' abstracted patient data. The "meta" key refers to "ctrl" on Windows 
#' and "Cmd" on Mac.
#' * Save current instrument data: “alt + meta + s”
#' 
#' @param id The module namespace
#' @name mod_redcap
#' 
#' @return 
#' *redcap_setup_ui*:
#' \item{tagList}{The REDCap setup/configuration UI}
#' *redcap_instrument_ui*:
#' \item{tagList}{A shiny representation of a REDCap Instrument}
#' *redcap_server*: 
#' \item{reactiveValues}{
#' \itemize{
#' \item{all_review_status}: A [dplyr::tibble] containing the review status of
#' all previously reviewed individuals.
#' \item{instrument_ui}: The module instrument ui function
#' \item{is_configured}: A string, with module configuration status. Valid statuses 
#' are yes' or 'no'.
#' \item{is_connected}: A string, with module connection status. Valid statuses are
#' 'yes' or 'no'.
#' \item{moduleName}: A string, containing the module moniker.
#' \item{moduleType}: A string, with the module type (what does it do?)
#' \item{previous_selected_instrument_complete_val}: A character ("1","2","3", NA_character)
#' representing a REDCap review status. 
#' \item{setup_ui}: The module setup ui function
#' }}
#' 
NULL
#> NULL

# UI ----
## Setup ----
#' @rdname mod_redcap
#' 
#' @keywords internal
#' 
#' @importFrom shinydashboard box
#' @importFrom shinyjs hidden
#' @importFrom shinycssloaders withSpinner
#'
redcap_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    src_add_external_resources(),
    shinydashboard::box(title = 'Connect to REDCap',
                        width = '100%',
                        status = 'danger',
                        solidHeader = F,
                        div(id=ns('redcap_connect_div'),
                            uiOutput(ns('setup')),
                            uiOutput(ns('setup_connect_btn')),
                            uiOutput(ns('setup_connect_error'))
                        ),
                        div(id=ns('redcap_connect_success_div'),
                            uiOutput(ns('setup_connect_success')) %>% shinycssloaders::withSpinner() 
                        )
    ),
    shinyjs::hidden(
      div(id=ns('redcap_configure_div'),
          shinydashboard::box(title = 'Configure REDCap',
                              width = '100%',
                              status = 'danger',
                              solidHeader = F,
                              div(id=ns('redcap_configuration_options_div'),
                                  uiOutput(ns('rc_configure_identifier')),
                                  uiOutput(ns('rc_configure_reviewer')),
                                  uiOutput(ns('rc_configure_select_reviewer')),
                                  uiOutput(ns('rc_configure_select_btn'))
                              ),
                              div(id=ns('redcap_configured_success_div'),
                                  uiOutput(ns('rc_configured_message')) %>% shinycssloaders::withSpinner()
                              )
          )
      )
    )
  )
}

## Instrument ----
#' @rdname mod_redcap
#' 
#' @keywords internal
#' 
#' @importFrom shinyjs hidden
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#'
redcap_instrument_ui <- function(id) {
  ns <- NS(id)
  ## Bind keyboard to input
  save <- paste0(
    "$(document).on('keydown', function(event){",
    "  var key = event.which;",
    "  if(event.metaKey && event.altKey && key === 83){",
    "    Shiny.setInputValue('",id,"-upload', true, {priority: 'event'});",
    "  } ",
    "});"
    )
  
  tagList(
    src_add_external_resources(),
    tags$head(tags$script(HTML(save))),
    shinydashboard::box(title = "REDCap Instrument",
                        width = '100%',
                        status = 'danger',
                        solidHeader = F,
                        uiOutput(ns('instrument_selection')),
                        shinyjs::hidden(
                          div(id = ns('instrument_select_warning_div'),
                            uiOutput(ns('instrument_select_warning'))
                          )
                        ),
                        div(style='max-height:550px; overflow-y:scroll',
                            uiOutput(ns('instrument_ui')) %>% withSpinner(type = 5, color = '#e83a2f') 
                            )
                        ),
    shinydashboard::box(title = 'Save to REDCap',
                        width = '100%',
                        status = 'danger',
                        solidHeader = F,
                        div(id = ns('instrument_status_select_div'),
                            selectizeInput(inputId = ns('survey_complete'),
                                           label = 'Instrument Status',
                                           choices = NULL
                            )
                        ),
                        uiOutput(ns('redcap_instrument_complete_warn')),
                        shinyjs::hidden(
                          div(id = ns('redcap_upload_btn_div'),
                              actionButton(inputId = ns('upload'), label = 'Save to REDCap')
                          )
                        )
    )
  )
}

# Server ----
#' @rdname mod_redcap
#' 
#' @keywords internal
#' 
#' @param subject_id A [shiny::reactive] expression containing a subject identifier.
#' 
#' @importFrom dplyr arrange as_tibble case_when coalesce contains count desc distinct everything filter full_join group_by inner_join left_join mutate mutate_all mutate_at mutate_if pull rename relocate select slice summarise ungroup vars
#' @importFrom DT datatable  
#' @importFrom glue glue glue_collapse
#' @importFrom httr config   
#' @importFrom magrittr %>% 
#' @importFrom purrr flatten_chr flatten_dfr keep map map2 map2_chr pmap modify_depth
#' @importFrom redcapAPI exportProjectInformation exportFieldNames exportInstruments exportMetaData exportNextRecordName
#' @importFrom REDCapR redcap_write
#' @importFrom rlang .data :=
#' @importFrom snakecase to_sentence_case
#' @importFrom shinyjs disable hide reset show
#' @importFrom shinyWidgets confirmSweetAlert sendSweetAlert
#' @importFrom stringr str_trim str_to_lower str_detect str_split
#' @importFrom tibble deframe rownames_to_column add_row add_column tibble
#' @importFrom tidyr drop_na pivot_wider pivot_longer separate separate_rows replace_na unite unnest
#'
redcap_server <- function(id, subject_id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      ## Setup Values ----
      redcap_setup <- reactiveValues(
        ### Connection Variables
        rc_con = NULL,
        rc_project_info = NULL,
        rc_field_names = NULL,
        rc_record_id_field = NULL,
        rc_instruments_tbl = NULL,
        rc_meta_data = NULL,
        rc_record_id_label = NULL,
        rc_records = NULL,
        unsupported_fields = NULL,
        unsupported_fields_modal = NULL,
        ### Configuration Variables
        temp_identifier_field = NULL,
        temp_reviewer_field = NULL,
        config_error = NULL,
        requires_reviewer = NULL,
        identifier_label = NULL,
        identifier_field = NULL,
        reviewer_label = NULL,
        reviewer_field = NULL,
        reviewer = NULL,
        rc_configured_message = NULL,
        rc_instruments_list = ''
        )
      
      ## Return Values ----
      redcap_export <- reactiveValues(
        ### Module Info
        moduleName = 'REDCap',
        moduleType = 'abstraction',
        setup_ui = redcap_setup_ui,
        instrument_ui = redcap_instrument_ui,
        ### Connection Variables
        is_connected = 'no',
        ### Configuration Variables
        is_configured = 'no',
        ### Instrument Variables
        all_review_status = NULL,
        previous_selected_instrument_complete_val = ''
        )
      
      ## REDCap Connection ----
      setup <- reactive({
        tagList(
          textInput(inputId = ns('rc_url'),label = 'REDCap URL:',value = 'https://'),
          passwordInput(inputId = ns('rc_token'),label = 'REDCap API Token:')
          )
        })
      
      rc_connect_btn <- reactive({
        req(input$rc_url, input$rc_token)
        if(input$rc_url == '' | input$rc_token == '') {
          return(NULL)
          } else {
            actionButton(inputId = ns('rc_connect'),label = "Connect to REDCap",icon = icon('notes-medical'))
            }
        })
      
      rc_connect_error <- eventReactive(redcap_setup$rc_con, {
        if(is.character(redcap_setup$rc_con ) ) {
          if(redcap_setup$rc_con == 'redcap_url_error') {
            return(HTML("<font color='#e83a2f'>Incorrect REDCap API URL. Please ensure you entered the correct URL.</font>"))
            } else if (redcap_setup$rc_con == 'redcap_unknown_error') {
              return(HTML("<font color='#e83a2f'>An unexpected server response was received, please verify that a REDCap Instance exists at the specified URL.</font>"))
              } else if (redcap_setup$rc_con == 'redcap_token_error' ) {
                return(HTML("<font color='#e83a2f'>Incorrect API key. Please ensure you have enabled API access to your project and/or double check your API credentials.</font>"))
                } else {
                  return(NULL)
                  }
          } else {
            return(NULL)
            }
        })
      
      rc_connected_message <- eventReactive(redcap_setup$rc_project_info, {
        if (nrow(redcap_setup$rc_project_info ) > 0) {
          HTML(paste('<H3>Success!!</H3>', 
                     'You have connected to the', redcap_setup$rc_project_info$project_title, 'Project in REDCap.',
                     '<br>',
                     '<br>',
                     '<H4>Project Information:</H4>',
                     '<b>Project ID:</b>', redcap_setup$rc_project_info$project_id,
                     '<br>',
                     '<b>Created:</b>', redcap_setup$rc_project_info$creation_time,
                     '<br>',
                     '<b>Production Status:</b>', redcap_setup$rc_project_info$in_production,
                     '<br><br>',
                     '<b>Please configure a REDCap Instrument in the box below before continuing.</b>',
                     '<br><br>'))
          } else {HTML(paste('No REDCap Projects were found. Please connect to a different REDCap Project.',
                             '<br><br>')
                       )
            }
        })
      
      ## REDCap Connection UI Outputs ----
      output$setup <- renderUI({ setup() })
      output$setup_connect_btn <- renderUI({ rc_connect_btn() })
      output$setup_connect_error <- renderUI({ rc_connect_error() })
      output$setup_connect_success <- renderUI({ 
        req(rc_connected_message() )
        tagList(
          rc_connected_message(),
          actionButton(inputId = ns('rc_disconnect'),label = 'Disconnect')
          )
        })
      
      ## REDCap Connection Observers ----
      observeEvent(input$rc_connect, { ### store redcapAPI connection object (or error) when connect button is pressed
        if ( input$rc_connect == 0 ) return()
        redcap_setup$rc_con <- redcap_connection(input$rc_url, input$rc_token)
        })
      
      observeEvent(redcap_setup$rc_con, {
        message('Retrieving REDCap project information')
        if (redcap_setup$rc_con %>% class() == 'redcapApiConnection') { ### When correct information is entered, the class of rc_con will be redcapApiConnection
          shinyjs::hide('redcap_connect_div') ### Hide REDCap connection GUI
          redcap_setup$rc_project_info <- redcapAPI::exportProjectInformation(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store Project Info
          redcap_setup$rc_field_names <- redcapAPI::exportFieldNames(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Field Names
          redcap_setup$rc_record_id_field <- redcap_setup$rc_field_names %>% slice(1) %>% pull(.data$export_field_name) ### Store REDCap Record ID field (always first field)
          redcap_setup$rc_instruments_tbl <- redcapAPI::exportInstruments(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Instrument Names
          redcap_setup$rc_instruments_list <-redcap_setup$rc_instruments_tbl %>% 
            relocate(.data$instrument_label, .data$instrument_name) %>% 
            deframe()
          redcap_setup$rc_meta_data <- redcapAPI::exportMetaData(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Instrument Meta Data
          redcap_setup$rc_record_id_label <- redcap_setup$rc_meta_data %>% 
            filter(.data$field_name == redcap_setup$rc_record_id_field) %>% 
            pull(.data$field_label)
          redcap_setup$rc_meta_exploded <- redcap_setup$rc_meta_data %>% 
            select(.data$field_name, .data$field_type, .data$select_choices_or_calculations) %>% 
            mutate(select_choices_or_calculations = case_when(.data$field_type == 'yesno' ~ '1, Yes | 0, No',
                                                              .data$field_type == 'truefalse' ~ '1, True | 0, False',
                                                              TRUE ~ .data$select_choices_or_calculations %>% as.character()
                                                              )
                   ) %>% 
            separate_rows(.data$select_choices_or_calculations, sep = '\\|') %>% 
            separate(.data$select_choices_or_calculations, into = c('value','value_label'), sep = ',') %>% 
            mutate_all(str_trim) %>% 
            mutate_all(replace_na, replace = '')
          redcap_setup$rc_records <- safe_exportRecords(redcap_setup$rc_con, redcap_setup$rc_field_names)
          redcap_setup$unsupported_fields <- redcap_setup$rc_meta_data %>%
            left_join(ReviewR::redcap_widget_map, 
                      by = c('field_type' = 'redcap_field_type', 'text_validation_type_or_show_slider_number' = 'redcap_field_validation')
                      ) %>% 
              select('Instrument' = .data$form_name, 'Field Type' = .data$field_type, 'Field Label' = .data$field_label, 'Field Name' = .data$field_name, 'Validation' = .data$text_validation_type_or_show_slider_number, .data$shinyREDCap_widget_function) %>% 
              filter(is.na(.data$shinyREDCap_widget_function))
          redcap_export$is_connected <- 'yes' ### Report REDCap is connected
          message('Complete')
          ## If unsupported field types are detected, prompt user for action.
          if(nrow(redcap_setup$unsupported_fields) > 0 ) {
            redcap_setup$unsupported_fields_modal <- redcap_setup$unsupported_fields %>% 
              select(-.data$shinyREDCap_widget_function) %>% 
              DT::datatable(
                options = list(scrollX = TRUE,
                               paging = FALSE,
                               autoWidth = TRUE,
                               columnDefs = list(list(width = '40%', targets = 0)),
                               sDom  = '<"top">lrt<"bottom">ip'
                ),
                rownames = F, 
                escape = F,
                class = 'cell-border strip hover'
              )
            confirmSweetAlert(
              session = session,
              inputId = ns('unsupported_fields'),
              title = 'Warning: Unsupported fields in REDCap Instrument(s)',
              text = tagList(
                HTML('Please disconnect and remove unsupported field types, or continue with unsupported fields disabled.<br>'),
                DT::dataTableOutput(ns('redcap_unsupported_fields'))
                ),
              type = 'warning',
              btn_labels = c('Continue', 'Disconnect REDCap'),
              btn_colors = NULL,
              closeOnClickOutside = FALSE,
              showCloseButton = FALSE,
              html = TRUE
              )
          } else {
            shinyjs::show('redcap_configure_div') ### Show REDCap configure GUI
            shinyjs::show('redcap_configuration_options_div')
            }
          } 
        })
      
      observeEvent(input$unsupported_fields, {
        req(input$unsupported_fields == TRUE)
        redcap_export$is_connected <- 'no' ### Report REDCap is disconnected
        redcap_export$is_configured <- 'no'
        })
      
      observeEvent(input$unsupported_fields, {
        req(input$unsupported_fields == FALSE) 
        ## Continue with unsupported inputs
        redcap_export$is_connected <- 'yes' ### Report REDCap is connected
        shinyjs::show('redcap_configure_div') ### Show REDCap configure GUI
        shinyjs::show('redcap_configuration_options_div')
      })
      
      observeEvent(input$rc_disconnect, { ### On REDCap disconnect
        if ( input$rc_disconnect == 0 ) return()
        redcap_export$is_connected <- 'no' ### Report REDCap is disconnected
        redcap_export$is_configured <- 'no'
        })
      
      ## REDCap Configuration ----
      redcap_project_record_id_selectInput <- reactive({
        req(redcap_setup$rc_meta_data )
        selectizeInput(inputId = ns('rc_identifier_field'),
                       label = 'Which variable contains your record identifier (e.g., MRN, subject ID)?',
                       choices = redcap_setup$rc_meta_data %>%
                         slice(-1) %>% ### Remove the REDCap Identifier Field
                         filter(.data$field_type == 'text') %>% 
                         select(.data$field_label) %>%
                         deframe(),
                       options = list(create = FALSE,
                                      placeholder = 'Select record identifier, or ensure a record identifier question is present in a REDCap Instrument.'
                                      )
                       )
        })
      
      redcap_project_reviewer_id_selectInput <- reactive({
        req(redcap_setup$rc_meta_data, input$rc_identifier_field )
        selectInput(inputId = ns('rc_reviewer_field'), 
                    label = 'Which variable contains your reviewer identifier?',
                    choices = append('(Not Applicable)', 
                                     redcap_setup$rc_meta_data %>%
                                       slice(-1) %>% ### Remove the REDCap Identifier Field
                                       filter(.data$field_type == 'text' & .data$field_label != input$rc_identifier_field ) %>% 
                                       select(.data$field_label) %>%
                                       deframe()
                                     )
                    )
        })
      
      observeEvent(c(redcap_setup$rc_records, redcap_setup$rc_meta_data, input$rc_identifier_field, input$rc_reviewer_field, redcap_export$is_configured), {
        req(redcap_export$is_connected == 'yes', input$rc_reviewer_field)
        redcap_setup$temp_identifier_field <- redcap_setup$rc_meta_data %>% 
          filter(.data$field_label == input$rc_identifier_field) %>% 
          pull(.data$field_name)
        
        redcap_setup$temp_reviewer_field <- if(input$rc_reviewer_field == '(Not Applicable)') {
          '(Not Applicable)'
          } else {
            redcap_setup$rc_meta_data %>% 
              filter(.data$field_label == input$rc_reviewer_field) %>% 
              distinct(.data$field_label,.keep_all = T) %>% ## identical labels? Go with the first
              pull(.data$field_name)
            }
        qty_redcap_records <- redcap_setup$rc_records %>% 
          select(redcap_setup$temp_identifier_field) %>% 
          nrow()
        qty_identifiers <- redcap_setup$rc_records %>% 
          select(redcap_setup$temp_identifier_field) %>%
          distinct() %>% 
          nrow()
        reviews_per_identifier_field <- if(redcap_setup$temp_reviewer_field != '(Not Applicable)') {
          redcap_setup$rc_records %>%
            select(redcap_setup$temp_identifier_field, redcap_setup$temp_reviewer_field) %>% 
            group_by(!!as.name(redcap_setup$temp_identifier_field), !!as.name(redcap_setup$temp_reviewer_field) ) %>% 
            count() 
          } else {
            tibble::tibble(n = '', .rows = 0)
            }
        reviewer_check <- reviews_per_identifier_field %>% 
          filter(.data$n > 1) %>%
          flatten_dfr() %>%
          nrow()
        if(reviewer_check > 0 ) {
          redcap_setup$config_error <- 'yes'
          } else {
            redcap_setup$config_error <- 'no'
          }
        if(input$rc_reviewer_field != '(Not Applicable)' & qty_redcap_records <= qty_identifiers) {
          redcap_setup$requires_reviewer <- 'yes'
        } else if(qty_redcap_records > qty_identifiers) {
          redcap_setup$requires_reviewer <- 'yes'
        } else {
          redcap_setup$requires_reviewer <- 'no'
          }
        })
      
      rc_current_reviewer_selectInput <- reactive({
        req(input$rc_reviewer_field, redcap_setup$config_error, redcap_setup$requires_reviewer)
        if( input$rc_reviewer_field == '(Not Applicable)' | redcap_setup$temp_reviewer_field == '(Not Applicable)') {
          return(NULL)
          } else if(redcap_setup$config_error == 'yes'){
            return(HTML("<font color='#e83a2f'>Warning: This REDCap instrument contains multiple records from the same reviewer for an individual record identifier. Please visit REDCap via the web to correct the instrument. </font>"))
            } else {
              rc_previous_reviewers <- redcap_setup$rc_records %>% 
                select(redcap_setup$temp_reviewer_field)
              selectizeInput(inputId = ns('rc_current_reviewer'),
                             label = 'Select your name from the list, or enter a new one:',
                             choices = append('',
                                              rc_previous_reviewers
                                              ),
                             selected = '',
                             options = list(create = TRUE,
                                            placeholder = 'New Reviewer'))
            }
        })
      
      rc_configure_btn <- reactive({
        req(input$rc_reviewer_field, redcap_setup$requires_reviewer, redcap_setup$config_error)
        input$rc_reviewer_field
        if(input$rc_reviewer_field == '(Not Applicable)' & redcap_setup$requires_reviewer == 'yes' ) {
          return(HTML("<font color='#e83a2f'>Warning: Multiple REDCap records exist for unique record identifiers. Please configure a reviewer identifier.</font>"))
          } else if (input$rc_reviewer_field == '(Not Applicable)' & redcap_setup$requires_reviewer == 'no' ) {
            actionButton(inputId = ns('rc_configure_btn'), label = 'Configure REDCap')
            } else if (is.null(input$rc_current_reviewer) ) { ### This input is null for a very brief moment while initializing. Don't allow configuration at this point in time.
              return(NULL)
              } else if (input$rc_current_reviewer == '') {
                return(NULL)
                } else {
                  actionButton(inputId = ns('rc_configure_btn'), label = 'Configure REDCap Instrument') 
                  }
        })
      
      observeEvent(input$rc_configure_btn, {
        shinyjs::hide('redcap_configuration_options_div')
        redcap_setup$identifier_label <- input$rc_identifier_field
        redcap_setup$identifier_field <- redcap_setup$temp_identifier_field
        redcap_setup$reviewer_label <- input$rc_reviewer_field
        redcap_setup$reviewer_field <- redcap_setup$temp_reviewer_field
        redcap_setup$reviewer <- input$rc_current_reviewer
        redcap_export$is_configured <- 'yes'
        shinyjs::show('redcap_configured_success_div')
        })
      
      observeEvent(redcap_export$is_configured, 
                   ignoreInit = T, {
                     req(redcap_export$is_configured == 'yes')
                     if(redcap_setup$reviewer_field == '(Not Applicable)') {
                       redcap_setup$rc_configured_message <- HTML(
                         paste('<H3>Success!!</H3>',
                               'You have configured the REDCap Instrument.',
                               '<br>',
                               '<br>',
                               '<H4>Configuration Information:</H4>',
                               '<b>Identifier Field:</b>', redcap_setup$identifier_label,
                               '<br>',
                               '<b>Reviewer Field:</b>', redcap_setup$reviewer_label,
                               '<br><br>',
                               '<b>You may now proceed to record review. Have fun and watch out for bugs!</b>',
                               '<br><br>'))
                       } else {
                         redcap_setup$rc_configured_message <- HTML(
                           paste('<H3>Success!!</H3>',
                                 'You have configured the REDCap Instrument.',
                                 '<br>',
                                 '<br>',
                                 '<H4>Configuration Information:</H4>',
                                 '<b>Identifier Field:</b>', redcap_setup$identifier_label,
                                 '<br>',
                                 '<b>Reviewer Field:</b>', redcap_setup$reviewer_label,
                                 '<br>',
                                 '<b>Reviewer Name:</b>', redcap_setup$reviewer,
                                 '<br><br>',
                                 '<b>You may now proceed to record review. Have fun and watch out for bugs!</b>',
                                 '<br><br>'))
                         }
                     })
      
      # REDCap Configuration UI Outputs ----
      output$rc_configure_identifier <- renderUI({ redcap_project_record_id_selectInput() })
      output$rc_configure_reviewer <- renderUI({ redcap_project_reviewer_id_selectInput() })
      output$rc_configure_select_reviewer <- renderUI({ rc_current_reviewer_selectInput() })
      output$rc_configure_select_btn <- renderUI({ rc_configure_btn() })
      output$rc_configured_message <- renderUI({ 
        req(redcap_setup$rc_configured_message)
        tagList(
          redcap_setup$rc_configured_message,
          actionButton(inputId = ns('rc_reconfig'),label = 'Reconfigure REDCap')
          )
        })
      output$redcap_unsupported_fields <- DT::renderDataTable(redcap_setup$unsupported_fields_modal)
      
      observeEvent(input$rc_reconfig, { 
        shinyjs::hide('redcap_configured_success_div')
        redcap_export$is_configured <- 'no'
        redcap_setup$temp_identifier_field <- NULL
        redcap_setup$temp_reviewer_field <- NULL
        redcap_setup$config_error <- NULL
        redcap_setup$requires_reviewer <- NULL
        redcap_setup$identifier_label <- NULL
        redcap_setup$identifier_field <- NULL
        redcap_setup$reviewer_label <- NULL
        redcap_setup$reviewer_field <- NULL
        redcap_setup$reviewer <- NULL
        redcap_setup$rc_configured_message <- NULL
        redcap_setup$rc_records <- safe_exportRecords(redcap_setup$rc_con, redcap_setup$rc_field_names) ### pull records, just in case data was entered
        shinyjs::show('redcap_configuration_options_div')
        shinyjs::reset('redcap_configuration_options_div')
        })
      observeEvent(input$record_integrity_alert, {
        req(input$record_integrity_alert )
        if(input$record_integrity_alert == TRUE){
          shinyjs::hide('redcap_configured_success_div')
          redcap_export$is_configured <- 'no'
          redcap_setup$temp_identifier_field <- NULL
          redcap_setup$temp_reviewer_field <- NULL
          redcap_setup$config_error <- NULL
          redcap_setup$requires_reviewer <- NULL
          redcap_setup$identifier_label <- NULL
          redcap_setup$identifier_field <- NULL
          redcap_setup$reviewer_label <- NULL
          redcap_setup$reviewer_field <- NULL
          redcap_setup$reviewer <- NULL
          redcap_setup$rc_configured_message <- NULL
          redcap_setup$rc_records <- safe_exportRecords(redcap_setup$rc_con, redcap_setup$rc_field_names) ### pull records, just in case data was entered
          shinyjs::show('redcap_configuration_options_div')
          shinyjs::reset('redcap_configuration_options_div')
          }
        })
      
      ## Instrument Values ----  
      redcap_instrument <- reactiveValues(
        selected_instrument_meta = NULL,
        selected_instrument_complete_field = NULL,
        selected_instrument_meta_required = NULL,
        previous_data = NULL,
        previous_subject_data = NULL,
        previous_subject_instrument_formatted_data = NULL,
        previous_subject_instrument_formatted_data_labels = NULL,
        current_record_id = NULL,
        rc_instrument_ui = NULL,
        data = NULL,
        current_subject_data = NULL,
        current_subject_instrument_formatted_data = NULL,
        current_subject_instrument_formatted_data_labels = NULL,
        data_comparison = NULL,
        data_is_different = NULL,
        required_answered = FALSE,
        overwrite_modal = NULL,
        upload_status = NULL
        )
      
      ## Instrument Preparation ----
      ### Select REDCap Instrument
      instrument_select <- reactive({
        selectizeInput(inputId = ns('rc_instrument_selection'),
                       label = 'Select REDCap Instrument',
                       choices = redcap_setup$rc_instruments_list
                       )
        })
      instrument_select_warning <- reactive({ HTML("<font color='#e83a2f'>Please click 'Save to REDCap' to capture currently entered values before selecting a new instrument.</font>") })
      output$instrument_selection <- renderUI({ instrument_select() })
      output$instrument_select_warning <- renderUI({ instrument_select_warning() })
      
      ## Extract and Prep REDCap Instrument
      observeEvent(input$rc_instrument_selection, {
        req(input$rc_instrument_selection)
        redcap_instrument$selected_instrument_meta <- redcap_setup$rc_meta_data %>%
          slice(-1) %>%   ### We drop the first row, as it most likely is the auto-increment field used in REDCap
          filter(str_to_lower(.data$form_name) == input$rc_instrument_selection ) %>% # Extract the instrument based on the user selection
          rownames_to_column() %>%
          filter(!.data$field_type %in% c('slider','calc','descriptive')) %>%
          ### If some information is not defined within REDCap, it will convert those to logical types by default.  We are
          ### assuming that they will be all character values, so we need to perform explicit casting to continue with that
          ### assumption.
          mutate_if(is.logical, as.character) %>%
          left_join(ReviewR::redcap_widget_map,
                    by = c('field_type' = 'redcap_field_type', 'text_validation_type_or_show_slider_number' = 'redcap_field_validation')
                    ) %>%
          mutate(section_header = coalesce(.data$section_header, ''),
                 field_note = coalesce(.data$field_note, '')
                 )
        redcap_instrument$selected_instrument_meta_required <- redcap_instrument$selected_instrument_meta %>% 
          select(.data$field_name, .data$required_field) %>% 
          filter(.data$required_field == 'y')
        ### Create the Instrument Complete field
        redcap_instrument$selected_instrument_complete_field <- glue::glue('{input$rc_instrument_selection}_complete')
        })
      
      ## Retrieve Previous REDCap data ----
      observeEvent(c(redcap_export$is_configured, redcap_instrument$upload_status, subject_id()), {
        req(redcap_export$is_connected == 'yes', redcap_export$is_configured == 'yes')
        message('Refreshing instrument data from REDCap')
        
        ### Determine if the instrument(s) are empty by exporting the next record id. If 1 is returned, the instrument(s) are empty.
        redcap_instrument$is_empty <- if(redcapAPI::exportNextRecordName(redcap_setup$rc_con) == 1) {
          'yes'
          } else {
            'no'
            }
        
        ### Export all project records across all instruments
        redcap_instrument$previous_data <- safe_exportRecords(redcap_setup$rc_con, redcap_setup$rc_field_names)
        
        ### All Record status
        if(redcap_setup$requires_reviewer == 'yes') {
          temp_review_status <- redcap_instrument$previous_data %>% 
            select('ID' = redcap_setup$identifier_field, 'reviewer' = redcap_setup$reviewer_field, contains('_complete')) %>% 
            pivot_longer(cols = contains('_complete'), names_to = 'complete_field', values_to = 'complete_value') %>% 
            mutate(complete_field = stringr::str_remove(.data$complete_field, '_complete'),
                   complete_field = snakecase::to_sentence_case(.data$complete_field),
                   complete_value = as.numeric(.data$complete_value)
                   ) %>% 
            left_join(ReviewR::redcap_survey_complete, by = c('complete_value' ='redcap_survey_complete_values')) %>%
            mutate(complete_field = glue::glue('- {.data$complete_field}'),
                   redcap_survey_complete_names = glue::glue('<em>{.data$redcap_survey_complete_names}</em>'),
                   ) %>% 
            tidyr::unite(col = 'complete_field', .data$complete_field, .data$redcap_survey_complete_names, sep = ': ') %>% 
            select(-.data$complete_value) %>%
            group_by(.data$ID, .data$reviewer) %>% 
            summarise(review_status = glue::glue_collapse(.data$complete_field, sep = '<br>')) %>% 
            ungroup()
          
          ### Determine Status of every Record ID for all other reviewers
          all_other_reviewer_status <- temp_review_status %>% 
            filter(.data$reviewer != redcap_setup$reviewer) %>%
            mutate(reviewer = glue::glue('<b>{.data$reviewer}:</b>')) %>% 
            tidyr::unite(col = 'review_status', .data$reviewer, .data$review_status , sep = '<br>') %>% 
            group_by(.data$ID) %>% 
            summarise('REDCap Record Status:<br>Other Reviewers' = glue::glue_collapse(.data$review_status, sep = '<br>'))
          
          ### Determine Status of every Record ID for currently configured reviewer
          all_current_reviewer_status <- temp_review_status %>% 
            filter(.data$reviewer == redcap_setup$reviewer) %>%
            select(-.data$reviewer, !!glue::glue('REDCap Record Status:<br>{redcap_setup$reviewer}') := .data$review_status) 
          
          ### Combine other with current and export
          redcap_export$all_review_status <- all_other_reviewer_status %>% 
            dplyr::full_join(all_current_reviewer_status) %>% 
            mutate_all(replace_na, '<em>Review Not Started</em>')
          } else {
            redcap_export$all_review_status <- redcap_instrument$previous_data %>% 
              select('ID' = redcap_setup$identifier_field, contains('_complete')) %>% 
              pivot_longer(cols = contains('_complete'), names_to = 'complete_field', values_to = 'complete_value') %>% 
              mutate(complete_field = stringr::str_remove(.data$complete_field, '_complete'),
                     complete_field = snakecase::to_sentence_case(.data$complete_field),
                     complete_value = as.numeric(.data$complete_value)
                     ) %>% 
              left_join(ReviewR::redcap_survey_complete, by = c('complete_value' ='redcap_survey_complete_values')) %>%
              mutate(complete_field = glue::glue('- {.data$complete_field}'),
                     redcap_survey_complete_names = glue::glue('<em>{.data$redcap_survey_complete_names}</em>'),
                     ) %>% 
              tidyr::unite(col = 'complete_field', .data$complete_field, .data$redcap_survey_complete_names, sep = ': ') %>% 
              select(-.data$complete_value) %>%
              group_by(.data$ID) %>% 
              summarise('REDCap Record Status' = glue::glue_collapse(.data$complete_field, sep = '<br>')) %>% 
              ungroup()
            }
        message('REDCap Refresh Complete')
        })
      
      ## Process Previous data ----
      ### Filter down existing REDCap data to the subject in context. If no data exists, create empty data structure
      observeEvent(c(subject_id(), redcap_instrument$previous_data), {
        req(redcap_export$is_connected == 'yes', redcap_export$is_configured == 'yes', subject_id())
        ### Special case, when the REDCap Instrument has no previous data
        redcap_instrument$previous_subject_data <- if(redcap_instrument$is_empty == 'yes') { 
          redcap_instrument$previous_data
          ### Export existing Records, filtering to the subject in context  
          } else if (redcap_instrument$is_empty == 'no' & redcap_setup$requires_reviewer == 'no' ) {
            redcap_instrument$previous_data %>% 
              filter(!!as.name(redcap_setup$identifier_field ) == subject_id() )
          ### Export existing Records, filtering to the subject AND reviewer in context  
            } else {
              redcap_instrument$previous_data %>% 
                filter(!!as.name(redcap_setup$identifier_field ) == subject_id() & !!as.name(redcap_setup$reviewer_field) == redcap_setup$reviewer )
              }
        
        ### Format previous data to display appropriately in the Shiny representation of the REDCap Instrument
        redcap_instrument$previous_subject_instrument_formatted_data <- if(nrow(redcap_instrument$previous_subject_data ) > 0 ){
          if(ncol(redcap_instrument$previous_subject_data %>% select(contains('___')) ) > 0 ) {
            redcap_instrument$previous_subject_data %>%
              # Turn wide data from RedCAP to long, collapsing checkbox type questions along the way
              pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
              separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
              mutate(checkbox_value = map2_chr(.x = .data$checkbox_value, .y = .data$value_present, ~ case_when(.y == 0 ~ '',
                                                                                                                TRUE ~ .x)
                                               )
                     ) %>%
              select(-.data$value_present) %>% # remove value presence variable
              pivot_wider(names_from = .data$checkbox_questions, values_from = .data$checkbox_value, values_fn = list(checkbox_value = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
              pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
            } else {
              redcap_instrument$previous_subject_data %>%
                pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
              }
          } else if(nrow(redcap_instrument$previous_subject_data ) == 0 & redcap_setup$requires_reviewer == 'no' ) {
            if(ncol(redcap_instrument$previous_subject_data %>% select(contains('___')) ) > 0 ) {
              redcap_instrument$previous_subject_data %>%
                add_row(!!redcap_setup$identifier_field := subject_id() ) %>% # Add default data, without reviewer info, if present
                mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
                pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
                separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
                select(-.data$checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
                pivot_wider(names_from = .data$checkbox_questions, values_from = .data$value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
                pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
              } else {
                redcap_instrument$previous_subject_data %>%
                  add_row(!!redcap_setup$identifier_field := subject_id() ) %>% 
                  pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
                }
            } else {
              if(ncol(redcap_instrument$previous_subject_data %>% select(contains('___')) ) > 0 ) {
                redcap_instrument$previous_subject_data %>%
                  add_row(!!redcap_setup$identifier_field := subject_id(), !!redcap_setup$reviewer_field := redcap_setup$reviewer ) %>% # Add default data, with reviewer info, if present
                  mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
                  pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
                  separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name  
                  select(-.data$checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
                  pivot_wider(names_from = .data$checkbox_questions, values_from = .data$value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
                  pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
                } else {
                  redcap_instrument$previous_subject_data %>%
                    add_row(!!redcap_setup$identifier_field := subject_id(), !!redcap_setup$reviewer_field := redcap_setup$reviewer ) %>% 
                    pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
                  }
              }
        
        ### Add labels to previous data
        redcap_instrument$previous_subject_instrument_formatted_data_labels <- redcap_instrument$previous_subject_instrument_formatted_data %>%
          unnest(.data$previous_value) %>%
          ### This column allows us to determine if no previous data has been entered (New REDCap Record).
          mutate(is_empty = case_when(.data$previous_value == '' ~ 1,
                                      TRUE ~ 0)
                 ) %>% 
          left_join(redcap_setup$rc_meta_exploded,
                    by = c('field_name' = 'field_name', 'previous_value' = 'value')
                    ) %>% 
          mutate(previous_value_label = case_when(is.na(.data$value_label) ~ .data$previous_value,
                                                  TRUE ~ .data$value_label
                                                  )
                 ) %>% 
          select(-.data$field_type, -.data$value_label) %>% 
          group_by(.data$field_name) %>% 
          summarise(previous_value = paste(.data$previous_value, collapse = ','),
                    is_empty = min(.data$is_empty, na.rm = T),
                    previous_html = paste(.data$previous_value_label, collapse = '<br><br>'),
                    .groups = 'keep'
                    ) %>%
          distinct(.data$previous_html, .keep_all = T)
        
        ## REDCap Record ID
        redcap_instrument$current_record_id <- redcap_instrument$previous_subject_instrument_formatted_data %>% 
          filter(.data$field_name == redcap_setup$rc_record_id_field) %>% 
          rename(inputID = .data$field_name, current_value = .data$previous_value)
        })
      
      ## Create REDCap Instrument ---- 
      ### Create a Shiny tagList for each question type present in the instrument
      observeEvent(c(redcap_instrument$selected_instrument_meta, redcap_instrument$previous_subject_instrument_formatted_data), {
        req(redcap_export$is_connected == 'yes', redcap_export$is_configured == 'yes', redcap_instrument$selected_instrument_meta, redcap_instrument$previous_subject_instrument_formatted_data)
        redcap_instrument$rc_instrument_ui <- redcap_instrument$selected_instrument_meta %>%
          left_join(redcap_instrument$previous_subject_instrument_formatted_data ) %>% #### add current subject info, if present, to the mix
          mutate( ## mutate shiny tags/inputs
            shinyREDCap_widget_function = tidyr::replace_na(.data$shinyREDCap_widget_function, ''),
            shiny_header = map(.data$section_header, h3),
            shiny_field_label = case_when(is.na(.data$required_field) ~ .data$field_label,
                                          TRUE ~ paste(.data$field_label,"<br/><font color='#FC0020'>* must provide value</font>")
                                          ),
            shiny_input = pmap(list(shinyREDCap_type = .data$shinyREDCap_widget_function,
                                    id = ns(.data$field_name),
                                    field_label = .data$shiny_field_label,
                                    required = .data$required_field,
                                    choices = .data$select_choices_or_calculations,
                                    current_subject_data = .data$previous_value 
                                    ),
                               render_redcap_instrument
                               ),
            shiny_note = map(.data$field_note, tags$sub),
            shiny_taglist = pmap(list(.data$shiny_header,
                                      .data$shiny_input,
                                      .data$shiny_note
                                      ),
                                 tagList
                                 )
            )
        message('Monitoring Instrument for changes...')
      })
      
      ## Collect Instrument data ----
      ### Reactively extract data from the selected REDCap instrument inputs as they are entered by the user.
      observe({
        req(redcap_instrument$current_record_id, redcap_instrument$selected_instrument_meta, redcap_instrument$previous_data)
        redcap_instrument$data <- tibble(inputID = names(reactiveValuesToList(input)),
                                         current_value = unname(reactiveValuesToList(input))
                                         ) %>% 
          filter(.data$inputID %in% redcap_instrument$selected_instrument_meta$field_name) %>%  ### Limit to only instrument inputs
          add_row(redcap_instrument$current_record_id) ### Add REDCap Record id
        })
      
      ## Process Instrument data ----
      ## Process User Entered Data for REDCap Upload
      observeEvent(c(redcap_instrument$data, input$survey_complete), {
        req(redcap_instrument$selected_instrument_complete_field)
        shinyjs::disable(redcap_setup$identifier_field)
        shinyjs::disable(redcap_setup$reviewer_field)
        req(redcap_instrument$data)
        redcap_instrument$current_subject_data <- redcap_instrument$selected_instrument_meta %>%
          select(.data$shinyREDCap_widget_function, .data$field_name, .data$select_choices_or_calculations) %>% ## Include select_choices_or_calculations so that all columns can be sent back to REDCap. This allows for overwriting old data with blank ''
          add_row(field_name = redcap_setup$rc_record_id_field) %>% ## Add REDCap record ID field back into the instrument, so it can be joined with any previous data.
          left_join(redcap_instrument$data, by = c('field_name' = 'inputID')) %>% ## Join the instrument inputs with the selected instrument. This ensures inputs are collected only for the active instrument
          modify_depth(2, as.character) %>% ## the input values are all lists at this moment. Dive into each list (depth = 2) and make sure that the values within the list are coded as characters
          separate_rows(.data$select_choices_or_calculations, sep = '\\|') %>% ## Expand select_choices_or_calculations
          mutate(select_choices_or_calculations = str_trim(.data$select_choices_or_calculations)) %>% ## Trim
          separate(.data$select_choices_or_calculations, into = c('rc_val','rc_label'), sep = ',') %>% ## Separate
          ## This mutate adds additional column names to hold values for checkbox questions
          mutate(rc_label = str_trim(.data$rc_label), ## Trim
                 inputID = pmap(list(x = .data$shinyREDCap_widget_function, y = .data$field_name, z = .data$rc_val ),  function(x,y,z) case_when(str_detect(string = x, pattern = 'shinyREDCap_checkbox') ~ paste0(y, '___', z), ## Create additional column names for inputs where multiple inputs are allowed
                                                                                                                                                        TRUE ~ y)
                                ),
                 current_value = pmap(list(x = .data$shinyREDCap_widget_function, y = .data$rc_val, z = .data$current_value), function(x,y,z) case_when(str_detect(string = x, pattern = 'shinyREDCap_checkbox') & y == z ~ '1',
                                                                                                                                                               str_detect(string = x, pattern = 'shinyREDCap_checkbox') & y != z ~ '',
                                                                                                                                                               TRUE ~ z)
                                      ),
                 inputID = flatten_chr(.data$inputID)
                 ) %>%
          select(.data$inputID, .data$current_value) %>%
          unnest(cols = .data$current_value, keep_empty = T) %>% ## in the case that all checkbox questions are de-selected, this keeps empty values, but stores them as NA.
          ## This mutates values to blanks, except for the special case when the record ID is NA. We would like to drop this value, if NA.
          mutate(current_value = map2_chr(.x = .data$inputID, .y = .data$current_value, ~ case_when(str_detect(string = .x,pattern = !!redcap_setup$rc_record_id_field) & is.na(.y) ~ .y,
                                                                                                    is.na(.y) ~ '',
                                                                                                    TRUE ~ .y)
                                          )
                 ) %>%
          arrange(desc(.data$current_value)) %>%
          distinct(.data$inputID,.keep_all = T) %>%
          # tidyr::drop_na() %>%
          pivot_wider(names_from = .data$inputID, values_from = .data$current_value) %>%
          select(!!redcap_setup$rc_record_id_field, everything() ) %>% ## RedCAP API likes the record identifier in the first column
          flatten_dfr() %>% 
          ## Add Instrument complete value
          add_column(!!redcap_instrument$selected_instrument_complete_field := input$survey_complete)
        
        ### Format current data to for comparison with previous REDCap instrument formatted data
        redcap_instrument$current_subject_instrument_formatted_data <- if(ncol(redcap_instrument$current_subject_data %>% select(contains('___')) ) > 0 ) {
          redcap_instrument$current_subject_data %>%
            # Turn wide data from RedCAP to long, collapsing checkbox type questions along the way
            pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
            separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
            arrange(as.numeric(.data$checkbox_value)) %>% ## Ensure that order is correct so that list variables will have responses in the correct place.
            mutate(checkbox_value = map2_chr(.x = .data$checkbox_value, .y = .data$value_present, ~ case_when(.y == '' ~ '',
                                                                                                              TRUE ~ .x)
                                             )
                   ) %>%
            select(-.data$value_present) %>% # remove value presence variable
            pivot_wider(names_from = .data$checkbox_questions, values_from = .data$checkbox_value, values_fn = list(checkbox_value = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
            pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'current_value', values_transform = list(current_value = as.list), values_ptypes = list(current_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
          } else {
            redcap_instrument$current_subject_data %>% 
              pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'current_value', values_transform = list(current_value = as.list), values_ptypes = list(current_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
            }
        
        ### Add labels to current data  
        redcap_instrument$current_subject_instrument_formatted_data_labels <- redcap_instrument$current_subject_instrument_formatted_data %>%
          unnest(.data$current_value) %>%
          left_join(redcap_setup$rc_meta_exploded,
                    by = c('field_name' = 'field_name', 'current_value' = 'value')
                    ) %>% 
          mutate(current_value_label = case_when(is.na(.data$value_label) ~ .data$current_value,
                                                 TRUE ~ .data$value_label
                                                 )
                 ) %>% 
          select(-.data$field_type, -.data$value_label) %>% 
          group_by(.data$field_name) %>% 
          mutate(current_value = paste(.data$current_value, collapse = ','),
                 current_html = paste(.data$current_value_label, collapse = '<br><br>')) %>%
          distinct(.data$current_html, .keep_all = T)
        })
      
      ## Determine Changes ----
      observeEvent(c(redcap_instrument$previous_subject_instrument_formatted_data_labels, redcap_instrument$current_subject_instrument_formatted_data_labels), {
        req(redcap_instrument$previous_subject_instrument_formatted_data_labels, redcap_instrument$current_subject_instrument_formatted_data_labels)
        ### Combine previous and current data to determine what, if anything, has changed   
        redcap_instrument$data_comparison <- redcap_instrument$previous_subject_instrument_formatted_data_labels %>% 
          inner_join(redcap_instrument$current_subject_instrument_formatted_data_labels, by = c('field_name' = 'field_name')) %>% 
          mutate(diff = case_when(.data$previous_value != .data$current_value ~ T,
                                  TRUE ~ F
                                  )
                 ) %>% 
          filter(.data$field_name != redcap_setup$rc_record_id_field & diff == TRUE) %>% ## This will be different when entering new data
          filter(!.data$field_name %in% c(redcap_setup$reviewer_field, redcap_setup$identifier_field))
        redcap_instrument$data_is_different <- nrow(redcap_instrument$data_comparison) > 0
        
        ### Create modal for displaying changes
        redcap_instrument$overwrite_modal <- redcap_instrument$data_comparison %>% 
          ungroup() %>% 
          left_join(redcap_instrument$selected_instrument_meta %>% select(.data$field_name, .data$field_label)) %>% 
          select('Question' = .data$field_label, 'Previous Value' = .data$previous_html, 'New Value' = .data$current_html) %>%
          filter(.data$Question != is.na(.data$Question)) %>% ### Remove instrument complete differences from display modal
          dplyr::mutate_at(dplyr::vars(-.data$Question), stringr::str_split, '<br>') %>% 
          mutate('Previous Value' = purrr::map(.data$`Previous Value`, 
                                          ~purrr::keep(.x, ~ stringr::str_detect(.x, '') ) 
                                          ),
                 'Previous Value' = purrr::map(.data$`Previous Value`,
                                          ~glue::glue_collapse(.x, sep = '<br><br>')
                                          ),
                 'New Value' = purrr::map(.data$`New Value`, 
                                          ~purrr::keep(.x, ~ stringr::str_detect(.x, '') ) 
                                          ),
                 'New Value' = purrr::map(.data$`New Value`,
                                          ~glue::glue_collapse(.x, sep = '<br><br>')
                                          )
                 ) %>% 
          DT::datatable(
            options = list(scrollX = TRUE,
                           paging = FALSE,
                           autoWidth = TRUE,
                           columnDefs = list(list(width = '40%', targets = 0)),
                           sDom  = '<"top">lrt<"bottom">ip'
                           ),
            rownames = F, 
            escape = F,
            class = 'cell-border strip hover'
          )
      })
      
      ## Upload Button ----
      ### Show/Hide the REDCap Upload button based on whether new data has been entered
      observeEvent(redcap_instrument$data_comparison, {
        # browser()
        if(redcap_instrument$data_is_different == TRUE) { # Instrument Data differs from existing data 
          if(nrow(redcap_instrument$data_comparison %>% filter(!str_detect(.data$field_name, '_instrument_complete')) ) == 0) { # No previous data, no new data entered (enable)
            # Enable Instrument Switching
            shinyjs::enable('rc_instrument_selection')  
            shinyjs::hide('instrument_select_warning_div')
            # Disable Upload Button
            shinyjs::hide('redcap_upload_btn_div')
            } else {  # Data differs from previous
              # Disable Instrument Switching
              shinyjs::disable('rc_instrument_selection')  
              shinyjs::show('instrument_select_warning_div')
              # Enable Upload Button
              shinyjs::show('redcap_upload_btn_div')
              }
          } else { # Instrument Data identical to existing data
            # Enable Instrument Switching
            shinyjs::enable('rc_instrument_selection')  
            shinyjs::hide('instrument_select_warning_div')
            # Disable Upload Button
            shinyjs::hide('redcap_upload_btn_div')
            }
        })
      
      ## Required Responses ----
      ## Check to see if all required questions have been answered
      observeEvent(c(redcap_instrument$selected_instrument_meta_required, redcap_instrument$current_subject_instrument_formatted_data), {
        req(redcap_instrument$current_subject_instrument_formatted_data)
        redcap_instrument$qty_required <- nrow(redcap_instrument$selected_instrument_meta_required)
        redcap_instrument$qty_required_answered <- suppressWarnings(redcap_instrument$current_subject_instrument_formatted_data %>% 
                                                                      left_join(redcap_instrument$selected_instrument_meta_required) %>% 
                                                                      filter(.data$required_field == 'y') %>%
                                                                      unnest(.data$current_value) %>% 
                                                                      mutate(answered = case_when(.data$current_value != '' ~ 1,
                                                                                                  TRUE ~ 0)
                                                                             ) %>% 
                                                                      group_by(.data$field_name) %>% 
                                                                      summarise(answered = max(.data$answered,na.rm = T),.groups = 'drop') %>% 
                                                                      filter(.data$answered > 0 ) %>% 
                                                                      nrow()
                                                                    )
        redcap_instrument$required_answered <- redcap_instrument$qty_required == redcap_instrument$qty_required_answered
        })
      
      ## REDCap Survey Complete ----
      observeEvent(c(input$rc_instrument_selection, redcap_instrument$previous_subject_data, redcap_instrument$required_answered), {
        req(input$rc_instrument_selection, redcap_instrument$previous_subject_data, redcap_instrument$selected_instrument_complete_field)
        ### Choices to present to user
        choices <- if (redcap_instrument$required_answered == TRUE) {
          ReviewR::redcap_survey_complete
        } else {
          ReviewR::redcap_survey_complete %>% 
            filter(.data$redcap_survey_complete_names != 'Complete')
          }
        
        ### Existing Instrument Complete Value
        redcap_export$previous_selected_instrument_complete_val <- redcap_instrument$previous_subject_data %>% 
          pull(redcap_instrument$selected_instrument_complete_field)
        
        selected <- if (redcap_instrument$required_answered == TRUE) {
          redcap_export$previous_selected_instrument_complete_val
          } else if(redcap_instrument$required_answered == FALSE) {
            0
            } else if (identical(redcap_export$previous_selected_instrument_complete_val, character(0)) ) {
              ''
              } else {
                redcap_export$previous_selected_instrument_complete_val
                }
        updateSelectizeInput(session = session, 
                             inputId = 'survey_complete',
                             choices = choices %>% deframe(),
                             selected = selected,
                             server = T,
                             options = list(create = FALSE,
                                            placeholder = 'Review Not Started'))
        })
      
      ## Instrument Complete Warning ----
      observeEvent(c(redcap_instrument$data_comparison, redcap_instrument$required_answered), {
        req(redcap_instrument$data_comparison)
        ### Create text for displaying Instrument Status Changes
        temp_complete_diff <- redcap_instrument$data_comparison %>% 
          ungroup() %>% 
          filter(.data$field_name == redcap_instrument$selected_instrument_complete_field) %>% 
          mutate(previous_value = case_when(.data$previous_value == '0' ~ 'Incomplete',
                                            .data$previous_value == '1' ~ 'Unverified',
                                            .data$previous_value == '2' ~ 'Complete',
                                            TRUE ~ 'Review Not Started'    
                                            ),
          current_value = case_when(.data$current_value == '0' ~ 'Incomplete',
                                    .data$current_value == '1' ~ 'Unverified',
                                    .data$current_value == '2' ~ 'Complete',
                                    TRUE ~ 'Review Not Started'    
                                    )
          )
        
        redcap_instrument$complete_status_html <- if(redcap_instrument$data_is_different == TRUE & nrow(temp_complete_diff) > 0) {
          complete_previous <- temp_complete_diff %>% pull(.data$previous_value)
          complete_new <- temp_complete_diff %>% pull(.data$current_value)
          if(redcap_instrument$required_answered == FALSE & complete_previous != complete_new) {
            ### https://external-preview.redd.it/AKOGxCJ0ksM7c5zzIxWg0kDOL-llpMlKpZbcvjohFHs.png?auto=webp&s=571052da630cc6b755bd7bf8f202bdd3e8329e8c
            HTML(glue::glue('<br>Note: The instrument status has been automatically changed from <em>{complete_previous}</em> to <em>{complete_new}</em> due to missing answers for required questions.
                            <br><br>Changed Data:')
                 )
            } else {
            HTML(glue::glue('<br>The instrument status has changed from <em>{complete_previous}</em> to <em>{complete_new}</em>
                            <br><br>Changed data:')
                 )
              }
          } else {
            NULL
            }
        })
      
      instrument_complete_warn <- reactive({
        if(redcap_instrument$required_answered == TRUE) {
          NULL
          } else {
            HTML("<font color='#e83a2f'>Warning: To select 'Complete' please answer all required questions.</font>")
            }
        })
      
      ## Upload Data to REDCap ----
      ### Here, we decide what to do. 
      observeEvent(input$upload, ignoreInit = T, {
        req(redcap_instrument$data_is_different == TRUE)
        ### Pause before upload. Evaluate your life choices up until this point.
        message('Determining overwrite status...')
        upload_checkData <- if(redcap_setup$requires_reviewer == 'yes') {
          safe_exportRecords(redcap_setup$rc_con, redcap_setup$rc_field_names) %>% 
            filter(!!as.name(redcap_setup$identifier_field) == subject_id() & !!as.name(redcap_setup$reviewer_field) == !!redcap_setup$reviewer)
          } else {
            safe_exportRecords(redcap_setup$rc_con, redcap_setup$rc_field_names) %>% 
              filter(!!as.name(redcap_setup$identifier_field) == subject_id() )
            }
        record_integrity <- identical(upload_checkData, redcap_instrument$previous_subject_data) ## Has someone else modified the record since you first began?
        overwrite_existing <- nrow(upload_checkData) > 0
        
        if(record_integrity == FALSE) {
          message('Record integrity failure')
          record_integrity_msg <- if(redcap_setup$requires_reviewer == 'yes') {
            HTML(glue::glue("This record has been modified since you began working on it using your name as a configured reviewer. <br><br><em><font color='#e83a2f'>New values will not be uploaded to REDCap.</font></em>"))
            } else {
              HTML(glue::glue("This record has been modified since you began working on it. Please consider configuring a reviewer for this REDCap Project so that multiple people can work on the same {redcap_setup$identifier_label} without conflict. <br><br><em><font color='#e83a2f'>New values will not be uploaded to REDCap.</font></em>"))
              }
          confirmSweetAlert(
            session = session,
            inputId = ns('record_integrity_alert'),
            title = 'Here be dragons!',
            text = record_integrity_msg,
            type = 'error',
            btn_labels = c("Return to Instrument", 'Reconfigure REDCap'),
            btn_colors = NULL,
            closeOnClickOutside = FALSE,
            showCloseButton = FALSE,
            html = TRUE
            )
          } else if(overwrite_existing == TRUE & record_integrity == TRUE) {
            message('Existing record detected. Overwrite?')
            ### Are we overwriting existing REDCap data? Notify the user, else upload to redcap
            confirmSweetAlert(
              session = session,
              inputId = ns('confirm_overwrite'),
              title = 'Warning! Overwriting existing REDCap data.',
              text = tagList(redcap_instrument$complete_status_html,
                             DT::dataTableOutput(ns('redcap_overwrite'))
                             ),
              type = "warning",
              btn_labels = c("Cancel", "Save to REDCap"),
              btn_colors = NULL,
              closeOnClickOutside = FALSE,
              showCloseButton = FALSE,
              html = TRUE
              )
            } else {
              redcap_instrument$upload_data <- if(redcap_setup$requires_reviewer == 'yes') {
                redcap_instrument$current_subject_data %>%
                  ### Only upload non-empty data. REDCap hates empty data. Turn empty to NA to 'reset' in REDCap
                  pivot_longer(cols = everything(),
                               names_to = 'field_name',
                               values_to = 'value'
                               ) %>% 
                  mutate(value = case_when(.data$value == '' ~ NA_character_,
                                           TRUE ~ .data$value)
                         ) %>% 
                  pivot_wider(names_from = .data$field_name, values_from = .data$value) %>% 
                  ### Always ensure subject id and reviewer are uploaded when a reviewer is configured
                  mutate(!!redcap_setup$identifier_field := subject_id(),
                         !!redcap_setup$reviewer_field := redcap_setup$reviewer
                         )
                } else {
                  message('No conflicts detected. Uploading abstraction data to REDCap')
                  redcap_instrument$current_subject_data %>%
                    ### Only upload non-empty data. REDCap hates empty data. Turn empty to NA to 'reset' in REDCap
                    pivot_longer(cols = everything(),
                                 names_to = 'field_name',
                                 values_to = 'value'
                                 ) %>% 
                    mutate(value = case_when(.data$value == '' ~ NA_character_,
                                             TRUE ~ .data$value)
                           ) %>% 
                    pivot_wider(names_from = .data$field_name, values_from = .data$value) %>% 
                    ## Always ensure subject id is uploaded
                    mutate(!!redcap_setup$identifier_field := subject_id())
                  }
              ## Determine if a new record id is needed
              temp_redcap_record_id <- redcap_instrument$upload_data %>%
                pull(redcap_setup$rc_record_id_field)
              redcap_instrument$upload_data <- if(is.na(temp_redcap_record_id) ) {
                redcap_instrument$upload_data %>% 
                  mutate(!!redcap_setup$rc_record_id_field :=  redcapAPI::exportNextRecordName(redcap_setup$rc_con))
                } else {
                  redcap_instrument$upload_data
                  }
          ## Perform Upload
          redcap_instrument$upload_status <- REDCapR::redcap_write(ds_to_write = redcap_instrument$upload_data, 
                                                                   redcap_uri = redcap_setup$rc_con$url,
                                                                   token = redcap_setup$rc_con$token,
                                                                   verbose = F,
                                                                   config_options = httr::config( ssl_verifypeer = 1L )
                                                                   )
          upload_message <- paste('REDCap', redcap_setup$rc_record_id_label, redcap_instrument$upload_status$affected_ids, 'uploaded successfully.')
          sendSweetAlert(
            session = session,
            title = "Success!!",
            text = upload_message,
            btn_labels = NA,
            type = "success"
            )
          ## Clear old data
          redcap_instrument$previous_data <- NULL
          redcap_instrument$previous_subject_data <- NULL 
          redcap_instrument$previous_subject_instrument_formatted_data <- NULL
          redcap_instrument$previous_subject_instrument_formatted_data_labels <- NULL
          redcap_instrument$current_subject_data <- NULL 
          redcap_instrument$current_subject_instrument_formatted_data <- NULL
          redcap_instrument$current_subject_instrument_formatted_data_labels <- NULL
          }
        })
      
      ### Overwrite confirmation confirmed, write to REDCap, else don't
      observeEvent(input$confirm_overwrite, {
        if(input$confirm_overwrite == TRUE) {
          message('Overwriting existing abstraction data in REDCap')
          ### Add instrument complete status
          redcap_instrument$overwrite_data <- if(redcap_setup$requires_reviewer == 'yes') {
            redcap_instrument$current_subject_data %>% 
              ### Only upload non-empty data. REDCap hates empty data. Turn empty to NA to 'reset' in REDCap
              pivot_longer(cols = everything(),
                           names_to = 'field_name',
                           values_to = 'value'
                           ) %>% 
              mutate(value = case_when(.data$value == '' ~ NA_character_,
                                       TRUE ~ .data$value)
                     ) %>% 
              pivot_wider(names_from = .data$field_name, values_from = .data$value) %>% 
              ### Always ensure subject id and reviewer are uploaded when a reviewer is configured
              mutate(!!redcap_setup$identifier_field := subject_id(),
                     !!redcap_setup$reviewer_field := redcap_setup$reviewer
                     )
            } else {
              redcap_instrument$current_subject_data %>%
                ### Only upload non-empty data. REDCap hates empty data. Turn empty to NA to 'reset' in REDCap
                pivot_longer(cols = everything(),
                             names_to = 'field_name',
                             values_to = 'value'
                             ) %>% 
                mutate(value = case_when(.data$value == '' ~ NA_character_,
                                         TRUE ~ .data$value)
                       ) %>% 
                pivot_wider(names_from = .data$field_name, values_from = .data$value) %>% 
                ## Always ensure subject id is uploaded
                mutate(!!redcap_setup$identifier_field := subject_id())
              }
          ## Perform Upload
          redcap_instrument$upload_status <- REDCapR::redcap_write(ds_to_write = redcap_instrument$overwrite_data, 
                                                                   redcap_uri = redcap_setup$rc_con$url,
                                                                   token = redcap_setup$rc_con$token,
                                                                   verbose = F, 
                                                                   config_options = httr::config( ssl_verifypeer = 1L )
                                                                   )
          overwrite_message <- paste('REDCap', redcap_setup$rc_record_id_label, redcap_instrument$upload_status$affected_ids, 'modified successfully.')
          sendSweetAlert(
            session = session,
            title = "Success!!",
            text = overwrite_message,
            btn_labels = NA,
            type = "info"
            )
          ## Clear old data
          redcap_instrument$previous_data <- NULL
          redcap_instrument$previous_subject_data <- NULL  
          redcap_instrument$previous_subject_instrument_formatted_data <- NULL
          redcap_instrument$previous_subject_instrument_formatted_data_labels <- NULL
          redcap_instrument$current_subject_data <- NULL 
          redcap_instrument$current_subject_instrument_formatted_data <- NULL
          redcap_instrument$current_subject_instrument_formatted_data_labels <- NULL
          } else {
            message('Canceled upload.')
            }
        })
      
      ## Instrument UI Outputs ----
      output$instrument_ui <- renderUI({ 
        req(redcap_instrument$rc_instrument_ui$shiny_taglist)
        redcap_instrument$rc_instrument_ui$shiny_taglist 
        })
      output$redcap_instrument_complete_warn <- renderUI({ instrument_complete_warn() })
      output$redcap_overwrite <- DT::renderDataTable({ redcap_instrument$overwrite_modal })
      
      
      ## Cleanup ----
      ### Reset Setup Values on disconnect
      observeEvent(redcap_export$is_connected, ignoreInit = T, {
        if(redcap_export$is_connected == 'no') {
          ### Reset Connection Variables
          redcap_setup$rc_con <- NULL
          redcap_setup$rc_project_info <- NULL
          redcap_setup$rc_field_names <- NULL
          redcap_setup$rc_record_id_field <- NULL
          redcap_setup$rc_instruments_tbl <- NULL
          redcap_setup$rc_meta_data <- NULL
          redcap_setup$rc_record_id_label <- NULL
          redcap_setup$rc_records <- NULL
          redcap_setup$unsupported_fields <- NULL
          redcap_setup$unsupported_fields_modal <- NULL
          ### Reset Configuration Variables
          redcap_setup$temp_identifier_field <- NULL
          redcap_setup$temp_reviewer_field <- NULL
          redcap_setup$config_error <- NULL
          redcap_setup$requires_reviewer <- NULL
          redcap_setup$identifier_label <- NULL
          redcap_setup$identifier_field <- NULL
          redcap_setup$reviewer_label <- NULL
          redcap_setup$reviewer_field <- NULL
          redcap_setup$reviewer <- NULL
          redcap_setup$rc_configured_message <- NULL
          redcap_setup$rc_instruments_list <- ''
          ### Reset UI
          shinyjs::hide('redcap_configured_success_div')
          shinyjs::show('redcap_connect_div') ### Show REDCap connection GUI
          shinyjs::reset('redcap_connect_div') ### Reset inputs on REDCap connection GUI
          shinyjs::reset('redcap_configure_div')
          shinyjs::hide('redcap_configure_div') ### Hide REDCap configuration GUI
          message('REDCap Disconnect')
        }
      })
      
      
      ### Reset Instrument/Export Values on disconnect or reconfigure
      observeEvent(c(redcap_export$is_connected, redcap_export$is_configured), {
        if(redcap_export$is_connected == 'no' | redcap_export$is_configured == 'no') {
          ### Reset Exports
          redcap_export$all_review_status <- NULL
          redcap_export$previous_selected_instrument_complete_val = ''
          ### Instrument Metadata
          redcap_instrument$selected_instrument_meta <- NULL
          redcap_instrument$selected_instrument_complete_field <- NULL
          redcap_instrument$selected_instrument_meta_required <- NULL
          ### Instrument Data
          redcap_instrument$data <- NULL
          redcap_instrument$current_record_id <- NULL
          redcap_instrument$current_subject_data <- NULL
          redcap_instrument$current_subject_instrument_formatted_data <- NULL
          redcap_instrument$current_subject_instrument_formatted_data_labels <- NULL
          redcap_instrument$previous_data <- NULL
          redcap_instrument$previous_subject_data <- NULL
          redcap_instrument$previous_subject_instrument_formatted_data <- NULL
          redcap_instrument$previous_subject_instrument_formatted_data_labels <- NULL
          ### Instrument Data
          redcap_instrument$rc_instrument_ui <- NULL
          redcap_instrument$data_comparison <- NULL
          redcap_instrument$data_is_different <- NULL
          redcap_instrument$required_answered <- FALSE
          redcap_instrument$overwrite_modal <- NULL
          redcap_instrument$upload_status <- NULL
          ### Reset UI
          shinyjs::hide('redcap_upload_btn_div')
          }
        }) 
      
      ## Return ----
      return(redcap_export)
      }
  )
}
