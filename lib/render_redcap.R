## Create Shiny Widget Translation Functions ----
reviewr_textInput <- function(id, field_label, value = NULL, placeholder = NULL, ...) {
  textInput(inputId = id ,label = HTML(field_label), value = value , placeholder = placeholder)
  }

reviewr_dateInput <- function(id, field_label, value = NULL, ...) {
  dateInput(inputId = id, label = HTML(field_label), value = value)
  }

reviewr_dropdown <- function(id, field_label, choices, value = NULL, ...) {
  ## Create selectable choices
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim) %>% 
    mutate_all(as.character) %>% 
    add_row(Values = '', Names = '[Leave Blank]')
  dropdown_choices <- temp$Values
  names(dropdown_choices) <- temp$Names
  dropdown_choices = dropdown_choices
  selectInput(inputId = id, label = HTML(field_label), choices = dropdown_choices, selected = value)
  }

reviewr_truefalse <- function(id, field_label, value = NULL, ...) {
  radio_names <- list('True', 'False', HTML("<font color='grey'>[Leave Blank]</font>"))
  radio_values <- c(1, 0, '')
  radioButtons(inputId = id, label = HTML(field_label), choiceNames = radio_names, choiceValues = radio_values, selected = value)
  }

reviewr_yesno <- function(id, field_label, value = NULL, ...) {
  radio_names <- list('Yes', 'No', HTML("<font color='grey'>[Leave Blank]</font>"))
  radio_values <- c(1, 0, '')
  radioButtons(inputId = id, label = HTML(field_label), choiceNames = radio_names, choiceValues = radio_values, selected = value)
  }

reviewr_radio <- function(id, field_label, choices, value = NULL, ...) {
  ## Create selectable choices
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim) %>% 
    mutate_all(as.character)
    #add_row(Values = '', Names = HTML("<font color='grey'>[Leave Blank]</font>"))
  radio_names <- temp %>% 
    select(Names) %>% 
    flatten() %>% 
    append(list(HTML("<font color='grey'>[Leave Blank]</font>")))
  radio_values <- temp %>% 
    select(Values) %>% 
    flatten() %>% 
    append(list(''))
  radioButtons(inputId = id, label = HTML(field_label), choiceNames = radio_names, choiceValues = radio_values, selected = value)
  }

reviewr_checkbox <- function(id, field_label, choices, value = NULL, ...) {
  ## Create selectable choices
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim)
  checkbox_choices <- temp$Values
  names(checkbox_choices) <- temp$Names
  checkboxGroupInput(inputId = id, label = HTML(field_label), choices = checkbox_choices, selected = value)
  }

reviewr_notes <- function(id, field_label, value = NULL, ...) {
  textAreaInput(inputId = id, label = HTML(field_label), value = value)
  }

reviewr_integer <- function(id, field_label, value = NULL, ...) {
  numericInput(inputId = id, label = HTML(field_label), value = value)
}

## Render REDCap Instrument shinyInput Tags ----
## Remember to add default values!! WIP
render_redcap <- function(reviewr_type, field_name, field_label, choices, current_subject_data = NULL, other_default_data = NULL ) {
  if(reviewr_type == 'reviewr_text') {   ## Text: textInput ----
    reviewr_textInput(id = field_name, field_label = field_label, value = current_subject_data)
  } else if(reviewr_type == 'reviewr_date') {             ## Date: dateInput ----
    reviewr_dateInput(id = field_name, field_label = field_label, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_dropdown') {        ## DropDown: selectInput
    reviewr_dropdown(id = field_name, field_label = field_label, choices = choices, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_truefalse') {       ## TrueFalse: radioButtoms ---- 
    reviewr_truefalse(id = field_name, field_label = field_label, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_yesno') {           ## YesNo: radioButtons ----
    reviewr_yesno(id = field_name, field_label = field_label, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_radio') {           ## Radio: radioButtons ----
    reviewr_radio(id = field_name, field_label = field_label, choices = choices, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_checkbox') {        ## Checkbox: checkboxGroupInput ----
    reviewr_checkbox(id = field_name, field_label = field_label, choices = choices, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_notes') {           ## Notes: textAreaInput ----
    reviewr_notes(id = field_name, field_label = field_label, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_integer') {         ## Integer: numericInput ----
    reviewr_integer(id = field_name, field_label = field_label, value = current_subject_data)
  } else {                                                ## Unsupported input ----
    reviewr_textInput(id = field_name, field_label = "This is an unsupported field type", placeholder = reviewr_type)
    }
  }
