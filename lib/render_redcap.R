## Create Shiny Widget Translation Functions ----
reviewr_textInput <- function(id, field_label, value = NULL, placeholder = NULL, ...) {
  textInput(inputId = id ,label = field_label,value = value , placeholder = placeholder)
}

reviewr_dateInput <- function(id, field_label, value = NULL, ...) {
  dateInput(inputId = id, label = field_label, value = value)
}

reviewr_dropDown <- function(id, field_label, choices = NULL, value = NULL, ...) {
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim)
  dropdown_choices <- temp$Values
  names(dropdown_choices) <- temp$Names
  selectInput(inputId = id, label = field_label, choices = dropdown_choices, selected = value)
}

## Render REDCap Instrument Tags ----
render_redcap <- function(reviewr_type, field_name, field_label, choices, current_subject_data = NULL, other_default_data = NULL ) {
  ## Text: textInput ----
  if(reviewr_type == 'reviewr_text') {
    reviewr_textInput(id = field_name, field_label = field_label)
  ## Date: dateInput ----
  } else if(reviewr_type == 'reviewr_date') {
    reviewr_dateInput(id = field_name, field_label = field_label)
  ## DropDown: selectInput  
  } else if (reviewr_type == 'reviewr_dropdown') {
    reviewr_dropDown(id = field_name, field_label = field_label, choices = choices)
  } else {
    ## unsupported input ----
    reviewr_textInput(id = field_name, field_label = "This is an unsupported field type", placeholder = reviewr_type)
    }
}

## TESTING
instrument %>% 
  mutate(shiny_input = pmap(list(reviewr_type = reviewr_redcap_widget_function, 
                                 field_name = shiny_inputID, 
                                 field_label = field_label, 
                                 choices = select_choices_or_calculations
                                 ), 
                            render_redcap
                            )
         )


