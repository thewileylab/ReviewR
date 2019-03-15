library(shiny)

#' Creates the appropriate shiny control given a REDCap field
#' 
#' \code{render_redcap} returns a shiny control to represent the REDCap field.
#' 
#' @param dat The REDCap field definition
#' 
#' @return A string containing the version for the CDM that the user's database is most likely running.
render_redcap <- function(dat){
  if(dat$reviewr_redcap_widget_function[1] == 'reviewr_text'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    textInput(inputId = dat$reviewr_inputID[1], label = question, placeholder = dat$text_validation_type_or_show_slider_number[1])
  } else if(dat$reviewr_redcap_widget_function[1] == 'reviewr_date'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    dateInput(inputId = dat$reviewr_inputID[1],label = question)
  } else if(dat$reviewr_redcap_widget_function[1] == 'reviewr_dropdown'){
    temp <- dat %>% 
        unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
        separate(choices, into = c("Values","Names"), sep = ",") %>% 
        mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    dropdown_choices <- temp$Values
    names(dropdown_choices) <- temp$Names
    selectInput(inputId = dat$reviewr_inputID[1],label = question, choices = dropdown_choices)
  } else if(dat$reviewr_redcap_widget_function[1] == 'reviewr_truefalse'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    radio_choices <- c(1, 0)
    names(radio_choices) <- c('True','False')
    radioButtons(inputId = dat$reviewr_inputID[1],label = question,choices = radio_choices)
  } else if(dat$reviewr_redcap_widget_function[1] == 'reviewr_yesno') {
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    radio_choices <- c(1, 0)
    names(radio_choices) <- c('Yes','No')
    radioButtons(inputId = dat$reviewr_inputID[1],label = question,choices = radio_choices)
  } else if(dat$reviewr_redcap_widget_function[1] == 'reviewr_radio'){
    temp <- dat %>% 
      unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
      separate(choices, into = c("Values","Names"), sep = ",") %>% 
      mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    radio_choices <- temp$Values
    names(radio_choices) <- temp$Names
    radioButtons(inputId = dat$reviewr_inputID[1],label = question, choices = radio_choices)
  } else if(dat$reviewr_redcap_widget_function[1] == 'reviewr_checkbox'){
    temp <- dat %>% 
      unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
      separate(choices, into = c("Values","Names"), sep = ",") %>% 
      mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    checkbox_choices <- temp$Values
    names(checkbox_choices) <- temp$Names
    checkboxGroupInput(inputId = dat$reviewr_inputID,label = question, choices = checkbox_choices)
  } else if(dat$reviewr_redcap_widget_function[1] == 'reviewr_notes'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    textAreaInput(inputId = dat$reviewr_inputID[1], label = question, placeholder = dat$text_validation_type_or_show_slider_number[1],rows = 5,resize = 'vertical')
  } else if(dat$reviewr_redcap_widget_function[1] == 'reviewr_integer'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    numericInput(inputId = dat$reviewr_inputID[1], label = question,value = 42, min = dat$text_validation_min[1], max = dat$text_validation_max[1])
  } else {textInput(inputId = dat$reviewr_inputID[1],label = "This is an unsupported field type",placeholder = dat$field_type[1])}
}