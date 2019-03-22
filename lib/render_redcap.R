library(shiny)
library(redcapAPI)

#' Creates the appropriate shiny control given a REDCap field
#' 
#' \code{render_redcap} returns a shiny control to represent the REDCap field.
#' 
#' @param redcap_field The REDCap field definition
#' 
#' @return A string containing the version for the CDM that the user's database is most likely running.
render_redcap <- function(redcap_field, current_subject_data, other_default_data){
  if(redcap_field$reviewr_redcap_widget_function[1] == 'reviewr_text'){
    question <- ifelse(is.na(redcap_field$required_field[1]) == T,paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1]),paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1],' *'))
    field_note <- ifelse(is.na(redcap_field$field_note) !=T, redcap_field$field_note[1],'')
    default_value <- ifelse(nrow(current_subject_data) == 1,
                            current_subject_data %>% select(!!as.name(redcap_field$field_name[1])) %>% unnest(),
                            ifelse(is.null(other_default_data) == FALSE && (redcap_field$field_name %in% names(other_default_data)), 
                                   other_default_data %>% select(!!as.name(redcap_field$field_name[1])) %>% unnest(), ''))
    renderUI({tagList(
      textInput(inputId = redcap_field$reviewr_inputID[1], label = question, placeholder = redcap_field$text_validation_type_or_show_slider_number[1], value = default_value),
      renderUI({div(field_note, style = "color: grey;font-style: italic;font-size:.80em;")}))})
  } else if(redcap_field$reviewr_redcap_widget_function[1] == 'reviewr_date'){
    question <- ifelse(is.na(redcap_field$required_field[1]) == T,paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1]),paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1],' *'))
    field_note <- ifelse(is.na(redcap_field$field_note) !=T, redcap_field$field_note[1],'')
    default_value <- ifelse(nrow(current_subject_data) == 1,
                            format(current_subject_data %>% select(!!as.name(redcap_field$field_name[1])) %>% unnest() %>% pluck(1)), '')
    renderUI({tagList(
      dateInput(inputId = redcap_field$reviewr_inputID[1],label = question, value = default_value),
      renderUI({div(field_note, style = "color: grey;font-style: italic;font-size:.80em;")}))})
  } else if(redcap_field$reviewr_redcap_widget_function[1] == 'reviewr_dropdown'){
    temp <- redcap_field %>% 
      unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
      separate(choices, into = c("Values","Names"), sep = ",") %>% 
      mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(redcap_field$required_field[1]) == T,paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1]),paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1],' *'))
    dropdown_choices <- temp$Values
    names(dropdown_choices) <- temp$Names
    dropdown_choices = append("",dropdown_choices)
    default_value <- ifelse(nrow(current_subject_data) == 1,
                            current_subject_data %>% select(!!as.name(redcap_field$field_name[1])) %>% unnest() %>% pluck(1), '')
    field_note <- ifelse(is.na(redcap_field$field_note) !=T, redcap_field$field_note[1],'')
    renderUI({tagList(
      selectInput(inputId = redcap_field$reviewr_inputID[1],label = question, choices = dropdown_choices, selected = default_value),
      renderUI({div(field_note, style = "color: grey;font-style: italic;font-size:.80em;")}))})
  } else if(redcap_field$reviewr_redcap_widget_function[1] == 'reviewr_truefalse'){
    question <- ifelse(is.na(redcap_field$required_field[1]) == T,paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1]),paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1],' *'))
    radio_choices <- c(1, 0)
    names(radio_choices) <- c('True','False')
    field_note <- ifelse(is.na(redcap_field$field_note) !=T, redcap_field$field_note[1],'')
    default_value <- ifelse(nrow(current_subject_data) == 1,
                            ifelse(current_subject_data %>% select(!!as.name(redcap_field$field_name[1])) %>% unnest() %>% pluck(1), 1, 0), '')
    renderUI({tagList(
      radioButtons(inputId = redcap_field$reviewr_inputID[1],label = question,choices = radio_choices, selected = default_value),
      renderUI({div(field_note, style = "color: grey;font-style: italic;font-size:.80em;")}))})
  } else if(redcap_field$reviewr_redcap_widget_function[1] == 'reviewr_yesno') {
    question <- ifelse(is.na(redcap_field$required_field[1]) == T,paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1]),paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1],' *'))
    radio_choices <- c(1, 0)
    names(radio_choices) <- c('Yes','No')
    field_note <- ifelse(is.na(redcap_field$field_note) !=T, redcap_field$field_note[1],'')
    default_value <- ifelse(nrow(current_subject_data) == 1,
                            ifelse(current_subject_data %>% select(!!as.name(redcap_field$field_name[1])) %>% unnest() %>% pluck(1) == 'Yes', 1, 0), '')
    renderUI({tagList(
      radioButtons(inputId = redcap_field$reviewr_inputID[1],label = question,choices = radio_choices, selected = default_value),
      renderUI({div(field_note, style = "color: grey;font-style: italic;font-size:.80em;")}))})
  } else if(redcap_field$reviewr_redcap_widget_function[1] == 'reviewr_radio'){
    temp <- redcap_field %>% 
      unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
      separate(choices, into = c("Values","Names"), sep = ",") %>% 
      mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(redcap_field$required_field[1]) == T,paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1]),paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1],' *'))
    radio_choices <- temp$Values
    names(radio_choices) <- temp$Names
    field_note <- ifelse(is.na(redcap_field$field_note) !=T, redcap_field$field_note[1],'')
    default_value <- ifelse(nrow(current_subject_data) == 1,
                            current_subject_data %>% select(!!as.name(redcap_field$field_name[1])) %>% unnest() %>% pluck(1), '')
    renderUI({tagList(
      radioButtons(inputId = redcap_field$reviewr_inputID[1],label = question, choices = radio_choices, selected = default_value),
      renderUI({div(field_note, style = "color: grey;font-style: italic;font-size:.80em;")}))})
  } else if(redcap_field$reviewr_redcap_widget_function[1] == 'reviewr_checkbox'){
    temp <- redcap_field %>% 
      unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
      separate(choices, into = c("Values","Names"), sep = ",") %>% 
      mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(redcap_field$required_field[1]) == T,paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1]),paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1],' *'))
    checkbox_choices <- temp$Values
    names(checkbox_choices) <- temp$Names
    field_note <- ifelse(is.na(redcap_field$field_note) !=T, redcap_field$field_note[1],'')
    if (nrow(current_subject_data) == 1) {
      checked_boxes = current_subject_data %>%
                      select(contains("checkbox_test")) %>%
                      gather() %>%
                      separate(key, c("name", "index"), sep="___") %>%
                      filter(value == 'Checked') %>%
                      select(index)
    }
    else {
      checked_boxes <- character(0)
    }
    renderUI({tagList(
      checkboxGroupInput(inputId = redcap_field$reviewr_inputID,label = question, choices = checkbox_choices, selected = unlist(checked_boxes)),
      renderUI({div(field_note, style = "color: grey;font-style: italic;font-size:.80em;")}))})
  } else if(redcap_field$reviewr_redcap_widget_function[1] == 'reviewr_notes'){
    question <- ifelse(is.na(redcap_field$required_field[1]) == T,paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1]),paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1],' *'))
    field_note <- ifelse(is.na(redcap_field$field_note) !=T, redcap_field$field_note[1],'')
    default_value <- ifelse(nrow(current_subject_data) == 1,
                            current_subject_data %>% select(!!as.name(redcap_field$field_name[1])) %>% unnest(), '')
    renderUI({tagList(
      textAreaInput(inputId = redcap_field$reviewr_inputID[1], label = question, placeholder = redcap_field$text_validation_type_or_show_slider_number[1],rows = 5,resize = 'vertical', value = default_value),
      renderUI({div(field_note, style = "color: grey;font-style: italic;font-size:.80em;")}))})
  } else if(redcap_field$reviewr_redcap_widget_function[1] == 'reviewr_integer'){
    question <- ifelse(is.na(redcap_field$required_field[1]) == T,paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1]),paste0(as.numeric(redcap_field$rowname[1]),') ',redcap_field$field_label[1],' *'))
    field_note <- ifelse(is.na(redcap_field$field_note) !=T, redcap_field$field_note[1],'')
    default_value <- ifelse(nrow(current_subject_data) == 1,
                            current_subject_data %>% select(!!as.numeric(as.names(redcap_field$field_name[1]))) %>% unnest(), '')
    renderUI({tagList(
      numericInput(inputId = redcap_field$reviewr_inputID[1], label = question, min = redcap_field$text_validation_min[1], max = redcap_field$text_validation_max[1], value = default_value),
      renderUI({div(field_note, style = "color: grey;font-style: italic;font-size:.80em;")}))})
  } else {textInput(inputId = redcap_field$reviewr_inputID[1],label = "This is an unsupported field type",placeholder = redcap_field$field_type[1])}
}