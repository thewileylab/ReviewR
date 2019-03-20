render_redcap <- function(dat){
  
  if(dat$reviewr_function[1] == 'reviewr_text'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    field_note <- ifelse(is.na(dat$field_note) !=T, dat$field_note[1],'')
    renderUI({tagList(
    textInput(inputId = dat$reviewr_inputID[1], label = question, placeholder = dat$text_validation_type_or_show_slider_number[1]),
    renderText(expr = field_note))})
  } else if(dat$reviewr_function[1] == 'reviewr_date'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    field_note <- ifelse(is.na(dat$field_note) !=T, dat$field_note[1],'')
    renderUI({tagList(
    dateInput(inputId = dat$reviewr_inputID[1],label = question),
    renderText(expr = field_note))})
  } else if(dat$reviewr_function[1] == 'reviewr_dropdown'){
    temp <- dat %>% 
        unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
        separate(choices, into = c("Values","Names"), sep = ",") %>% 
        mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    dropdown_choices <- temp$Values
    names(dropdown_choices) <- temp$Names
    field_note <- ifelse(is.na(dat$field_note) !=T, dat$field_note[1],'')
    renderUI({tagList(
    selectInput(inputId = dat$reviewr_inputID[1],label = question, choices = dropdown_choices),
    renderText(expr = field_note))})
  } else if(dat$reviewr_function[1] == 'reviewr_truefalse'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    radio_choices <- c(1, 0)
    names(radio_choices) <- c('True','False')
    field_note <- ifelse(is.na(dat$field_note) !=T, dat$field_note[1],'')
    renderUI({tagList(
    radioButtons(inputId = dat$reviewr_inputID[1],label = question,choices = radio_choices),
    renderText(expr = field_note))})
  } else if(dat$reviewr_function[1] == 'reviewr_yesno') {
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    radio_choices <- c(1, 0)
    names(radio_choices) <- c('Yes','No')
    field_note <- ifelse(is.na(dat$field_note) !=T, dat$field_note[1],'')
    renderUI({tagList(
    radioButtons(inputId = dat$reviewr_inputID[1],label = question,choices = radio_choices),
    renderText(expr = field_note))})
  } else if(dat$reviewr_function[1] == 'reviewr_radio'){
    temp <- dat %>% 
      unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
      separate(choices, into = c("Values","Names"), sep = ",") %>% 
      mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    radio_choices <- temp$Values
    names(radio_choices) <- temp$Names
    field_note <- ifelse(is.na(dat$field_note) !=T, dat$field_note[1],'')
    renderUI({tagList(
    radioButtons(inputId = dat$reviewr_inputID[1],label = question, choices = radio_choices),
    renderText(expr = field_note))})
  } else if(dat$reviewr_function[1] == 'reviewr_checkbox'){
    temp <- dat %>% 
      unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
      separate(choices, into = c("Values","Names"), sep = ",") %>% 
      mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    checkbox_choices <- temp$Values
    names(checkbox_choices) <- temp$Names
    field_note <- ifelse(is.na(dat$field_note) !=T, dat$field_note[1],'')
    renderUI({tagList(
    checkboxGroupInput(inputId = dat$reviewr_inputID,label = question, choices = checkbox_choices),
    renderText(expr = field_note))})
  } else if(dat$reviewr_function[1] == 'reviewr_notes'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    field_note <- ifelse(is.na(dat$field_note) !=T, dat$field_note[1],'')
    renderUI({tagList(
    textAreaInput(inputId = dat$reviewr_inputID[1], label = question, placeholder = dat$text_validation_type_or_show_slider_number[1],rows = 5,resize = 'vertical'),
    renderText(expr = field_note))})
  } else if(dat$reviewr_function[1] == 'reviewr_integer'){
    question <- ifelse(is.na(dat$required_field[1]) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    field_note <- ifelse(is.na(dat$field_note) !=T, dat$field_note[1],'')
    renderUI({tagList(
    numericInput(inputId = dat$reviewr_inputID[1], label = question,value = NULL, min = dat$text_validation_min[1], max = dat$text_validation_max[1]),
    renderText(expr = field_note))})
  } else {textInput(inputId = dat$reviewr_inputID[1],label = "This is an unsupported field type",placeholder = dat$field_type[1])}
}