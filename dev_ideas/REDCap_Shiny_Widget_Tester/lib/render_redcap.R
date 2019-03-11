render_redcap <- function(dat){
  
  if(dat$reviewr_function[1] == 'reviewr_text'){
    question <- ifelse(is.na(dat$required_field) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    textInput(inputId = dat$field_name, label = question, placeholder = dat$text_validation_type_or_show_slider_number)
  } else if(dat$reviewr_function[1] == 'reviewr_date'){
    question <- ifelse(is.na(dat$required_field) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    dateInput(inputId = dat$field_name,label = question)
  } else if(dat$reviewr_function[1] == 'reviewr_dropdown'){
    temp <- dat %>% 
        unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
        separate(choices, into = c("Values","Names"), sep = ",") %>% 
        mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(dat$required_field) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    dropdown_choices <- temp$Values
    names(dropdown_choices) <- temp$Names
    selectInput(inputId = dat$field_name,label = question, choices = dropdown_choices)
  } else if(dat$reviewr_function[1] == 'reviewr_truefalse'){
    question <- ifelse(is.na(dat$required_field) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    radio_choices <- c(1, 0)
    names(radio_choices) <- c('True','False')
    radioButtons(inputId = dat$field_name,label = question,choices = radio_choices)
  } else if(dat$reviewr_function[1] == 'reviewr_yesno') {
    question <- ifelse(is.na(dat$required_field) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    radio_choices <- c(1, 0)
    names(radio_choices) <- c('Yes','No')
    radioButtons(inputId = dat$field_name,label = question,choices = radio_choices)
  } else if(dat$reviewr_function[1] == 'reviewr_radio'){
    temp <- dat %>% 
      unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
      separate(choices, into = c("Values","Names"), sep = ",") %>% 
      mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(dat$required_field) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    dropdown_choices <- temp$Values
    names(dropdown_choices) <- temp$Names
    radioButtons(inputId = dat$field_name,label = question, choices = dropdown_choices)
  } else if(dat$reviewr_function[1] == 'reviewr_checkbox'){
    temp <- dat %>% 
      unnest(choices = str_split(select_choices_or_calculations, "\\|")) %>% 
      separate(choices, into = c("Values","Names"), sep = ",") %>% 
      mutate_at(vars("Values":"Names"), str_trim)
    question <- ifelse(is.na(dat$required_field) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    dropdown_choices <- temp$Values
    names(dropdown_choices) <- temp$Names
    checkboxGroupInput(inputId = dat$field_name,label = question, choices = dropdown_choices)
  } else if(dat$reviewr_function[1] == 'reviewr_notes'){
    question <- ifelse(is.na(dat$required_field) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    textAreaInput(inputId = dat$field_name, label = question, placeholder = dat$text_validation_type_or_show_slider_number,rows = 5,resize = 'vertical')
  } else if(dat$reviewr_function[1] == 'reviewr_integer'){
    question <- ifelse(is.na(dat$required_field) == T,dat$field_label[1],paste(dat$field_label[1],'*'))
    numericInput(inputId = dat$field_name, label = question,value = 42, min = dat$text_validation_min,max = dat$text_validation_max)
  } else {textInput(inputId = 'one',label = "This is an unsupported field type",placeholder = dat$field_type)}
}