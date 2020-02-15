## code to prepare `DATASET` dataset goes here

library(tibble)
redcap_field_type <- c('text','text','text','dropdown','truefalse','yesno','radio','checkbox','notes')
redcap_field_val <- c(NA,'date_mdy','integer',NA,NA,NA,NA,NA,NA)
reviewr_redcap_widget_function <- c('reviewr_text','reviewr_date','reviewr_integer','reviewr_dropdown','reviewr_truefalse','reviewr_yesno','reviewr_radio','reviewr_checkbox','reviewr_notes')
redcap_widget_map <- tibble(redcap_field_type, redcap_field_val, reviewr_redcap_widget_function)
usethis::use_data(redcap_widget_map)

# REDCap survey complete choices
redcap_survey_complete_values <- c(0,1,2)
usethis::use_data(redcap_survey_complete_values)

redcap_survey_complete_names <- c('Incomplete', 'Unverified', 'Complete')
names(redcap_survey_complete_values) <-redcap_survey_complete_names
redcap_survey_complete_tbl <- tibble(redcap_survey_complete_names, redcap_survey_complete_values)
usethis::use_data(redcap_survey_complete_tbl)


