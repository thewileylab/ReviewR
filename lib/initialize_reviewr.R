#' Initialize ReviewR
#' Run this once at the outset to prep for manual review. Sources all libraries, libs, and functions needed to run the application.
#' @return supported data models/REDCap widgets
#'
#' @examples
initialize_reviewr <- function() {
  message('Clear the mechanism... initializing ReviewR!')
  options(shiny.port = 8100)
  options(shiny.reactlog=TRUE)
  
  # Check that Packages are installed
  # message('Checking available packages, installing those that are missing...')
  # source('lib/check_packages.R')
  # options(repos=structure(c(CRAN="https://cloud.r-project.org/"))) 
  # check_packages(pkg = list('shiny',
  #                           'shinydashboard',
  #                           'shinycssloaders',
  #                           'shinyjs',
  #                           'tidyverse',
  #                           'tibble',
  #                           'lubridate',
  #                           'magrittr',
  #                           'bigrquery',
  #                           'httr',
  #                           'dbplyr',
  #                           'DBI',
  #                           'DT',
  #                           'snakecase',
  #                           'redcapAPI')
  #                )
  # Load All necessary libraries
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyjs)
  library(tidyverse)
  library(tibble)
  library(lubridate)
  library(magrittr)
  library(bigrquery)
  library(httr)
  library(dbplyr)
  library(DBI)
  library(DT)
  library(snakecase)
  library(redcapAPI)
  
  # Source helper libraries
  # source('lib/img_uri.R')
  
  # message('Done.')
  # Load Supported data models
  ## This isn't a large dataset by any means, but it can be made available across all R sessions upon App initialization. 
  # message('Loading supported data models...')
  # suppressMessages(
  # supported_models <- list.files(path = file.path('data_models/'),full.names = T,recursive = T) %>% 
  #   tibble(file_path = .) %>% 
  #   mutate(data_model = str_extract(string = file_path, pattern = regex('(mimic3)|(omop)',ignore_case = T)),
  #          data_model = tolower(data_model),
  #          model_version = basename(file_path),
  #          model_version = str_replace(string = model_version, pattern = regex(pattern = '(mimic3)(_)?|(omop_cdm_)',ignore_case = T),replacement = ''),
  #          model_version = str_replace(string = model_version, pattern = regex(pattern = '.csv',ignore_case = T),replacement = ''),
  #          model_version = tolower(x = model_version),
  #          cdm = map(.x = file_path,.f = read_csv)
  #   ) %>% 
  #   ## Process the supported data models slightly, turning table names and fields to lowercase. Re-group.
  #   unnest(cols = c(cdm)) %>% 
  #   mutate(table = tolower(table),
  #          field = tolower(field)) %>% 
  #   group_by(file_path,data_model,model_version) %>% 
  #   nest()
  # )
  # data_model_message <- paste('Discovered and imported', nrow(supported_models), 'supported data model versions.')
  # message(data_model_message)
  
  # message('Importing supported REDCap widgets...')
  # # Create widget map - used by REDCap initialization process
  # redcap_field_type <- c('text','text','text','dropdown','truefalse','yesno','radio','checkbox','notes')
  # redcap_field_val <- c(NA,'date_mdy','integer',NA,NA,NA,NA,NA,NA)
  # reviewr_redcap_widget_function <- c('reviewr_text','reviewr_date','reviewr_integer','reviewr_dropdown','reviewr_truefalse','reviewr_yesno','reviewr_radio','reviewr_checkbox','reviewr_notes')
  # redcap_widget_map <- tibble(redcap_field_type, redcap_field_val, reviewr_redcap_widget_function)
  # 
  # # REDCap survey complete choices
  # redcap_survey_complete_values <- c(0,1,2)
  # redcap_survey_complete_names <- c('Incomplete', 'Unverified', 'Complete')
  # names(redcap_survey_complete_values) <-redcap_survey_complete_names
  # redcap_survey_complete_tbl <- tibble(redcap_survey_complete_names, redcap_survey_complete_values)
  # redcap_message <- paste('Discovered and imported', nrow(redcap_widget_map), 'supported REDCap widgets.')
  # 
  # message(redcap_message)
  
  # message('Proceeding to manual record review!')
  # return(list(
  #   # 'supported_models' = supported_models,
  #   'redcap_widget_map' = redcap_widget_map,
  #   'redcap_survey_complete_vals' = redcap_survey_complete_values,
  #   'redcap_survey_complete_tbl' = redcap_survey_complete_tbl))
}