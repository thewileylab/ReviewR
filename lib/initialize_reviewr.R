#' Initialize ReviewR
#' Run this once at the outset to prep for manual review. Sources all libraries, libs, and functions needed to run the application.
#' @return supported data models
#'
#' @examples
initialize_reviewr <- function() {
  message('Clear the mechanism... initializing ReviewR!')
  options(shiny.port = 8100)
  options(shiny.reactlog=TRUE)
  
  # Check that Packages are installed
  message('Checking available packages, installing those that are missing...')
  source('lib/check_packages.R')
  check_packages(pkg = list('shiny',
                            'shinydashboard',
                            'shinycssloaders',
                            'shinyjqui',
                            'shinyjs',
                            'tidyverse',
                            'tibble',
                            'lubridate',
                            'magrittr',
                            'bigrquery',
                            'httr',
                            'dbplyr',
                            'DBI',
                            'DT',
                            'snakecase',
                            'redcapAPI')
                 )
  # Load All necessary libraries
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyjqui)
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
  
  # Load Supported data models
  ## This isn't a large dataset by any means, but it can be made available across all R sessions upon App initialization. 
  message('Loading supported data models...')
  suppressMessages(
  supported_models <- list.files(path = file.path('data_models/'),full.names = T,recursive = T) %>% 
    tibble(file_path = .) %>% 
    mutate(data_model = str_extract(string = file_path, pattern = regex('(mimic3)|(omop)',ignore_case = T)),
           data_model = tolower(data_model),
           model_version = basename(file_path),
           model_version = str_replace(string = model_version, pattern = regex(pattern = '(mimic3)(_)?|(omop_cdm_)',ignore_case = T),replacement = ''),
           model_version = str_replace(string = model_version, pattern = regex(pattern = '.csv',ignore_case = T),replacement = ''),
           model_version = tolower(x = model_version),
           cdm = map(.x = file_path,.f = read_csv)
    ) %>% 
    ## Process the supported data models slightly, turning table names and fields to lowercase. Re-group.
    unnest() %>% 
    mutate(table = tolower(table),
           field = tolower(field)) %>% 
    group_by(file_path,data_model,model_version) %>% 
    nest(.key = cdm)
  )
  data_model_message <- paste('Discovered and imported', nrow(supported_models), 'supported data model versions.')
  message(data_model_message)
  message('Proceeding to manual record review!')
  return(supported_models)
}