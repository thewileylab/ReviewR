# Datasets ----
#' ReviewR Database Module Template
#'
#' A character vector containing a database module template
#' 
#' @docType data
#'
#' @format A character vector with 51 elements
"db_module_template"

#' ReviewR Database Function: All Patients Table Template
#'
#' A character vector containing a function template for creating the 
#' 'All Patients' table as displayed on the "Patient Search" Tab
#' 
#' @docType data
#'
#' @format A character vector with 21 elements
"db_function_all_patients_table_template"

#' ReviewR Database Function: Subject Table Template
#'
#' A character vector containing a function template for creating the 
#' 'Subject Filtered' tables as displayed on the "Chart Review" Tab
#'  
#' @docType data
#'
#' @format A character vector with 15 elements
"db_function_subject_table_template"

# Dev Functions ----
#' Develop Database Module
#'
#' This function will assist in providing a database module skeleton for ReviewR
#'
#' @param mod_name A string, denoting the module suffix eg: 'mariadb'
#' @param display_name A string, denoting the module display name eg: 'MariaDB'
#'
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map
#' @importFrom rstudioapi navigateToFile
dev_database_module <- function(mod_name = NULL, display_name = NULL) {
  if(is.null(mod_name) | is.null(display_name)) {
    message('mod_name and display_name are required arguments.')
    } else {
      filename <- glue::glue('R/mod_{mod_name}.R')
      cat(glue::glue_collapse(x = map(ReviewR::db_module_template,
                                      ~glue::glue(.x)),
                              sep = '\n' 
                              ),
          file = filename
          ) 
          rstudioapi::navigateToFile( filename )
      }
}

#' Develop Datamodel Functions
#' 
#' This function will assist in adding a new supported datamodel to 
#' ReviewR. A CDM file, stored as a CSV will be added to the 
#' namespace, such that the CDM can be identified by the datamodel
#' detection module. Additionally, a database_tables.R file will 
#' be created and opened in R/ with basic table skeletons created
#' based on the CDM stored in the user supplied CSV.
#'
#' @param csv 
#' @param all_patients_table 
#' @param patient_identifier_field 
#' 
#' @importFrom dplyr distinct filter mutate pull
#' @importFrom glue glue glue_collapse
#' @importFrom magrittr %>% extract2
#' @importFrom purrr map imap
#' @importFrom stringr str_remove_all str_split
#' @importFrom tibble enframe
#' @importFrom tidyr replace_na separate
#' @importFrom rlang .data
dev_add_datamodel <- function(csv, all_patients_table, patient_identifier_field) {
  ## Add user supplied CSV file to package and incorporate into ReviewR::supported_datamodels.rda
  file.copy(from = csv, to = 'data-raw/datamodels/')
  supported_datamodels <- list.files(path = file.path('data-raw/datamodels'),full.names = T,recursive = T) %>%
    tibble::enframe(name = NULL, value = 'file_path') %>% 
    mutate(datamodel = basename(.data$file_path),
           datamodel = str_remove_all(.data$datamodel, '.csv')
           ) %>% 
    separate(col = .data$datamodel, into = c('datamodel','model_version'), sep = '_', extra = 'drop', fill = 'right') %>% 
    mutate(model_version = tidyr::replace_na(.data$model_version, ''),
           cdm = map(.data$file_path,
                     ~read_csv(.x)
                     )
           ) %>% 
    unnest(cols = .data$cdm) %>% 
    mutate(joinable_table = tolower(.data$table),
           joinable_field = tolower(.data$field)
           ) %>% 
    group_by(.data$file_path,.data$datamodel,.data$model_version) %>% 
    nest()
  # usethis::use_data(supported_datamodels, overwrite = T)
  
  ## Determine Datamodel moniker and version
  temp_datamodel <-basename(csv) %>%
    str_remove_all('.csv') %>% 
    str_split(pattern = '_')
  new_datamodel <- temp_datamodel[[1]][1]
  new_datamodel_version <- temp_datamodel[[1]][2]
  
  ## Create a filename to hold datamodel table functions
  fn_filename <- glue::glue('R/database_tables_{new_datamodel}.R')
  
  ## Discover Subject Tables that should potentially be rendered
  new_tables <- supported_datamodels %>% 
    filter(.data$datamodel == new_datamodel & .data$model_version == new_datamodel_version ) %>% 
    pull(.data$data) %>% 
    magrittr::extract2(1) %>% 
    distinct(.data$table) %>% 
    filter(.data$table != all_patients_table) %>% 
    pull(.data$table)
  
  ## Create All Patients Table from template
  cat(glue::glue_collapse(x = map(ReviewR::db_function_all_patients_table_template,
                                  ~glue::glue(.x)
                                  ),
                          sep = '\n'),
      file = fn_filename
      )
  
  ## Subject Tables
  subject_tables <- imap(new_tables,
                         ~{new_table <- new_tables[.y]
                         glue::glue_collapse(x = map(ReviewR::db_function_subject_table_template,
                                                     ~glue::glue(.x)
                                                     ),
                                             sep = '\n'
                                             )
                         }
                         )
  map(subject_tables,
      ~cat(.x, file = fn_filename, append = T))
  ## Open the file for editing
  rstudioapi::navigateToFile( fn_filename )
}
