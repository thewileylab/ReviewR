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

# Helper Functions ----
#' DT to Viewer
#'
#' Save a DT::datatable as a self contained html file to display as a prompt mid function
#'
#' @param .data A tibble/data frame containing the desired data to save
#' @param file File location with extension
#'
#' @keywords internal
#' @importFrom dplyr select
#' @importFrom DT datatable saveWidget
#' @importFrom rlang .data
#' @return HTML file
#'
dt_2_viewer <- function(.data, file = NULL) {
  if(is.null(file) ) {
    temp_file <- tempfile(fileext = '.html')
    .data %>%
      DT::datatable(rownames = F) %>%
      DT::saveWidget(file = temp_file, selfcontained = T)
    viewer <- getOption('viewer')
    viewer(temp_file)
  } else {
    .data %>%
      DT::datatable(rownames = F) %>%
      DT::saveWidget(file = file, selfcontained = T)
    viewer <- getOption('viewer')
    viewer(file)
  }
}

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
#' @param csv The file path of a CSV file containing a datamodel CDM
#' 
#' @importFrom dplyr distinct filter mutate pull
#' @importFrom glue glue glue_collapse
#' @importFrom magrittr %>% extract2
#' @importFrom purrr map imap
#' @importFrom stringr str_remove_all str_split
#' @importFrom tibble enframe
#' @importFrom tidyr replace_na nest separate
#' @importFrom readr read_csv
#' @importFrom rlang .data names2
#' @importFrom rstudioapi navigateToFile
dev_add_datamodel <- function(csv) {
  ## Validate CSV ----
  ### Define Required Columns
  required_cols <- c('table','field')
  ### Read User CSV
  temp <- readr::read_csv(file = csv)

  ## If required columns are present, Add user supplied CSV file to package and 
  ## incorporate into ReviewR::supported_datamodels.rda
  if(all(required_cols %in% rlang::names2(temp)) ) {
    file.copy(from = csv, to = 'data-raw/datamodels/')
    supported_datamodels <- list.files(path = file.path('data-raw/datamodels'),full.names = T,recursive = T) %>%
      tibble::enframe(name = NULL, value = 'file_path') %>% 
      mutate(datamodel = basename(.data$file_path),
             datamodel = str_remove_all(.data$datamodel, '\\.csv$')
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
    usethis::use_data(supported_datamodels, overwrite = T)
  
    ## Determine Datamodel moniker and version
    temp_datamodel <-basename(csv) %>%
      str_remove_all('\\.csv$') %>% 
      str_split(pattern = '_')
    new_datamodel <- temp_datamodel[[1]][1]
    new_datamodel_version <- if(is.na(temp_datamodel[[1]][2]) ){
      ''
      } else {
        temp_datamodel[[1]][2]
        }
    
    ## Create a filename to hold datamodel table functions
    fn_filename <- glue::glue('R/database_tables_{new_datamodel}.R')
    
    ## Interview the User ----
    ### All Patients Table
    table_choices <- temp %>% 
      dplyr::distinct(.data$table) %>% 
      tibble::rownames_to_column(var = 'Selection') %>% 
      mutate(Selection = as.numeric(.data$Selection))
    table_choices %>% 
      ReviewR:::dt_2_viewer()
    all_patients_selection <- -1
    while(all_patients_selection == -1 | {all_patients_selection > 0 & all_patients_selection < min(table_choices$Selection)} | all_patients_selection > max(table_choices$Selection) ) {
      table_question <- if(all_patients_selection == -1) {
        glue::glue('Please identify which table contains a listing of all patients from the choices in the Viewer pane and enter your selection {min(table_choices$Selection)}-{max(table_choices$Selection)}: ')
        } else {
          glue::glue('Please enter an integer {min(table_choices$Selection)}-{max(table_choices$Selection)}, or 0 to skip: ')
          }
      all_patients_selection <- readline(prompt = table_question)
      all_patients_selection <- round(as.numeric(all_patients_selection), digits = 0)
      }
    
    ### Patient Identifier field
    field_choices <- temp %>% 
      dplyr::distinct(.data$field) %>% 
      tibble::rownames_to_column(var = 'Selection') %>% 
      mutate(Selection = as.numeric(.data$Selection))
    field_choices %>% 
      ReviewR:::dt_2_viewer()
    patient_identifier_field_selection <- -1
    while(patient_identifier_field_selection == -1 | {patient_identifier_field_selection > 0 & patient_identifier_field_selection < min(field_choices$Selection)} | patient_identifier_field_selection > max(field_choices$Selection) ) {
      field_question <- if(patient_identifier_field_selection == -1){
        glue::glue('Please identify which field contains the patient identifier from the choices in the Viewer pane and enter your selection {min(field_choices$Selection)}-{max(field_choices$Selection)}: ')
        } else {
          glue::glue('Please enter an integer {min(field_choices$Selection)}-{max(field_choices$Selection)}, or 0 to quit: ')
          }
      patient_identifier_field_selection <- readline(prompt = field_question)
      patient_identifier_field_selection <- round(as.numeric(patient_identifier_field_selection), digits = 0)
      }
    ## Create function skeletons ---- 
    if (all_patients_selection == 0 | patient_identifier_field_selection == 0) {
      stop('Selection of an all patients table and a patient identifier field is required. ')
      } else {
        all_patients_table <- table_choices %>% 
          filter(.data$Selection == all_patients_selection) %>% 
          pull(.data$table)
        patient_identifier_field <- field_choices %>% 
          filter(.data$Selection == patient_identifier_field_selection) %>% 
          pull(.data$field)
        
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
        
        ## Create Subject Tables from template
        subject_tables <- imap(new_tables,
                               ~{new_table <- new_tables[[.y]]
                               glue::glue_collapse(x = map(ReviewR::db_function_subject_table_template,
                                                           ~glue::glue(.x)
                                                           ),
                                                   sep = '\n'
                                                   )
                               }
                               )
        ### Append Subject Tables to datamodel_tables R file
        map(subject_tables,
            ~cat(.x, file = fn_filename, append = T))
        
        ## Open the file for editing
        rstudioapi::navigateToFile( fn_filename )
        }
    } else {
      message('Warning: Did not find "table" or "field" columns in specified CSV. Please ensure these fields are present, or specify a different CSV file.')
    }
}
