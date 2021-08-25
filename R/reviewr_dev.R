# Datasets ----
#' Database Module Template
#'
#' A character vector containing a database module template
#' 
#' @docType data
#' @family Development Templates
#'
#' @format A character vector with 52 elements
"db_module_template"

#' Database Table Function: All Patients Table Template
#'
#' A character vector containing a function template for creating the 
#' 'All Patients' table as displayed on the "Patient Search" Tab
#' 
#' @docType data
#' @family Development Templates
#'
#' @format A character vector with 22 elements
"db_function_all_patients_table_template"

#' Database Table Function: Subject Table Template
#'
#' A character vector containing a function template for creating the 
#' 'Subject Filtered' tables as displayed on the "Chart Review" Tab
#'  
#' @docType data
#' @family Development Templates
#'
#' @format A character vector with 15 elements
"db_function_subject_table_template"

# Helper Functions ----
#' DT to Viewer
#'
#' Save a temporary [DT::datatable] as a self contained HTML file to display 
#' in the RStudio Viewer Pane. Used to provided users with choices
#' when prompted for action by a dev function.
#'
#' @param .data A [dplyr::tibble] containing the desired data to save
#' @param file \emph{Optional}. Manually define file path (with .html extension) 
#' for HTML representation of DT
#'
#' @keywords internal
#' @importFrom dplyr select
#' @importFrom DT datatable saveWidget
#' @importFrom rlang .data
#' 
#' @return This function returns a temporary HTML file displayed in the 
#' RStudio Viewer Pane
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
#' Develop A Database Module
#'
#' This function will create a database module skeleton with 
#' required elements already populated, based on user inputs.
#' Common database module packages are imported automatically,
#' but developers should add imports to the roxygen skeleton as 
#' necessary to both the UI and server functions to collect 
#' user info and create a DBI connection object, respectively. 
#'
#' @param mod_name \emph{Required}. A string, denoting the module suffix eg: 'mariadb'
#' @param display_name \emph{Required}. A string, denoting the module display name eg: 'MariaDB Server'. 
#' This is the 'user viewable' name that will appear in the database module selector dropdown. 
#' 
#' @family Development Functions
#'
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map
#' 
#' @return A .R file populated with a database module skeleton
dev_add_database_module <- function(mod_name = NULL, display_name = NULL) {
  if(!requireNamespace('rstudioapi', quietly = T)) {
    stop("'rstudioapi' package is required for this function to work. Please install it.",
         call. = FALSE)
    }
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

#' Develop Data Model Table Functions
#' 
#' This function will assist in adding a new supported data model to 
#' ReviewR. A schema file, stored as a CSV will be added to the 
#' namespace, such that the database can be identified by the data model
#' detection module. Additionally, a database_tables.R file will 
#' be created and opened in R/ with basic table skeletons created
#' based on the schema stored in the user supplied CSV.
#'
#' @param csv \emph{Required}. The file path of a CSV file containing a data model schema
#' 
#' @family Development Functions
#' 
#' @importFrom dplyr distinct filter mutate pull relocate row_number tibble
#' @importFrom glue glue glue_collapse
#' @importFrom magrittr %>% extract2
#' @importFrom purrr map imap
#' @importFrom stringr str_remove_all str_split
#' @importFrom tidyr replace_na nest separate
#' @importFrom rlang .data names2
#' 
#' @return A .R file populated with basic database table functions
dev_add_data_model <- function(csv) {
  if(!requireNamespace('readr', quietly = T)) {
    stop("'readr' package is required for this function to work. Please install it.",
         call. = FALSE)
    }
  if(!requireNamespace('rstudioapi', quietly = T)) {
    stop("'rstudioapi' package is required for this function to work. Please install it.",
         call. = FALSE)
    }
  if(!requireNamespace('usethis', quietly = T)) {
    stop("'usethis' package is required for this function to work. Please install it.",
         call. = FALSE)
    }
  ## Validate CSV ----
  ### Define Required Columns
  required_cols <- c('table','field')
  ### Read User CSV
  temp <- readr::read_csv(file = csv, show_col_types = FALSE)

  ## If required columns are present, Add user supplied CSV file to package and 
  ## incorporate into ReviewR::supported_data_models.rda
  if(all(required_cols %in% rlang::names2(temp)) ) {
    file.copy(from = csv, to = 'data-raw/data_models/')
    supported_data_models <-  dplyr::tibble(file_path = list.files(path = file.path('data-raw/data_models'),full.names = T,recursive = T)) %>% 
      mutate(data_model = basename(.data$file_path),
             data_model = str_remove_all(.data$data_model, '\\.csv$')
             ) %>% 
      separate(col = .data$data_model, into = c('data_model','model_version'), sep = '_', extra = 'drop', fill = 'right') %>% 
      mutate(model_version = tidyr::replace_na(.data$model_version, ''),
             cdm = map(.data$file_path,
                       ~readr::read_csv(.x, show_col_types = FALSE)
                       )
             ) %>% 
      unnest(cols = .data$cdm) %>% 
      mutate(joinable_table = tolower(.data$table),
             joinable_field = tolower(.data$field)
             ) %>% 
      group_by(.data$file_path,.data$data_model,.data$model_version) %>% 
      nest() %>% 
      relocate(.data$data_model, .data$model_version, .data$data, .data$file_path)
    usethis::use_data(supported_data_models, overwrite = T)
  
    ## Determine data model moniker and version
    temp_data_model <-basename(csv) %>%
      str_remove_all('\\.csv$') %>% 
      str_split(pattern = '_')
    new_data_model <- temp_data_model[[1]][1]
    new_data_model_version <- if(is.na(temp_data_model[[1]][2]) ) {
      ''
      } else {
        temp_data_model[[1]][2]
        }
    
    ## Create a filename to hold data model table functions
    fn_filename <- glue::glue('R/database_tables_{new_data_model}.R')
    
    ## Interview the User ----
    ### All Patients Table
    table_choices <- temp %>% 
      dplyr::distinct(.data$table) %>% 
      mutate(Selection = row_number()) %>% 
      mutate(Selection = as.numeric(.data$Selection))
    table_choices %>% 
      dt_2_viewer()
    all_patients_selection <- -1.1
    while(all_patients_selection <= -1 | {all_patients_selection > 0 & all_patients_selection < min(table_choices$Selection)} | all_patients_selection > max(table_choices$Selection) ) {
      table_question <- if(all_patients_selection == -1.1) {
        glue::glue('Please identify which table contains a listing of all patients from the choices in the Viewer pane and enter your selection {min(table_choices$Selection)}-{max(table_choices$Selection)}: ')
        } else if (all_patients_selection == -1.2) {
          glue::glue('Only integer values {min(table_choices$Selection)}-{max(table_choices$Selection)} are allowed, or 0 to skip: ')
          } else {
            glue::glue('Please enter an integer {min(table_choices$Selection)}-{max(table_choices$Selection)}, or 0 to skip: ')
            }
      all_patients_selection <- readline(prompt = table_question)
      if(!is.na(suppressWarnings(as.numeric(all_patients_selection))) ) {
        all_patients_selection <- round(as.numeric(all_patients_selection), digits = 0)
        } else {
          all_patients_selection <- -1.2
          }
      }
    ### Patient Identifier field
    field_choices <- temp %>% 
      dplyr::distinct(.data$field) %>% 
      mutate(Selection = row_number()) %>% 
      mutate(Selection = as.numeric(.data$Selection))
    field_choices %>% 
      dt_2_viewer()
    patient_identifier_field_selection <- -1.1
    while(patient_identifier_field_selection <= -1 | {patient_identifier_field_selection > 0 & patient_identifier_field_selection < min(field_choices$Selection)} | patient_identifier_field_selection > max(field_choices$Selection) ) {
      field_question <- if(patient_identifier_field_selection == -1.1){
        glue::glue('Please identify which field contains the patient identifier from the choices in the Viewer pane and enter your selection {min(field_choices$Selection)}-{max(field_choices$Selection)}: ')
        } else if(patient_identifier_field_selection == -1.2) {
          glue::glue('Only integer values {min(field_choices$Selection)}-{max(field_choices$Selection)} are allowed, or 0 to quit: ')
          } else {
            glue::glue('Please enter an integer {min(field_choices$Selection)}-{max(field_choices$Selection)}, or 0 to quit: ')
            }
      patient_identifier_field_selection <- readline(prompt = field_question)
      if(!is.na(suppressWarnings(as.numeric(patient_identifier_field_selection))) ) {
        patient_identifier_field_selection <- round(as.numeric(patient_identifier_field_selection), digits = 0)
        } else {
          patient_identifier_field_selection <- -1.2
          }
      }
    ## Create Table Function Skeletons ---- 
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
        new_tables <- supported_data_models %>% 
          filter(.data$data_model == new_data_model & .data$model_version == new_data_model_version ) %>% 
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
        ### Append Subject Tables to data_model_tables R file
        map(subject_tables,
            ~cat(.x, file = fn_filename, append = T))
        
        ## Open the file for editing
        rstudioapi::navigateToFile( fn_filename )
        }
    } else {
      message('Warning: Did not find "table" or "field" columns in specified CSV. Please ensure these fields are present, or specify a different CSV file.')
    }
}
