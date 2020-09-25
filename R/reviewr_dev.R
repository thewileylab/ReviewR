# Datasets ----
#' ReviewR Database Module Template
#'
#' A character vector containing a database module template
#' 
#' @docType data
#'
#' @format A character vector with 51 elements
"db_module_template"

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


####

csv <- '~/Desktop/test_v1.4.csv'
all_patients <- 'table_1'
all_patients_table_template <- readLines('data-raw/templates/template_all_patients_table')
subject_table_template <- readLines('data-raw/templates/template_subject_table')

library(tidyverse)
dev_add_datamodel <- function(csv, all_patients) {
  file.copy(from = csv, to = 'data-raw/datamodels/')
  supported_datamodels <- list.files(path = file.path('data-raw/datamodels'),full.names = T,recursive = T) %>%
    tibble::enframe(name = NULL, value = 'file_path') %>% 
    mutate(datamodel = basename(file_path),
           datamodel = str_remove_all(datamodel, '.csv')
    ) %>% 
    separate(col = datamodel, into = c('datamodel','model_version'), sep = '_', extra = 'drop', fill = 'right') %>% 
    mutate(model_version = tidyr::replace_na(model_version, ''),
           cdm = map(file_path,
                     ~read_csv(.x)
           )
    ) %>% 
    unnest(cols = cdm) %>% 
    mutate(joinable_table = tolower(table),
           joinable_field = tolower(field)
    ) %>% 
    group_by(file_path,datamodel,model_version) %>% 
    nest()
  
  # usethis::use_data(supported_datamodels, overwrite = T)
  temp_datamodel <-basename(csv) %>%
    str_remove_all('.csv') %>% 
    str_split(pattern = '_')
  new_datamodel <- temp_datamodel[[1]][1]
  new_datamodel_version <- temp_datamodel[[1]][2]
  
  new_tables <- supported_datamodels %>% 
    filter(datamodel == new_datamodel & model_version == new_datamodel_version ) %>% 
    pull(data) %>% 
    magrittr::extract2(1) %>% 
    distinct(table) %>% 
    filter(table != all_patients) %>% 
    pull(table)
  
  ## All Patients
  glue::glue_collapse(x = map(all_patients_table_template,
                              ~glue::glue(.x)
                              ),
                      sep = '\n')
  ## Subject Tables
  test <- imap(new_tables,
              ~{new_table <- new_tables[.y]
                glue::glue_collapse(x = map(subject_table_template,
                                            ~glue::glue(.x)
                                            ),
                                    sep = '\n'
                                    )}
      )
  map(test,
      ~cat(.x, file = '~/Desktop/cat_test',append = T))
  # test<-map_ch(subject_table_template,
  #          ~glue::glue(.x)
  #          )
  # test2<-map_chr(test,
  #     ~glue::glue_collapse(.x, sep = '\n')
  #     )
}
