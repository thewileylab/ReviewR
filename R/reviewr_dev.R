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
