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
#' @param mod_name 
#' @param display_name 
#'
#' @return
#'
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map
#' @examples
dev_database_module <- function(mod_name, display_name) {
  filename <- glue::glue('mod_{mod_name}.R')
  cat(glue::glue_collapse(x = map(ReviewR::db_module_template,
                                  ~glue::glue(.x)),
                          sep = '\n' 
                          ),
      file = glue::glue('R/{filename}') 
      )
}
