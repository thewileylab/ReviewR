# UI ----
#' Data Model Detection Module
#'
#' This module is designed to connect to a user supplied database, compare it with known common data models and determine the most likely version of the user's database.
#'
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_datamodel_setup
#' 
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList HTML
#' 

data_model_detection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('data_model_ui'))
    )
}

# Server ----
#' @rdname mod_datamodel_setup
#' @param db_connection Connection info received from the database setup module
#' @export
#' @keywords internal
#' @importFrom magrittr %>% 
#' @importFrom DBI dbListTables dbListFields
#' @importFrom dplyr mutate rename select left_join filter ungroup arrange slice group_by desc
#' @importFrom glue glue
#' @importFrom tibble tibble enframe
#' @importFrom purrr map
#' @importFrom tidyr unnest as_tibble separate drop_na
#' @importFrom stringr str_replace str_replace_all regex str_extract
#' @importFrom rlang .data
#' @importFrom utils data

mod_data_model_detection_server <- function(id, database_vars) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$n
      data_model_vars <- reactiveValues(
        table_map = NULL,
        message = NULL
        )
      
      # Calculate Table Map ----
      ## Determine which user fields map to known CDM field values
      observeEvent(database_vars()$is_connected, {
        req(database_vars()$is_connected == 'yes', database_vars()$db_con)
        
        ### Load user tables and nest fields. 
        user_tables <- dbListTables(database_vars()$db_con) %>% 
          tibble(user_database_table = .data$.) %>% 
          mutate(user_fields_long = map(.x = .data$user_database_table,.f = dbListFields,conn=database_vars()$db_con),
                 user_fields_long = map(.x = .data$user_fields_long,.f = as_tibble)
                 ) %>% 
          #### Unnest user tables and coerce to match cdm standards
          unnest(cols = c(.data$user_fields_long)) %>% 
          rename(user_fields = .data$value) %>% 
          mutate(clean_user_fields = tolower(.data$user_fields),
                 clean_user_fields = str_replace(string = .data$clean_user_fields, pattern = regex(pattern = '[.!?\\-]'),replacement = '_'),
                 clean_table = tolower(.data$user_database_table),
                 clean_table = str_replace(string = .data$clean_table, pattern = regex(pattern = '[.!?\\-]'),replacement = '_')) %>% 
          select(.data$user_database_table, .data$clean_table, .data$user_fields, .data$clean_user_fields)
        
        ### Join user tables with supported data models, determine which one the user is likely running
        user_joined <- ReviewR::supported_datamodels %>% 
          mutate(model_match = map(.x = data,.f = left_join, user_tables, by = c('table'='clean_table', 'field'='clean_user_fields')))%>% 
          mutate(filtered = map(.x = .data$model_match,.f = filter, is.na(.data$user_fields)!=T),
                 count_filtered = map(.x = .data$filtered,.f = nrow), 
                 count_filtered = unlist(.data$count_filtered)
                 )
        
        ### Select and store the most likely mapping based on matching fields
        data_model_vars$table_map <- user_joined %>% 
          ungroup() %>% 
          filter(.data$count_filtered == max(.data$count_filtered)) %>% 
          select(.data$data_model, .data$model_version, .data$data, .data$model_match, .data$count_filtered) %>%
          arrange(desc(.data$model_version)) %>%
          slice(1)
        })
      
      # Create UI Message ----
      observeEvent(data_model_vars$table_map, {
        req(data_model_vars$table_map)
        data_model_vars$message <-if (data_model_vars$table_map$count_filtered !=0) {
            HTML(glue::glue('<em>Data Model: {data_model_vars$table_map$data_model} {str_replace_all(data_model_vars$table_map$model_version,"_",".")}</em>'))
            } else {HTML(paste('<em>The selected database does not appear to be in OMOP or MIMIC III format. Please disconnect and select another database.</em>'))
              }
        })
      
      # UI Outputs ----
      output$data_model_ui <- renderUI({
        req(database_vars()$is_connected == 'yes')
        tagList(
          data_model_vars$message
          )
        })
      
      # Return ----
      return(data_model_vars)
      }
    )
}
