#' Data Model Detection Module
#'
#' This module is designed to connect to a user supplied database, compare it with known common data models and determine the most likely version of the user's database.
#'
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_model_detection_module
#' 
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList HTML
#' 

data_model_detection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('data_model_ui')) %>% withSpinner(type = 2,color.background = '#FFFFFF')
    )
}


#' @rdname mod_data_model_detection_module
#' @param db_connection Connection info received from the database setup module
#' @export
#' @keywords internal
#' @importFrom magrittr %>% 
#' @importFrom DBI dbListTables dbListFields dbGetInfo
#' @importFrom dplyr mutate rename select left_join filter ungroup arrange slice group_by desc
#' @importFrom tibble tibble enframe
#' @importFrom purrr map
#' @importFrom tidyr unnest as_tibble separate drop_na
#' @importFrom stringr str_replace regex str_extract
#' @importFrom rlang .data
#' @importFrom utils data

data_model_detection_logic <- function(input, output, session, db_connection, connect, db_type) {
  ns <- session$ns
  
  table_map <- eventReactive(connect(), {
    req(db_connection())
    
    # Load user tables and nest fields. 
    user_tables <- dbListTables(db_connection()) %>% 
      tibble(user_database_table = .data$.) %>% 
      mutate(user_fields_long = map(.x = .data$user_database_table,.f = dbListFields,conn=db_connection()),
             user_fields_long = map(.x = .data$user_fields_long,.f = as_tibble)
             ) %>% 
      ## Unnest user tables and coerce to match cdm standards
      unnest(cols = c(.data$user_fields_long)) %>% 
      rename(user_fields = .data$value) %>% 
      mutate(clean_user_fields = tolower(.data$user_fields),
             clean_user_fields = str_replace(string = .data$clean_user_fields, pattern = regex(pattern = '[.!?\\-]'),replacement = '_'),
             clean_table = tolower(.data$user_database_table),
             clean_table = str_replace(string = .data$clean_table, pattern = regex(pattern = '[.!?\\-]'),replacement = '_')) %>% 
      select(.data$user_database_table, .data$clean_table, .data$user_fields, .data$clean_user_fields)
    
    # Join user tables with supported data models, determine which one the user is likely running
    user_joined <- ReviewR::supported_models %>% 
      mutate(model_match = map(.x = data,.f = left_join, user_tables, by = c('table'='clean_table', 'field'='clean_user_fields')))%>% 
      mutate(filtered = map(.x = .data$model_match,.f = filter, is.na(.data$user_fields)!=T),
             count_filtered = map(.x = .data$filtered,.f = nrow), 
             count_filtered = unlist(.data$count_filtered)
             )
    
    table_map <- user_joined %>% 
      ungroup() %>% 
      filter(.data$count_filtered == max(.data$count_filtered)) %>% 
      select(.data$data_model, .data$model_version, .data$data, .data$model_match, .data$count_filtered) %>%
      arrange(desc(.data$model_version)) %>%
      slice(1)
    return(table_map)
    })
  db_info <- reactive({
    req(db_connection() )
    DBI::dbGetInfo(db_connection() ) %>% 
      tibble::enframe(name = NULL) %>% 
      separate(col = .data$value, into = c('project', 'dataset'),sep = '\\.',fill = 'right') %>% 
      tidyr::drop_na()
  })
  
  data_model_message <- eventReactive(connect(), {
    req(db_connection() )
    if (table_map()$count_filtered !=0) {
        HTML(paste('<H3>Success!!</H3>', 
                   'You have connected to a', ifelse(db_type() == 'bigquery', 'Google BigQuery', 'Unknown'), 'database.',
                   '<br>',
                   '<br>',
                   '<H4>Connection Information:</H4>',
                   '<b>Project:</b>', db_info()$project,
                   '<br>',
                   '<b>Dataset:</b>', db_info()$dataset,
                   '<br>',
                   '<b>Data Model:</b>', table_map()$data_model,
                   '<br>',
                   '<b>Version:</b>', table_map()$model_version,
                   '<br><br>'))
    } else {HTML(paste('The selected database does not appear to be in OMOP or MIMIC III format. Please disconnect and select another database.',
                       '<br><br>'))
      }
  })
  
  data_model_text <- eventReactive(connect(), {
    req(db_connection() )
    if (table_map()$count_filtered !=0) {
      HTML(paste('<b>Data Model:</b>', table_map()$data_model,
                 '<br>',
                 '<b>Version:</b>', table_map()$model_version,
                 '<br><br>'))
    } else {HTML(paste('The selected database does not appear to be in OMOP or MIMIC III format. Please disconnect and select another database.',
                       '<br><br>'))
    }
  })
  
  output$data_model_ui <- renderUI({
    req(data_model_message() )
    tagList(
      data_model_message(),
      actionButton(inputId = ns('db_disconnect'),label = 'Disconnect')
      )
  })

  db_disconnect <- reactive({ input$db_disconnect })  

  return(list(
    'table_map' = table_map,
    'data_model_message' = data_model_message,
    'data_model_text' = data_model_text, 
    'db_disconnect' = db_disconnect
  ))
}