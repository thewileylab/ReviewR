data_model_detection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('data_model_ui')) %>% withSpinner(type = 2,color.background = '#FFFFFF')
    )
}

#' data_model_detection_logic
#'
#' @param input Required by Shiny for module operation
#' @param output Required by Shiny for module operation
#' @param session Required by Shiny for module operation
#' @param db_connection Connection info received from the database setup module
#'
#' @return tibble containing a the cdm that most closely matches the user's database and a map of standard tables to user tables
#' @export
#'
#' @examples
data_model_detection_logic <- function(input, output, session, db_connection, connect, supported_models, db_type) {
  library(tidyverse)
  library(DBI)
  ns <- session$ns
  
  table_map <- eventReactive(connect(), {
    req(db_connection())
    # Load user tables and nest fields. 
    user_tables <- dbListTables(db_connection()) %>% 
      tibble(user_database_table = .) %>% 
      mutate(user_fields_long = map(.x = user_database_table,.f = dbListFields,conn=db_connection()),
             user_fields_long = map(.x = user_fields_long,.f = as_tibble)
             ) %>% 
      ## Unnest user tables and coerce to match cdm standards
      unnest(cols = c(user_fields_long)) %>% 
      rename(user_fields = value) %>% 
      mutate(clean_user_fields = tolower(user_fields),
             clean_user_fields = str_replace(string = clean_user_fields, pattern = regex(pattern = '[.!?\\-]'),replacement = '_'),
             clean_table = tolower(user_database_table),
             clean_table = str_replace(string = clean_table, pattern = regex(pattern = '[.!?\\-]'),replacement = '_')) %>% 
      select(user_database_table, clean_table, user_fields, clean_user_fields)
    
    # Join user tables with supported data models, determine which one the user is likely running
    user_joined <- supported_models %>% 
      mutate(model_match = map(.x = data,.f = left_join, user_tables, by = c('table'='clean_table', 'field'='clean_user_fields')))%>% 
      mutate(filtered = map(.x = model_match,.f = filter, is.na(user_fields)!=T),
             count_filtered = map(.x = filtered,.f = nrow), 
             count_filtered = unlist(count_filtered)
             )
    
    table_map <- user_joined %>% 
      ungroup() %>% 
      filter(count_filtered == max(count_filtered)) %>% 
      select(data_model, model_version, data, model_match, count_filtered) %>%
      arrange(desc(model_version)) %>%
      slice(1)
    return(table_map)
    })
  db_info <- reactive({
    req(db_connection() )
    DBI::dbGetInfo(db_connection() ) %>% 
      tibble::enframe(name = NULL) %>% 
      separate(col = value, into = c('project', 'dataset'),sep = '\\.',fill = 'right') %>% 
      remove_missing(na.rm = TRUE)
  })
  
  data_model_message <- eventReactive(connect(), {
    req(db_connection(), table_map() )
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
    req(db_connection(), table_map() )
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