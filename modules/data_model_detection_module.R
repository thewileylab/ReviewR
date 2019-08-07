data_model_detection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('data_model_ui')) %>% withSpinner()
    )
}

data_model_detection_logic <- function(input, output, session, db_connection) {
  library(tidyverse)
  library(pool)
  library(DBI)
  ns <- session$ns
  
  table_map <- eventReactive(db_connection(), {
    # Load all the models we support. Process the file path and file name to determine model type and version (hopefully) 
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
    
    # Load user tables and nest fields. 
    user_tables <- dbListTables(db_connection()) %>% 
      tibble(user_database_table = .) %>% 
      mutate(user_fields_long = map(.x = user_database_table,.f = dbListFields,conn=db_connection()),
             user_fields_long = map(.x = user_fields_long,.f = as.tibble)
             ) %>% 
      ## Unnest user tables and coerce to match cdm standards
      unnest() %>% 
      rename(user_fields = value) %>% 
      mutate(clean_user_fields = tolower(user_fields),
             clean_user_fields = str_replace(string = clean_user_fields, pattern = regex(pattern = '[.!?\\-]'),replacement = '_'),
             clean_table = tolower(user_database_table),
             clean_table = str_replace(string = clean_table, pattern = regex(pattern = '[.!?\\-]'),replacement = '_')) %>% 
      select(user_database_table, clean_table, user_fields, clean_user_fields)
    
    # Join user tables with supported data models, determine which one the user is likely running
    user_joined <- supported_models %>% 
      mutate(model_match = map(.x = cdm,.f = left_join, user_tables, by = c('table'='clean_table', 'field'='clean_user_fields')))%>% 
      mutate(filtered = map(.x = model_match,.f = filter, is.na(user_fields)!=T),
             count_filtered = map(.x = filtered,.f = nrow), 
             count_filtered = unlist(count_filtered)
             )
    
    table_map <- user_joined %>% 
      filter(count_filtered == max(count_filtered)) %>% 
      select(data_model, model_version, cdm, model_match, count_filtered) %>%
      arrange(desc(model_version)) %>%
      slice(1)
    return(table_map)
    })
  
  data_model_text <- reactive({
    if(is.null( table_map() )) {
      return(NULL)
      } else if (!is.null( table_map() ) & table_map()$count_filtered !=0) {
        paste('<b>Data Model:</b>',table_map()$data_model,'<br>','<b>Version:</b>', table_map()$model_version)
        } else {paste('The selected database does not appear to be in OMOP or MIMIC III format. Please disconnect and select another database.')}
  })
  
  output$data_model_ui <- renderText({ data_model_text() })
    
    
  return(list(
    'table_map' = table_map
  ))
}