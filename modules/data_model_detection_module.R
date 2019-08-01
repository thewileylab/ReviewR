

data_model_detection_logic <- function(input, output, session, db_connection) {
  library(tidyverse)
  library(pool)
  library(DBI)
  #ns <- session$ns
  
  # Determine what kind of connection has been established
  connection_info <- reactive({
    if(is.null(db_connection()))
      return(NULL)
    pool::dbGetInfo(db_connection())
    })
  connection_type <- connection_info$pooledObjectClass()
  
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
  )
  
  # Process the supported data models slightly, turning table names and fields to lowercase
  clean_supported_models <- supported_models %>% 
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
    )
  
  # Unnest user tables and coerce to match cdm standards
  clean_user_tables <- user_tables %>% 
    unnest() %>% 
    rename(user_fields = value) %>% 
    mutate(clean_user_fields = tolower(user_fields),
           clean_user_fields = str_replace(string = clean_user_fields, pattern = regex(pattern = '[.!?\\-]'),replacement = '_'),
           clean_table = tolower(user_database_table),
           clean_table = str_replace(string = clean_table, pattern = regex(pattern = '[.!?\\-]'),replacement = '_')) %>% 
    select(user_database_table, clean_table, user_fields, clean_user_fields)
  
  # Join user tables with supported data models, determine which one the user is likely running
  user_joined <- clean_supported_models %>% 
    mutate(model_match = map(.x = cdm,.f = left_join, clean_user_tables, by = c('table'='clean_table', 'field'='clean_user_fields')))
  
  check_it <- user_joined %>% 
    mutate(filtered = map(.x = model_match,.f = filter, is.na(user_fields)!=T),
           count_filtered = map(.x = filtered,.f = nrow), 
           count_filtered = unlist(count_filtered)
    )
  
  table_map <- check_it %>% 
    filter(count_filtered == max(count_filtered)) %>% 
    select(data_model, model_version, cdm, model_match) %>%
    arrange(desc(model_version)) %>%
    slice(1)
  
  return(list(
    'table_map' = table_map
  ))
}