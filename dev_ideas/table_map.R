table_map <- function(db_type = "BigQuery", data_model = "OMOP", model_version = "v5_3_1", connection = con){

  # For Testing... would be pulled from elsewhere in the Shiny App
  project_id <- "class-coursera-dev"
  wd <- getwd()
  con <- DBI::dbConnect(drv = bigquery(),
                        project = project_id,
                        dataset = "synpuf1k_omop_cdm")
  
  db_type = "BigQuery"; user_data_model = "omop"; user_model_version = "v6_0"
# Load all the models we support. Process the file path and file name to determine model type and version (hopefully) 
  models <- list.files(path = file.path(wd,"models"),full.names = T,recursive = T) %>% 
    tibble(file_path = .) %>% 
    mutate(data_model = str_extract(string = file_path, pattern = regex('(mimic3)|(omop)',ignore_case = T)),
           data_model = tolower(data_model),
           model_version = basename(file_path),
           model_version = str_replace(string = model_version, pattern = regex(pattern = '(mimic3)(_)?|(omop_cdm_)',ignore_case = T),replacement = ''),
           model_version = str_replace(string = model_version, pattern = regex(pattern = '.csv',ignore_case = T),replacement = ''),
           model_version = tolower(x = model_version),
           cdm = map(.x = file_path,.f = read_csv)
          )
#########################################################################  
# Populate reactive dropdown from data_model column
# Populate reactive dropdown based on data_model with supported versions
#########################################################################
  
# Select the appropriate data_model based on user input
  selected_model <-models %>% 
    filter(data_model == user_data_model & model_version == user_model_version) #Select based on user input
    
# Generate table mapping with user data
  ## Store expected data model
  cdm <- unnest(data = selected_model$cdm[[1]]) %>% 
    select(table,field) %>% 
    nest(field, .key = "fields_long")
  ## Load user tables
  user_tables <- dbListTables(con) %>% 
    tibble(user_database_table = .) %>% 
    mutate(user_fields_long = map(.x = user_database_table,.f = dbListFields,conn=con),
           user_fields_long = map(.x = user_fields_long,.f = as.tibble)
           )
  
  ## Coerce user tables to match expected data model
  user_tables %<>% 
    mutate(clean_table = tolower(user_database_table)) %>% 
    mutate(clean_table = str_replace(string = clean_table, pattern = regex(pattern = '[.!?\\-]'),replacement = '_')) ##Any other string separators?!
## Left Join to create table map
  table_map <- cdm %>% 
    left_join(user_tables, by = c("table"="clean_table"))
  
  
} #End function