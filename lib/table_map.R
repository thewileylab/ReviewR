#' Determine Common Data Model Version
#' 
#' \code{find_best_cdm_version} returns a structure containing the best CDM definition to use for the
#' given database.
#' 
#' This function performs a heuristic check between all available common data model (CDM) definitions
#' and the available table definitions (including columns) for the active connection.  It will
#' return the last (sorted alphabetically) CDM version if there are more than one that match.
#' 
#' @param models A tbl containing all of the CDM table and column names
#' @param user_tables The available tables within the active database connection
#' @param user_data_model The selected data model (OMOP or MIMIC) that the user said to use.
#' 
#' @return A string containing the version for the CDM that the user's database is most likely running.
find_best_cdm_version <- function(models, user_tables, user_data_model) {
  potential_models <- models %>%
    filter(data_model == user_data_model)
  
  ## Unnest and coerce to match cdm standards
  user_tables %<>% 
    unnest() %>% 
    rename(user_fields = value) %>% 
    mutate(clean_user_fields = tolower(user_fields),
           clean_user_fields = str_replace(string = clean_user_fields, pattern = regex(pattern = '[.!?\\-]'),replacement = '_'),
           clean_table = tolower(user_database_table),
           clean_table = str_replace(string = clean_table, pattern = regex(pattern = '[.!?\\-]'),replacement = '_')) %>% 
    select(user_database_table, clean_table, user_fields, clean_user_fields)
  
  potential_models %<>%
    mutate(join = map(.x = cdm,.f = left_join, user_tables, by = c("table"="clean_table", "field"="clean_user_fields")))
  
  check_it <- potential_models %>% 
    mutate(filtered = map(.x = join,.f = filter, is.na(user_fields)!=T),
           count_filtered = map(.x = filtered,.f = nrow), 
           count_filtered = unlist(count_filtered)
    )

  table_map <- check_it %>% 
    filter(count_filtered == max(count_filtered)) %>% 
    select(data_model, model_version, cdm, join) %>%
    arrange(desc(model_version)) %>%
    slice(1)
  #table_map$model_version
  table_map
}

#' Generate Common Data Model Table Map
#' 
#' \code{table_map} creates a tibble containing a map between canonical table/column names and what
#' is in the user's database
#'
table_map <- function(db_type, user_data_model, connection) {
  wd <- getwd()
  user_data_model <- tolower(user_data_model)

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
  
  ## Load user tables and nest fields
  user_tables <- dbListTables(connection) %>% 
    tibble(user_database_table = .) %>% 
    mutate(user_fields_long = map(.x = user_database_table,.f = dbListFields,conn=connection),
           user_fields_long = map(.x = user_fields_long,.f = as.tibble)
    )
  
  user_model = find_best_cdm_version(models, user_tables, user_data_model)
  user_model$join[[1]]
} #End function