user_table <- function(table_map, connection_info, desired_omop_table) {
  req(table_map(), connection_info())
  table_name <- table_map()$model_match[[1]] %>% 
    filter(table == desired_omop_table) %>% 
    distinct(table, .keep_all = T) %>% 
    select(user_database_table) %>% 
    pluck(1)
  tbl(src = connection_info(), table_name)
}

user_field <- function(table_map, desired_omop_table, desired_omop_field){
  req(table_map())
  table_map()$model_match[[1]] %>% 
    filter(table == desired_table & field == desired_omop_field) %>% 
    select(user_fields) %>% 
    pluck(1)
}
