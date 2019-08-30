#' OMOP Get Concepts
#'
#' @param table_map tibble containing standard CDM mapped to user tables
#' @param db_connection DBI connection object
#' @param concept_table Standard CDM concept table name
#' @param concept_id Standard CDM concept id field
#' @param concept_name Standard CDM concept name field
#' @param table Table that needs concepts retrieved
#' @param joinable_id what variable is joinable between the concept table and the desired table
#' @param table_concept which concept are you retrieving?
#' @param col_name would you like to rename the retrieved concept?
#'
#' @return
#' @export
#'
#' @examples 
#' gender_concepts <- get_concept(table_map = table_map,  db_connection = db_connection,  concept_table = 'concept', concept_id = 'concept_id',  concept_name = 'concept_name',  table = 'person', joinable_id = 'person_id', concept = 'gender_concept_id',  col_name = 'Gender')
#' provider_concepts <- get_concept(table_map = table_map,  db_connection = db_connection,  concept_table = 'provider', concept_id = 'provider_id',  concept_name = 'provider_name',  table = 'person', joinable_id = 'person_id', concept = 'provider_id',  col_name = 'Provider') 

get_all_concept <- function(table_map, db_connection, concept_table, concept_id, concept_name, table, joinable_id, table_concept_id, col_name) {
  req(table_map(), db_connection() )
  source('lib/user_table_helper.R', keep.source = F)
  
  user_table(table_map, db_connection, concept_table) %>% 
    select(user_field(table_map, concept_table, concept_id), 
           user_field(table_map, concept_table, concept_name)
    ) %>% 
    inner_join(user_table(table_map, db_connection, table) %>% 
                 select(user_field(table_map, table, joinable_id), 
                        user_field(table_map, table, table_concept_id)
                 ), 
               by=setNames(user_field(table_map, table, table_concept_id), user_field(table_map, concept_table, concept_id))
    ) %>% 
    rename(!!col_name := user_field(table_map, concept_table, concept_name)) %>% 
    select(-contains(concept_id,ignore.case = T))
}

get_subject_concept <- function(table_map, db_connection, concept_table, concept_id, concept_name, table, joinable_id, table_concept_id, col_name, subject_id_field, selected_subject) {
  req(table_map(), db_connection() )
  source('lib/user_table_helper.R', keep.source = F)
  
  user_table(table_map, db_connection, concept_table) %>% 
    select(user_field(table_map, concept_table, concept_id), 
           user_field(table_map, concept_table, concept_name)
    ) %>% 
    inner_join(user_table(table_map, db_connection, table) %>% 
                 filter(!!as.name(user_field(table_map, table,subject_id_field)) == selected_subject ) %>% 
                 select(user_field(table_map, table, joinable_id), 
                        user_field(table_map, table, table_concept_id)
                 ), 
               by=setNames(user_field(table_map, table, table_concept_id), user_field(table_map, concept_table, concept_id))
    ) %>% 
    rename(!!col_name := user_field(table_map, concept_table, concept_name)) %>% 
    select(-contains(!!concept_id,ignore.case = T))
}

