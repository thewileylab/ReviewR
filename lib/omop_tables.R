## Source function to extract tables and field names from table_map
source('lib/user_table_helper.R',keep.source = F)
source('lib/omop_get_concepts.R')
library(snakecase)

## OMOP All Patient Table -----
omop_table_all_patients <- function(table_map, db_connection) {
  req(table_map(), db_connection() )
## Build Concepts
  gender_concepts <- get_all_concept(table_map = table_map,  db_connection = db_connection,  concept_table = 'concept', concept_id = 'concept_id',  concept_name = 'concept_name',  table = 'person', joinable_id = 'person_id', table_concept_id = 'gender_concept_id',  col_name = 'Gender')
  race_concepts <- get_all_concept(table_map = table_map, db_connection = db_connection, concept_table = 'concept', concept_id = 'concept_id', concept_name = 'concept_name',table = 'person', joinable_id = 'person_id', table_concept_id = 'race_concept_id', col_name = 'Race')
  ethnicity_concepts <- get_all_concept(table_map = table_map, db_connection = db_connection, concept_table = 'concept', concept_id = 'concept_id', concept_name = 'concept_name',table = 'person', joinable_id = 'person_id',table_concept_id = 'ethnicity_concept_id', col_name = 'Ethnicity')
  provider_concepts <- get_all_concept(table_map = table_map,  db_connection = db_connection,  concept_table = 'provider', concept_id = 'provider_id',  concept_name = 'provider_name',  table = 'person', joinable_id = 'person_id', table_concept_id = 'provider_id',  col_name = 'Provider' )
                            
## Return All Patients Table Representation
  user_table(table_map, db_connection, 'person') %>% 
    select(-matches('gender*|race*|ethnicity*|provider*|location*|care*',ignore.case = T)) %>% 
    left_join(gender_concepts) %>% 
    left_join(race_concepts) %>% 
    left_join(ethnicity_concepts) %>% 
    left_join(provider_concepts) %>% 
    collect() %>% 
    unite(col = 'Birth_Date', c('year_of_birth','month_of_birth','day_of_birth')) %>% 
    mutate(Birth_Date = as_date(Birth_Date)) %>% 
    select('ID' = person_id, Gender, 'SourceVal' = person_source_value, everything()) %>% 
    arrange(ID) %>% 
    rename_at(vars(-contains('ID',ignore.case = F)), to_title_case)
}

## OMOP Condition ERA -----

omop_table_condition_era <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Condition ERA')
  subject <- as.integer(subject_id() )
  
  user_table(table_map, db_connection, 'concept') %>%
    select(user_field(table_map, 'concept', 'concept_id'),
           user_field(table_map, 'concept', 'concept_name')
    ) %>%
    inner_join(user_table(table_map, db_connection, 'condition_era') %>%
                 filter(!!as.name(user_field(table_map, 'condition_era','person_id')) == subject ),
               by=setNames(user_field(table_map, 'condition_era', 'condition_concept_id'), user_field(table_map, 'concept', 'concept_id'))
               ) %>%
    rename('Condition' = user_field(table_map, 'concept', 'concept_name')) %>%
    select(-matches('concept*|person*',ignore.case = T)) %>% 
    select('ID' = user_field(table_map, 'condition_era','condition_era_id'), everything()) %>% 
    arrange(ID) %>% 
    collect() %>% 
    rename_at(vars(-contains('ID',ignore.case = F)), to_title_case)
}

## OMOP Condition Occurrence -----

omop_table_condition_occurrence <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Condition Occurrence')
  subject <- as.integer(subject_id() )
  
## Build Concepts
  condition_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','condition_occurrence','condition_occurrence_id','condition_concept_id','Condition','person_id', subject)
  condition_type_concepts <- get_subject_concept(table_map, db_connection, 'concept', 'concept_id','concept_name','condition_occurrence','condition_occurrence_id','condition_type_concept_id','Type','person_id', subject)
  condition_status_concepts <- get_subject_concept(table_map, db_connection, 'concept', 'concept_id','concept_name','condition_occurrence','condition_occurrence_id','condition_status_concept_id','Status','person_id', subject)
  condition_provider_concepts <- get_subject_concept(table_map, db_connection, 'provider', 'provider_id','provider_name','condition_occurrence','condition_occurrence_id','provider_id','Provider','person_id', subject)
  
## Return Condition Occurrence Table Representation  
  user_table(table_map, db_connection, 'condition_occurrence') %>% 
    filter(!!as.name(user_field(table_map, 'condition_occurrence','person_id')) == subject ) %>% 
    select(-matches('person*|condition_concept*|condition_status*|provider_id|condition_type*|condition_source_concept*',ignore.case = T)) %>% 
    left_join(condition_concepts) %>% 
    left_join(condition_status_concepts) %>% 
    left_join(condition_provider_concepts) %>% 
    left_join(condition_type_concepts) %>% 
    select('ID' = user_field(table_map,'condition_occurrence','condition_occurrence_id'), Condition, 'SourceVal' = user_field(table_map,'condition_occurrence','condition_source_value'), Status, everything(), Type, Provider, 'Visit' = user_field(table_map,'condition_occurrence','visit_occurrence_id')) %>% 
    arrange(ID) %>% 
    collect() %>% 
    rename_at(vars(-contains('ID',ignore.case = F)), to_title_case)
  }
## OMOP Death -----

omop_table_death <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Death')
  subject <- as.integer(subject_id() )

## Build Concepts
  death_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','death','person_id','death_type_concept_id','Type','person_id', subject)
  death_cause_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','death','person_id','cause_concept_id','Cause','person_id', subject)
  
## Return Death Table Representation 
  user_table(table_map, db_connection, 'death') %>% 
    filter(!!as.name(user_field(table_map, 'death','person_id')) == subject ) %>% 
    select(-matches('death_type_concept*|cause_concept*|cause_source_concept*', ignore.case = T)) %>% 
    left_join(death_cause_concepts) %>% 
    left_join(death_type_concepts) %>% 
    select('ID' = person_id, 'SourceVal' = user_field(table_map, 'death', 'cause_source_value'), everything()) %>% 
    collect() %>% 
    rename_at(vars(-contains('ID',ignore.case = F)), to_title_case)
}

## Device Exposure -----
## Dose Era -----
## Drug Era -----
## Drug Exposure -----
## Measurement -----
## Note -----
## Observation -----
## Observation Period -----
## Payer Plan Period -----
## Procedure Occurrence -----
## Specimen -----
## Visit Occurrence -----