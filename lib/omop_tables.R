## OMOP All Patient Table -----
all_patients_table_omop <- function(table_map, db_connection) {
  req(table_map(), db_connection() )
## Source function to extract tables and field names from table_map
  source('lib/user_table_helper.R',keep.source = F)
  library(snakecase)

## Build Concepts
  gender_concepts <- user_table(table_map, db_connection, 'concept') %>% 
    select(user_field(table_map, 'concept', 'concept_id'), 
           user_field(table_map, 'concept', 'concept_name')
           ) %>% 
    inner_join(user_table(table_map, db_connection, 'person') %>% 
                 select(user_field(table_map, 'person', 'person_id'), 
                        user_field(table_map, 'person', 'gender_concept_id')
                        ), 
               by=setNames(user_field(table_map, 'person', 'gender_concept_id'), user_field(table_map, 'concept', 'concept_id'))
               ) %>% 
    rename('Gender' = user_field(table_map, 'concept', 'concept_name')) %>% 
    select(-contains('concept_id',ignore.case = T))
  
  race_concepts <- user_table(table_map, db_connection, 'concept') %>% 
    select(user_field(table_map, 'concept', 'concept_id'), 
           user_field(table_map, 'concept', 'concept_name')
           ) %>% 
    inner_join(user_table(table_map, db_connection, 'person') %>% 
                 select(user_field(table_map, 'person', 'person_id'),
                        user_field(table_map, 'person', 'race_concept_id')
                        ), 
               by=setNames(user_field(table_map, 'person', 'race_concept_id'), user_field(table_map, 'concept', 'concept_id'))
               ) %>% 
    rename('Race' = user_field(table_map, 'concept', 'concept_name')) %>% 
    select(-contains('concept_id', ignore.case = T))
  
  ethnicity_concepts <- user_table(table_map, db_connection, 'concept') %>% 
    select(user_field(table_map, 'concept', 'concept_id'), 
           user_field(table_map, 'concept', 'concept_name')
           ) %>% 
    inner_join(user_table(table_map, db_connection, 'person') %>% 
                 select(user_field(table_map, 'person', 'person_id'),
                        user_field(table_map, 'person', 'ethnicity_concept_id')
                        ), 
               by=setNames(user_field(table_map, 'person', 'ethnicity_concept_id'), user_field(table_map, 'concept', 'concept_id'))
               ) %>% 
    rename('Ethnicity' = user_field(table_map, 'concept', 'concept_name')) %>% 
    select(-contains('concept_id', ignore.case = T))
  
  provider_concepts <- user_table(table_map, db_connection, 'provider') %>% 
    select(user_field(table_map, 'provider','provider_id'), 
           user_field(table_map, 'provider', 'provider_name')
           ) %>% 
    inner_join(user_table(table_map, db_connection, 'person') %>% 
                 select(user_field(table_map, 'person', 'person_id'),
                        user_field(table_map, 'person', 'provider_id')
                        ), 
               by=setNames(user_field(table_map, 'person', 'provider_id'), user_field(table_map, 'provider','provider_id'))
               ) %>% 
    rename('Provider' = user_field(table_map, 'provider', 'provider_name')) %>% 
    select(-contains('provider_id', ignore.case = T))
  
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

condition_era_omop <- function(table_map, db_connection, subject_id) {
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

condition_occurrence_omop <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Condition Occurrence')
  subject <- as.integer(subject_id() )
  
## Build Concepts
  condition_concepts <- user_table(table_map, db_connection, 'concept') %>% 
    select(user_field(table_map, 'concept', 'concept_id'), 
           user_field(table_map, 'concept', 'concept_name')
    ) %>%     
    inner_join(user_table(table_map, db_connection, 'condition_occurrence') %>% 
                 filter(!!as.name(user_field(table_map, 'condition_occurrence','person_id')) == subject ) %>% 
                 select(user_field(table_map, 'condition_occurrence','condition_occurrence_id'),
                      user_field(table_map, 'condition_occurrence','condition_concept_id')
                      ),
               by=setNames(user_field(table_map, 'condition_occurrence', 'condition_concept_id'), user_field(table_map, 'concept', 'concept_id'))
               ) %>% 
    rename('Condition' = user_field(table_map, 'concept', 'concept_name')) %>% 
    select(-contains('concept_id',ignore.case = T))
  
  condition_type_concepts <- user_table(table_map, db_connection, 'concept') %>% 
    select(user_field(table_map, 'concept', 'concept_id'), 
           user_field(table_map, 'concept', 'concept_name')
    ) %>%     
    inner_join(user_table(table_map, db_connection, 'condition_occurrence') %>% 
                 filter(!!as.name(user_field(table_map, 'condition_occurrence','person_id')) == subject ) %>% 
                 select(user_field(table_map,'condition_occurrence','condition_occurrence_id'),
                      user_field(table_map,'condition_occurrence','condition_type_concept_id')
                      ),
               by=setNames(user_field(table_map,'condition_occurrence','condition_type_concept_id'),user_field(table_map,'concept','concept_id'))
               ) %>%
    rename('Type' = user_field(table_map,'concept','concept_name')) %>% 
    select(-contains('concept_id',ignore.case = T))
  
  condition_status_concepts <- user_table(table_map, db_connection, 'concept') %>% 
    select(user_field(table_map, 'concept', 'concept_id'), 
           user_field(table_map, 'concept', 'concept_name')
    ) %>% 
    inner_join(user_table(table_map, db_connection, 'condition_occurrence') %>% 
                 filter(!!as.name(user_field(table_map, 'condition_occurrence','person_id')) == subject ) %>% 
                 select(user_field(table_map,'condition_occurrence','condition_occurrence_id'),
                      user_field(table_map,'condition_occurrence','condition_status_concept_id')
               ),
               by=setNames(user_field(table_map,'condition_occurrence','condition_status_concept_id'), user_field(table_map,'concept','concept_id'))
               ) %>% 
    rename('Status' = user_field(table_map,'concept','concept_name')) %>% 
    select(-contains('concept_id',ignore.case = T))
  
  condition_provider_concepts <- user_table(table_map, db_connection, 'provider') %>% 
    select(user_field(table_map, 'provider', 'provider_id'),
           user_field(table_map, 'provider', 'provider_name')
    ) %>% 
    inner_join(user_table(table_map, db_connection, 'condition_occurrence') %>% 
                 filter(!!as.name(user_field(table_map, 'condition_occurrence','person_id')) == subject ) %>% 
                 select(user_field(table_map,'condition_occurrence','condition_occurrence_id'),
                      user_field(table_map,'condition_occurrence','provider_id')
                      ), 
               by=setNames(user_field(table_map,'condition_occurrence','provider_id'), user_field(table_map, 'provider','provider_id'))
               ) %>% 
    rename('Provider' = user_field(table_map, 'provider','provider_name')) %>% 
    select(-contains('provider_id',ignore.case = T))
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

