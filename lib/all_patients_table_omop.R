
all_patients_table_omop <- function(table_map, db_connection) {
req(table_map(), db_connection() )
  
## OMOP All Patient Table

## Source function to extract tables and field names from table_map
source('lib/omop_helper.R',keep.source = F)

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
  arrange(ID)
}

