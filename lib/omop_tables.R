## Source function to extract tables and field names from table_map
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
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    unite(col = 'Birth_Date', c('year_of_birth','month_of_birth','day_of_birth')) %>% 
    mutate(Birth_Date = as_date(Birth_Date)) %>% 
    select('ID' = user_field(table_map, 'person', 'person_id'), Gender, 'SourceVal' = user_field(table_map, 'person', 'person_source_value'), everything()) %>% 
    arrange(ID) %>% 
    rename_at(vars(-1), to_title_case)
}

## OMOP Condition Era -----

omop_table_condition_era <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Condition Era')
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
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
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
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
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
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Device Exposure -----
omop_table_device_exposure <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Device Exposure')
  subject <- as.integer(subject_id() )
  
  ## Build Concepts
  device_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','device_exposure','device_exposure_id','device_concept_id','Device','person_id', subject)
  device_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','device_exposure','device_exposure_id','device_type_concept_id','Type','person_id', subject)
  device_provider_concepts <- get_subject_concept(table_map, db_connection, 'provider','provider_id','provider_name','device_exposure','device_exposure_id','provider_id','Provider','person_id', subject)

  ## Return Device Exposure Table Representation
  user_table(table_map, db_connection, 'device_exposure') %>% 
    filter(!!as.name(user_field(table_map, 'device_exposure','person_id')) == subject ) %>% 
    left_join(device_concepts) %>% 
    left_join(device_type_concepts) %>% 
    left_join(device_provider_concepts) %>% 
    select('ID' = user_field(table_map, 'device_exposure','device_exposure_id'), everything()) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}
 
## Dose Era -----
omop_table_dose_era <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Dose Era')
  subject <- as.integer(subject_id() )

  ## Build Concepts
  dose_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','dose_era','dose_era_id','drug_concept_id','Drug','person_id', subject)
  unit_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','dose_era','dose_era_id','unit_concept_id','Unit','person_id', subject)

  ## Return Dose Era Table Representation
  user_table(table_map, db_connection, 'dose_era') %>% 
    filter(!!as.name(user_field(table_map, 'dose_era','person_id')) == subject ) %>% 
    select(-matches('person_id|drug_concept_id|unit_concept_id')) %>% 
    left_join(dose_concepts) %>% 
    left_join(unit_concepts) %>% 
    select('ID' = user_field(table_map, 'dose_era', 'dose_era_id'), Drug, Unit, 'DoseValue' = dose_value, everything()) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Drug Era -----
omop_table_drug_era <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Drug Era')
  subject <- as.integer(subject_id() )
  
  ## Build Concepts
  drug_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','drug_era','drug_era_id','drug_concept_id','Drug','person_id', subject)
  
  ## Return Drug Era Table Representation
  user_table(table_map, db_connection, 'drug_era') %>% 
    filter(!!as.name(user_field(table_map, 'drug_era','person_id')) == subject ) %>% 
    select(-matches('person_id|drug_concept_id')) %>% 
    left_join(drug_concepts) %>% 
    select('ID' = user_field(table_map, 'drug_era', 'drug_era_id'), Drug, everything()) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
  
}

## Drug Exposure -----
omop_table_drug_exposure <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Drug Exposure')
  subject <- as.integer(subject_id() )
  
  ## Build Concepts
  drug_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','drug_exposure','drug_exposure_id','drug_concept_id','Drug','person_id', subject)
  drug_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','drug_exposure','drug_exposure_id','drug_type_concept_id','Type','person_id', subject)
  route_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','drug_exposure','drug_exposure_id','route_concept_id','Route','person_id', subject)
  provider_concepts <- get_subject_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'drug_exposure', 'drug_exposure_id', 'provider_id', 'Provider', 'person_id', subject)

  ## Return Drug Exposure Table Representation
  user_table(table_map, db_connection, 'drug_exposure') %>% 
    filter(!!as.name(user_field(table_map, 'drug_era','person_id')) == subject ) %>%
    select(-matches('person_id|drug_concept_id|drug_type_concept_id|route_concept_id|provider_id|visit_detail_id|drug_source_concept_id|route_source_value|dose_unit_source_value')) %>% 
    left_join(drug_concepts) %>% 
    left_join(drug_type_concepts) %>% 
    left_join(route_concepts) %>% 
    left_join(provider_concepts) %>% 
    select('ID' = user_field(table_map, 'drug_exposure', 'drug_exposure_id'), Drug, 'SourceVal' = user_field(table_map, 'drug_exposure', 'drug_source_value'),  'StartDate' = user_field(table_map, 'drug_exposure', 'drug_exposure_start_date'),
           'StartDateTime' = user_field(table_map, 'drug_exposure','drug_exposure_start_datetime'), 'EndDate' = user_field(table_map, 'drug_exposure', 'drug_exposure_end_date'), 'EndDateTime' = user_field(table_map, 'drug_exposure', 'drug_exposure_end_datetime'),
           'VerbatimEnd' = user_field(table_map, 'drug_exposure', 'drug_exposure_verbatim_end_date'), Type, 'Visit' = user_field(table_map, 'drug_exposure', 'visit_occurrence_id'), everything() ) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
  
}

## Measurement -----
omop_table_measurement <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Measurement')
  subject <- as.integer(subject_id() )
  
  ## Build Concepts
  measurement_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','measurement_concept_id','Measurement','person_id', subject)
  measurement_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','measurement_type_concept_id','Type','person_id', subject)
  operator_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','operator_concept_id','Operator','person_id', subject)
  value_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','value_as_concept_id','Value','person_id', subject)
  unit_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','unit_concept_id','Unit','person_id', subject)
  provider_concepts <- get_subject_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'measurement', 'measurement_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Return Drug Exposure Table Representation
  user_table(table_map, db_connection, 'measurement') %>% 
    filter(!!as.name(user_field(table_map, 'drug_era','person_id')) == subject ) %>% 
    select(-matches('person_id|measurement_concept_id|measurement_type_concept_id|operator_concept_id|value_as_concept_id|unit_concept_id|provider_id')) %>% 
    left_join(measurement_concepts) %>% 
    left_join(measurement_type_concepts) %>% 
    left_join(operator_concepts) %>% 
    left_join(value_concepts) %>% 
    left_join(unit_concepts) %>% 
    left_join(provider_concepts) %>% 
    select('ID' = user_field(table_map, 'measurement', 'measurement_id'), everything() ) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Note -----
omop_table_note <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Note')
  subject <- as.integer(subject_id() )
  
  ## Build Concepts
  note_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','note','note_id','note_type_concept_id','Type','person_id', subject)
  note_class_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','note','note_id','note_class_concept_id','Class','person_id', subject)
  note_encoding_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','note','note_id','encoding_concept_id','Encoding','person_id', subject)
  note_language_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','note','note_id','language_concept_id','Language','person_id', subject)
  provider_concepts <- get_subject_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'note', 'note_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Redurn Note Table Representation
  user_table(table_map, db_connection, 'note') %>% 
    filter(!!as.name(user_field(table_map, 'note','person_id')) == subject ) %>% 
    select(-matches('person_id|note_type_concept_id|note_class_concept_id|encoding_concept_id|language_concept_id|provider_id')) %>% 
    left_join(note_type_concepts) %>% 
    left_join(note_class_concepts) %>% 
    left_join(note_encoding_concepts) %>% 
    left_join(note_language_concepts) %>% 
    left_join(provider_concepts) %>% 
    select('ID' = user_field(table_map, 'note', 'note_id'), everything()) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    mutate_if(is.character, str_replace_all, pattern = '\n', replacement = '<br>') %>% 
    rename_at(vars(-1), to_title_case)
}

## Observation -----
omop_table_observation <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Observation')
  subject <- as.integer(subject_id() )

  ## Build Concepts
  observation_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','observation_concept_id','Observation','person_id', subject)
  observation_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','observation_type_concept_id','Type','person_id', subject)
  observation_value_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','value_as_concept_id','Value','person_id', subject)
  observation_qualifier_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','qualifier_concept_id','Qualifier','person_id', subject)
  observation_unit_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','unit_concept_id','Unit','person_id', subject)
  provider_concepts <- get_subject_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'observation', 'observation_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Return Observation Table Representation
  user_table(table_map, db_connection, 'observation') %>% 
    filter(!!as.name(user_field(table_map, 'observation','person_id')) == subject ) %>% 
    select(-matches('person_id|observation_concept_id|observation_type_concept_id|value_as_concept_id|qualifier_concept_id|unit_concept_id|provider_id')) %>%
    left_join(observation_concepts) %>% 
    left_join(observation_type_concepts) %>% 
    left_join(observation_value_concepts) %>% 
    left_join(observation_qualifier_concepts) %>% 
    left_join(observation_unit_concepts) %>% 
    left_join(provider_concepts) %>% 
    select('ID' = user_field(table_map, 'observation','observation_id'),Observation, 'Date' = user_field(table_map, 'observation','observation_date'), 'DateTime' = user_field(table_map, 'observation','observation_datetime'),
           Type, 'ValueNum' = user_field(table_map, 'observation','value_as_number'), 'ValueString' = user_field(table_map, 'observation','value_as_string'), Value, 'SourceVal' = user_field(table_map, 'observation','observation_source_value'),
           Qualifier, Unit, Provider, 'Visit' = user_field(table_map, 'observation','visit_occurrence_id')) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Observation Period -----
omop_table_observation_period <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Observation Period')
  subject <- as.integer(subject_id() )
  
  ## Build Concepts
  observation_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation_period','observation_period_id','period_type_concept_id','Period','person_id', subject)
  
  ## Return Observation Period Table Representation
  user_table(table_map, db_connection, 'observation_period') %>% 
    filter(!!as.name(user_field(table_map, 'note','person_id')) == subject ) %>% 
    select(-matches('person_id|period_type_concept_id')) %>% 
    left_join(observation_type_concepts) %>% 
    select('ID' = user_field(table_map, 'observation_period','observation_period_id'), everything()) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Payer Plan Period -----
omop_table_payer_plan_period <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Payer Plan Period')
  subject <- as.integer(subject_id() )
  
  user_table(table_map, db_connection, 'payer_plan_period') %>% 
    filter(!!as.name(user_field(table_map, 'payer_plan_period','person_id')) == subject ) %>%
    select(-matches('person_id')) %>% 
    select('ID' = user_field(table_map, 'payer_plan_period', 'payer_plan_period_id'), everything()) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Procedure Occurrence -----
omop_table_procedure_occurrence <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Procedure Occurrence')
  subject <- as.integer(subject_id() )
  
  ## Build Concepts
  procedure_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','procedure_occurrence','procedure_occurrence_id','procedure_concept_id','Procedure','person_id', subject)
  procedure_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','procedure_occurrence','procedure_occurrence_id', 'procedure_type_concept_id','Type','person_id', subject)
  procedure_modifier_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','procedure_occurrence', 'procedure_occurrence_id', 'modifier_concept_id','Modifier','person_id', subject)
  provider_concepts <- get_subject_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'procedure_occurrence', 'procedure_occurrence_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Return Procedure Occurrence Table Representation
  user_table(table_map, db_connection, 'procedure_occurrence') %>% 
    filter(!!as.name(user_field(table_map, 'procedure_occurrence','person_id')) == subject ) %>% 
    select(-matches('person_id|procedure_concept_id|procedure_type_concept_id|modifier_concept_id|provider_id')) %>% 
    left_join(procedure_concepts) %>% 
    left_join(procedure_type_concepts) %>% 
    left_join(procedure_modifier_concepts) %>% 
    left_join(provider_concepts) %>% 
    select('ID' = user_field(table_map, 'procedure_occurrence','procedure_occurrence_id'), Procedure, 'SourceVal' = user_field(table_map, 'procedure_occurrence','procedure_source_value'),
           'Date' = user_field(table_map, 'procedure_occurrence','procedure_date'), Type, Modifier, everything()) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Specimen -----
omop_table_specimen <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Specimen')
  subject <- as.integer(subject_id() )
  
  ## Build Concepts
  specimen_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','specimen_concept_id','Specimen','person_id', subject)
  specimen_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','specimen_type_concept_id','Type','person_id', subject)
  specimen_unit_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','unit_concept_id','Unit','person_id', subject)
  anatomic_site_concepts<- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','anatomic_site_concept_id','AnatomicSite','person_id', subject)
  disease_status_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','disease_status_concept_id','DiseaseStatus','person_id', subject)
  
  ## Return Specimen Table Representation
  user_table(table_map, db_connection, 'specimen') %>% 
    filter(!!as.name(user_field(table_map, 'specimen','person_id')) == subject ) %>% 
    select(-matches('person_id|specimen_concept_id|specimen_type_concept_id|unit_concept_id|anatomic_site_concept_id|disease_status_concept_id')) %>% 
    left_join(specimen_concepts) %>% 
    left_join(specimen_type_concepts) %>% 
    left_join(specimen_unit_concepts) %>% 
    left_join(anatomic_site_concepts) %>% 
    left_join(disease_status_concepts) %>% 
    select('ID' = user_field(table_map, 'specimen', 'specimen_id'), Specimen, 'SourceVal' = user_field(table_map, 'specimen', 'specimen_source_value'),Type,'Date' = user_field(table_map, 'specimen', 'specimen_date'),
           'DateTime' = user_field(table_map, 'specimen', 'specimen_datetime'),everything()) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}
## Visit Occurrence -----
omop_table_visit_occurrence <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  message('Running Visit Occurrence')
  subject <- as.integer(subject_id() )
  
  ## Build Concepts
  visit_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','visit_occurrence','visit_occurrence_id','visit_concept_id','Visit','person_id', subject)
  visit_type_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','visit_occurrence','visit_occurrence_id','visit_type_concept_id','Type','person_id', subject)
  admitting_source_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','visit_occurrence','visit_occurrence_id','admitting_source_concept_id','AdmittingSource','person_id', subject)
  discharge_to_concepts <- get_subject_concept(table_map, db_connection, 'concept','concept_id','concept_name','visit_occurrence','visit_occurrence_id','discharge_to_concept_id','DischargeTo','person_id', subject)
  care_site_concepts <- get_subject_concept(table_map, db_connection, 'care_site','care_site_id','care_site_name','visit_occurrence','visit_occurrence_id','care_site_id','CareSite','person_id', subject)
  provider_concepts <- get_subject_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'visit_occurrence', 'visit_occurrence_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Return Visit Occurrence Table Representation
  user_table(table_map, db_connection, 'visit_occurrence') %>% 
    filter(!!as.name(user_field(table_map, 'visit_occurrence','person_id')) == subject ) %>% 
    select(-matches('person_id|visit_concept_id|visit_type_concept_id|admitting_source_concept_id|discharge_to_concept_id|care_site_id|provider_id')) %>% 
    left_join(visit_concepts) %>% 
    left_join(visit_type_concepts) %>% 
    left_join(admitting_source_concepts) %>% 
    left_join(discharge_to_concepts) %>% 
    left_join(care_site_concepts) %>% 
    left_join(provider_concepts) %>% 
    select('ID' = user_field(table_map, 'visit_occurrence', 'visit_occurrence_id'), Visit, 'StartDate' = user_field(table_map, 'visit_occurrence', 'visit_date'), 'StartDateTime' = user_field(table_map, 'visit_occurrence', 'visit_start_datetime'), 
           'EndDate' = user_field(table_map, 'visit_occurrence', 'visit_end_date'), 'EndDateTime' = user_field(table_map, 'visit_occurrence', 'visit_end_datetime'), Type, Provider, CareSite, AdmittingSource, DischargeTo, 
           'PrecedingVisit' = user_field(table_map, 'visit_occurrence', 'preceding_visit_occurrence_id')) %>% 
    arrange(ID) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}
