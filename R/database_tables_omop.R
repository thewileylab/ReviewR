# Helper Functions ----
#' Global Var Helper
#' 
#' https://github.com/tidyverse/magrittr/issues/29
#' 
#' @name helpr
#' @importFrom utils globalVariables
#' @keywords internal
#' @noRd
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' OMOP Get Concept
#' 
#' @description 
#' This function assists with transforming OMOP concept_ids to interpretable strings
#' by retrieving the requested concepts from the appropriate OMOP concept table. 
#' 
#' @param table_map A [dplyr::tibble] containing a mapping between the CDM standard
#' tables and fields to the user connected tables and fields.
#' @param db_connection A [DBI::dbConnect] object.
#' @param concept_table A string, containing the standard CDM concept table name.
#' @param concept_id A string, containing the standard CDM concept id field.
#' @param concept_name A string, containing the standard CDM concept name field.
#' @param table A string, containing the table name that requires OMOP concepts.
#' @param joinable_id A string, indicating what variable is "joinable" between the concept 
#' table and the desired table.
#' @param col_name A string, containing the desired name for the retrieved concept.
#' @param table_concept_id A string, containing the the table concept id
#' @param subject_id_field A string, identifying which table field contains the subject id.
#' @param selected_subject A numeric, or coercible to numeric containing the desired 
#' subject id.
#'
#' @keywords internal
#' 
#' @importFrom dplyr select inner_join rename contains mutate_all mutate_at
#' @importFrom stats setNames
#' @importFrom rlang := 
#' 
#' @return The desired OMOP concept based on the user data model for all subjects
#' 

get_concept <- function(table_map, db_connection, concept_table, concept_id, concept_name, table, joinable_id, table_concept_id, col_name, subject_id_field = NULL, selected_subject = NULL) {
  tryCatch({
    user_table(table_map, db_connection, concept_table) %>% 
      select(user_field(table_map, concept_table, concept_id), 
             user_field(table_map, concept_table, concept_name)
             ) %>% 
      mutate_at(vars(user_field(table_map, concept_table, concept_id)), as.character ) %>% 
      inner_join(user_table(table_map, db_connection, table) %>% 
                   {if(!is.null(selected_subject) ) filter(., !!as.name(user_field(table_map, table,subject_id_field)) == selected_subject ) else . } %>% 
                   select(user_field(table_map, table, joinable_id), 
                          user_field(table_map, table, table_concept_id)
                          ) %>% 
                   mutate_all(as.character), 
                 by=setNames(user_field(table_map, table, table_concept_id), user_field(table_map, concept_table, concept_id))
                 ) %>% 
      rename(!!col_name := user_field(table_map, concept_table, concept_name)) %>% 
      select(-contains(concept_id,ignore.case = T)) 
      }, 
    error=function(error) {
      return(NULL)
    })
  }

# Database Tables Documentation ----
#' OMOP Tables
#' 
#' @description 
#' Collection of functions to create prearranged views of OMOP patient data
#' when supplied with database connection information and a mapping of the connected 
#' database.
#' 
#' @param table_map A [dplyr::tibble] containing a mapping between the CDM standard
#' tables and fields to the user connected tables and fields.
#' @param db_connection A [DBI::dbConnect] object
#' @param subject_id A numeric, or coercible to numeric. 
#' @name omop_tables
#'
#' @keywords internal
#' 
#' @importFrom dplyr any_of arrange select everything matches mutate_all mutate_if rename_at collect filter inner_join vars left_join 
#' @importFrom snakecase to_title_case
#' @importFrom stringr str_replace regex str_replace_all
#' @importFrom rlang .data
#' @importFrom tidyr unite
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' 
#' @return A [dplyr::tibble] containing pre-coordinated patient information from the connected database.
#'
NULL
#> NULL
#' 

## OMOP All Patient Table -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_all_patients <- function(table_map, db_connection) {
  # browser()
  ## Build Concepts
  gender_concepts <- get_concept(table_map = table_map,  db_connection = db_connection,  concept_table = 'concept', concept_id = 'concept_id',  concept_name = 'concept_name', table = 'person', joinable_id = 'person_id', table_concept_id = 'gender_concept_id',  col_name = 'Gender')
  race_concepts <- get_concept(table_map = table_map, db_connection = db_connection, concept_table = 'concept', concept_id = 'concept_id', concept_name = 'concept_name', table = 'person', joinable_id = 'person_id', table_concept_id = 'race_concept_id', col_name = 'Race')
  ethnicity_concepts <- get_concept(table_map = table_map, db_connection = db_connection, concept_table = 'concept', concept_id = 'concept_id', concept_name = 'concept_name', table = 'person', joinable_id = 'person_id',table_concept_id = 'ethnicity_concept_id', col_name = 'Ethnicity')
  # provider_concepts <- get_concept(table_map = table_map,  db_connection = db_connection,  concept_table = 'provider', concept_id = 'provider_id', concept_name = 'provider_name', table = 'person', joinable_id = 'person_id', table_concept_id = 'provider_id', col_name = 'Provider')
                            
  ## Return All Patients Table Representation
  user_table(table_map, db_connection, 'person') %>% 
    select(-matches('gender*|race*|ethnicity*|provider*|location*|care*',ignore.case = T)) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(gender_concepts) ) left_join(., gender_concepts) else .} %>% 
    {if (!is.null(race_concepts) ) left_join(., race_concepts) else .} %>% 
    {if (!is.null(ethnicity_concepts) ) left_join(., ethnicity_concepts) else .} %>% 
    # {if (!is.null(provider_concepts) ) left_join(., provider_concepts) else .} %>%
    collect() %>% 
    unite(col = 'Birth_Date', 'year_of_birth','month_of_birth','day_of_birth', sep = '-') %>% 
    select(any_of(c(ID = user_field(table_map, 'person', 'person_id'), Gender = 'Gender', SourceVal = user_field(table_map, 'person', 'person_source_value'))), everything()) %>% 
    mutate_all(as.character) %>%
    arrange(as.numeric(.data$ID)) %>% 
    rename_at(vars(-1), to_title_case)
}

## OMOP Condition Era -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_condition_era <- function(table_map, db_connection, subject_id) {
  message('Running Condition Era')
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'concept') %>%
    select(user_field(table_map, 'concept', 'concept_id'),
           user_field(table_map, 'concept', 'concept_name')
           ) %>%
    mutate_at(vars(user_field(table_map, 'concept', 'concept_id')), as.character ) %>% 
    inner_join(user_table(table_map, db_connection, 'condition_era') %>%
                 filter(!!as.name(user_field(table_map, 'condition_era','person_id')) == subject ) %>% 
                 mutate_all(as.character),
               by=setNames(user_field(table_map, 'condition_era', 'condition_concept_id'), user_field(table_map, 'concept', 'concept_id'))
               ) %>%
    rename('Condition' = user_field(table_map, 'concept', 'concept_name')) %>%
    select(-matches('concept*|person*',ignore.case = T)) %>% 
    select(any_of(c(ID = user_field(table_map, 'condition_era','condition_era_id'))), everything()) %>% 
    mutate_all(as.character) %>%
    arrange(as.numeric(.data$ID)) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## OMOP Condition Occurrence -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_condition_occurrence <- function(table_map, db_connection, subject_id) {
  message('Running Condition Occurrence')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  condition_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','condition_occurrence','condition_occurrence_id','condition_concept_id','Condition','person_id', subject)
  condition_type_concepts <- get_concept(table_map, db_connection, 'concept', 'concept_id','concept_name','condition_occurrence','condition_occurrence_id','condition_type_concept_id','Type','person_id', subject)
  condition_status_concepts <- get_concept(table_map, db_connection, 'concept', 'concept_id','concept_name','condition_occurrence','condition_occurrence_id','condition_status_concept_id','Status','person_id', subject)
  # condition_provider_concepts <- get_concept(table_map, db_connection, 'provider', 'provider_id','provider_name','condition_occurrence','condition_occurrence_id','provider_id','Provider','person_id', subject)
  
  ## Return Condition Occurrence Table Representation  
  user_table(table_map, db_connection, 'condition_occurrence') %>% 
    filter(!!as.name(user_field(table_map, 'condition_occurrence','person_id')) == subject ) %>% 
    select(-matches('person*|condition_concept*|condition_status*|provider_id|condition_type*|condition_source_concept*',ignore.case = T)) %>% 
    mutate_all(as.character) %>% 
    {if (!is.null(condition_concepts) ) left_join(., condition_concepts) else .} %>% 
    {if (!is.null(condition_status_concepts) ) left_join(., condition_status_concepts) else .} %>% 
    # {if (!is.null(condition_provider_concepts) ) left_join(., condition_provider_concepts) else .} %>% 
    {if (!is.null(condition_type_concepts) ) left_join(., condition_type_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map,'condition_occurrence','condition_occurrence_id'), Condition = 'Condition', SourceVal = user_field(table_map,'condition_occurrence','condition_source_value'), Status = 'Status', Type = 'Type', Provider = 'Provider', Visit = user_field(table_map,'condition_occurrence','visit_occurrence_id'))), everything()) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## OMOP Death -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_death <- function(table_map, db_connection, subject_id) {
  message('Running Death')
  subject <- as.integer(subject_id)

  ## Build Concepts
  death_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','death','person_id','death_type_concept_id','Type','person_id', subject)
  death_cause_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','death','person_id','cause_concept_id','Cause','person_id', subject)
  
  ## Return Death Table Representation 
  user_table(table_map, db_connection, 'death') %>% 
    filter(!!as.name(user_field(table_map, 'death','person_id')) == subject ) %>% 
    select(-matches('death_type_concept*|cause_concept*|cause_source_concept*', ignore.case = T)) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(death_cause_concepts) ) left_join(., death_cause_concepts) else .} %>% 
    {if (!is.null(death_type_concepts) ) left_join(., death_type_concepts) else .} %>% 
    select(any_of(c(ID = 'person_id', SourceVal = user_field(table_map, 'death', 'cause_source_value'))), everything()) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Device Exposure -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_device_exposure <- function(table_map, db_connection, subject_id) {
  message('Running Device Exposure')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  device_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','device_exposure','device_exposure_id','device_concept_id','Device','person_id', subject)
  device_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','device_exposure','device_exposure_id','device_type_concept_id','Type','person_id', subject)
  # device_provider_concepts <- get_concept(table_map, db_connection, 'provider','provider_id','provider_name','device_exposure','device_exposure_id','provider_id','Provider','person_id', subject)

  ## Return Device Exposure Table Representation
  user_table(table_map, db_connection, 'device_exposure') %>% 
    filter(!!as.name(user_field(table_map, 'device_exposure','person_id')) == subject ) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(device_concepts) ) left_join(., device_concepts) else .} %>% 
    {if (!is.null(device_type_concepts) ) left_join(., device_type_concepts) else .} %>% 
    # {if (!is.null(device_provider_concepts) ) left_join(., device_provider_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'device_exposure','device_exposure_id'))), everything()) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}
 
## Dose Era -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_dose_era <- function(table_map, db_connection, subject_id) {
  message('Running Dose Era')
  subject <- as.integer(subject_id)

  ## Build Concepts
  dose_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','dose_era','dose_era_id','drug_concept_id','Drug','person_id', subject)
  unit_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','dose_era','dose_era_id','unit_concept_id','Unit','person_id', subject)

  ## Return Dose Era Table Representation
  user_table(table_map, db_connection, 'dose_era') %>% 
    filter(!!as.name(user_field(table_map, 'dose_era','person_id')) == subject ) %>% 
    select(-matches('person_id|drug_concept_id|unit_concept_id')) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(dose_concepts) ) left_join(., dose_concepts) else .} %>% 
    {if (!is.null(unit_concepts) ) left_join(., unit_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'dose_era', 'dose_era_id'), Drug = 'Drug', Unit = 'Unit', DoseValue = 'dose_value')), everything()) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Drug Era -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_drug_era <- function(table_map, db_connection, subject_id) {
  message('Running Drug Era')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  drug_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','drug_era','drug_era_id','drug_concept_id','Drug','person_id', subject)
  
  ## Return Drug Era Table Representation
  user_table(table_map, db_connection, 'drug_era') %>% 
    filter(!!as.name(user_field(table_map, 'drug_era','person_id')) == subject ) %>% 
    select(-matches('person_id|drug_concept_id')) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(drug_concepts) ) left_join(., drug_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'drug_era', 'drug_era_id'), Drug = 'Drug')), everything()) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
  
}

## Drug Exposure -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_drug_exposure <- function(table_map, db_connection, subject_id) {
  message('Running Drug Exposure')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  drug_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','drug_exposure','drug_exposure_id','drug_concept_id','Drug','person_id', subject)
  drug_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','drug_exposure','drug_exposure_id','drug_type_concept_id','Type','person_id', subject)
  route_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','drug_exposure','drug_exposure_id','route_concept_id','Route','person_id', subject)
  # provider_concepts <- get_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'drug_exposure', 'drug_exposure_id', 'provider_id', 'Provider', 'person_id', subject)

  ## Return Drug Exposure Table Representation
  user_table(table_map, db_connection, 'drug_exposure') %>% 
    filter(!!as.name(user_field(table_map, 'drug_era','person_id')) == subject ) %>%
    select(-matches('person_id|drug_concept_id|drug_type_concept_id|route_concept_id|provider_id|visit_detail_id|drug_source_concept_id|route_source_value|dose_unit_source_value')) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(drug_concepts) ) left_join(., drug_concepts) else .} %>% 
    {if (!is.null(drug_type_concepts) ) left_join(., drug_type_concepts) else .} %>% 
    {if (!is.null(route_concepts) ) left_join(., route_concepts) else .} %>% 
    # {if (!is.null(provider_concepts) ) left_join(., provider_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'drug_exposure', 'drug_exposure_id'), Drug = 'Drug', SourceVal = user_field(table_map, 'drug_exposure', 'drug_source_value'), StartDate = user_field(table_map, 'drug_exposure', 'drug_exposure_start_date'),
           StartDateTime = user_field(table_map, 'drug_exposure','drug_exposure_start_datetime'), EndDate = user_field(table_map, 'drug_exposure', 'drug_exposure_end_date'), EndDateTime = user_field(table_map, 'drug_exposure', 'drug_exposure_end_datetime'),
           VerbatimEnd = user_field(table_map, 'drug_exposure', 'drug_exposure_verbatim_end_date'), Type = 'Type', Visit = user_field(table_map, 'drug_exposure', 'visit_occurrence_id'))), everything() ) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
  
}

## Measurement -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_measurement <- function(table_map, db_connection, subject_id) {
  message('Running Measurement')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  measurement_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','measurement_concept_id','Measurement','person_id', subject)
  measurement_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','measurement_type_concept_id','Type','person_id', subject)
  operator_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','operator_concept_id','Operator','person_id', subject)
  value_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','value_as_concept_id','Value','person_id', subject)
  unit_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','measurement','measurement_id','unit_concept_id','Unit','person_id', subject)
  # provider_concepts <- get_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'measurement', 'measurement_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Return Drug Exposure Table Representation
  user_table(table_map, db_connection, 'measurement') %>% 
    filter(!!as.name(user_field(table_map, 'drug_era','person_id')) == subject ) %>% 
    select(-matches('person_id|measurement_concept_id|measurement_type_concept_id|operator_concept_id|value_as_concept_id|unit_concept_id|provider_id')) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(measurement_concepts) ) left_join(., measurement_concepts) else .} %>% 
    {if (!is.null(measurement_type_concepts) ) left_join(., measurement_type_concepts) else .} %>% 
    {if (!is.null(operator_concepts) ) left_join(., operator_concepts) else .} %>% 
    {if (!is.null(value_concepts) ) left_join(., value_concepts) else .} %>% 
    {if (!is.null(unit_concepts) ) left_join(., unit_concepts) else .} %>% 
    # {if (!is.null(provider_concepts) ) left_join(., provider_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'measurement', 'measurement_id'))), everything() ) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Note -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_note <- function(table_map, db_connection, subject_id) {
  message('Running Note')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  note_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','note','note_id','note_type_concept_id','Type','person_id', subject)
  note_class_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','note','note_id','note_class_concept_id','Class','person_id', subject)
  note_encoding_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','note','note_id','encoding_concept_id','Encoding','person_id', subject)
  note_language_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','note','note_id','language_concept_id','Language','person_id', subject)
  # provider_concepts <- get_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'note', 'note_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Return Note Table Representation
  user_table(table_map, db_connection, 'note') %>% 
    filter(!!as.name(user_field(table_map, 'note','person_id')) == subject ) %>% 
    select(-matches('person_id|note_type_concept_id|note_class_concept_id|encoding_concept_id|language_concept_id|provider_id')) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(note_type_concepts) ) left_join(., note_type_concepts) else .} %>% 
    {if (!is.null(note_class_concepts) ) left_join(., note_class_concepts) else .} %>% 
    {if (!is.null(note_encoding_concepts) ) left_join(., note_encoding_concepts) else .} %>% 
    {if (!is.null(note_language_concepts) ) left_join(., note_language_concepts) else .} %>% 
    # {if (!is.null(provider_concepts) ) left_join(., provider_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'note', 'note_id'))), everything()) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    mutate_all(str_replace_all, pattern = '\n', replacement = '<br>') %>% 
    rename_at(vars(-1), to_title_case)
}

## Observation -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_observation <- function(table_map, db_connection, subject_id) {
  message('Running Observation')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  observation_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','observation_concept_id','Observation','person_id', subject)
  observation_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','observation_type_concept_id','Type','person_id', subject)
  observation_value_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','value_as_concept_id','Value','person_id', subject)
  observation_qualifier_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','qualifier_concept_id','Qualifier','person_id', subject)
  observation_unit_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation','observation_id','unit_concept_id','Unit','person_id', subject)
  # provider_concepts <- get_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'observation', 'observation_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Return Observation Table Representation
  user_table(table_map, db_connection, 'observation') %>% 
    filter(!!as.name(user_field(table_map, 'observation','person_id')) == subject ) %>% 
    select(-matches('person_id|observation_concept_id|observation_type_concept_id|value_as_concept_id|qualifier_concept_id|unit_concept_id|provider_id')) %>%
    mutate_all(as.character) %>%
    {if (!is.null(observation_concepts) ) left_join(., observation_concepts) else .} %>% 
    {if (!is.null(observation_type_concepts) ) left_join(., observation_type_concepts) else .} %>% 
    {if (!is.null(observation_value_concepts) ) left_join(., observation_value_concepts) else .} %>% 
    {if (!is.null(observation_qualifier_concepts) ) left_join(., observation_qualifier_concepts) else .} %>% 
    {if (!is.null(observation_unit_concepts) ) left_join(., observation_unit_concepts) else .} %>% 
    # {if (!is.null(provider_concepts) ) left_join(., provider_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'observation','observation_id'), Observation = 'Observation', Date = user_field(table_map, 'observation','observation_date'), DateTime = user_field(table_map, 'observation','observation_datetime'),
           Type = 'Type', ValueNum = user_field(table_map, 'observation','value_as_number'), ValueString = user_field(table_map, 'observation','value_as_string'), Value = 'Value', SourceVal = user_field(table_map, 'observation','observation_source_value'),
           Qualifier = 'Qualifier', Unit = 'Unit', Provider = 'Provider', Visit = user_field(table_map, 'observation','visit_occurrence_id')))) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Observation Period -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_observation_period <- function(table_map, db_connection, subject_id) {
  message('Running Observation Period')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  observation_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','observation_period','observation_period_id','period_type_concept_id','Period','person_id', subject)
  
  ## Return Observation Period Table Representation
  user_table(table_map, db_connection, 'observation_period') %>% 
    filter(!!as.name(user_field(table_map, 'observation_period','person_id')) == subject ) %>% 
    select(-matches('person_id|period_type_concept_id')) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(observation_type_concepts) ) left_join(., observation_type_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'observation_period','observation_period_id'))), everything()) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Payer Plan Period -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_payer_plan_period <- function(table_map, db_connection, subject_id) {
  message('Running Payer Plan Period')
  subject <- as.integer(subject_id)
  
  user_table(table_map, db_connection, 'payer_plan_period') %>% 
    filter(!!as.name(user_field(table_map, 'payer_plan_period','person_id')) == subject ) %>%
    select(-matches('person_id')) %>% 
    mutate_all(as.character) %>%
    select(any_of(c(ID = user_field(table_map, 'payer_plan_period', 'payer_plan_period_id'))), everything()) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Procedure Occurrence -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_procedure_occurrence <- function(table_map, db_connection, subject_id) {
  message('Running Procedure Occurrence')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  procedure_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','procedure_occurrence','procedure_occurrence_id','procedure_concept_id','Procedure','person_id', subject)
  procedure_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','procedure_occurrence','procedure_occurrence_id', 'procedure_type_concept_id','Type','person_id', subject)
  procedure_modifier_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','procedure_occurrence', 'procedure_occurrence_id', 'modifier_concept_id','Modifier','person_id', subject)
  # provider_concepts <- get_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'procedure_occurrence', 'procedure_occurrence_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Return Procedure Occurrence Table Representation
  user_table(table_map, db_connection, 'procedure_occurrence') %>% 
    filter(!!as.name(user_field(table_map, 'procedure_occurrence','person_id')) == subject ) %>% 
    select(-matches('person_id|procedure_concept_id|procedure_type_concept_id|modifier_concept_id|provider_id')) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(procedure_concepts) ) left_join(., procedure_concepts) else .} %>% 
    {if (!is.null(procedure_type_concepts) ) left_join(., procedure_type_concepts) else .} %>% 
    {if (!is.null(procedure_modifier_concepts) ) left_join(., procedure_modifier_concepts) else .} %>% 
    # {if (!is.null(provider_concepts) ) left_join(., provider_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'procedure_occurrence','procedure_occurrence_id'), Procedure = 'Procedure', SourceVal = user_field(table_map, 'procedure_occurrence','procedure_source_value'),
           Date = user_field(table_map, 'procedure_occurrence','procedure_date'), Type = 'Type', Modifier = 'Modifier')), everything()) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## Specimen -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_specimen <- function(table_map, db_connection, subject_id) {
  message('Running Specimen')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  specimen_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','specimen_concept_id','Specimen','person_id', subject)
  specimen_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','specimen_type_concept_id','Type','person_id', subject)
  specimen_unit_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','unit_concept_id','Unit','person_id', subject)
  anatomic_site_concepts<- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','anatomic_site_concept_id','AnatomicSite','person_id', subject)
  disease_status_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','specimen','specimen_id','disease_status_concept_id','DiseaseStatus','person_id', subject)
  
  ## Return Specimen Table Representation
  user_table(table_map, db_connection, 'specimen') %>% 
    filter(!!as.name(user_field(table_map, 'specimen','person_id')) == subject ) %>% 
    select(-matches('person_id|specimen_concept_id|specimen_type_concept_id|unit_concept_id|anatomic_site_concept_id|disease_status_concept_id')) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(specimen_concepts) ) left_join(., specimen_concepts) else .} %>% 
    {if (!is.null(specimen_type_concepts) ) left_join(., specimen_type_concepts) else .} %>% 
    {if (!is.null(specimen_unit_concepts) ) left_join(., specimen_unit_concepts) else .} %>% 
    {if (!is.null(anatomic_site_concepts) ) left_join(., anatomic_site_concepts) else .} %>% 
    {if (!is.null(disease_status_concepts) ) left_join(., disease_status_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'specimen', 'specimen_id'), Specimen = 'Specimen', SourceVal = user_field(table_map, 'specimen', 'specimen_source_value'), Type = 'Type', Date = user_field(table_map, 'specimen', 'specimen_date'),
           DateTime = user_field(table_map, 'specimen', 'specimen_datetime'))), everything()) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}
## Visit Occurrence -----
#' @rdname omop_tables
#' @keywords internal
#' 
omop_table_visit_occurrence <- function(table_map, db_connection, subject_id) {
  message('Running Visit Occurrence')
  subject <- as.integer(subject_id)
  
  ## Build Concepts
  visit_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','visit_occurrence','visit_occurrence_id','visit_concept_id','Visit','person_id', subject)
  visit_type_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','visit_occurrence','visit_occurrence_id','visit_type_concept_id','Type','person_id', subject)
  admitting_source_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','visit_occurrence','visit_occurrence_id','admitting_source_concept_id','AdmittingSource','person_id', subject)
  discharge_to_concepts <- get_concept(table_map, db_connection, 'concept','concept_id','concept_name','visit_occurrence','visit_occurrence_id','discharge_to_concept_id','DischargeTo','person_id', subject)
  care_site_concepts <- get_concept(table_map, db_connection, 'care_site','care_site_id','care_site_name','visit_occurrence','visit_occurrence_id','care_site_id','CareSite','person_id', subject)
  # provider_concepts <- get_concept(table_map, db_connection, 'provider', 'provider_id', 'provider_name', 'visit_occurrence', 'visit_occurrence_id', 'provider_id', 'Provider', 'person_id', subject)
  
  ## Return Visit Occurrence Table Representation
  user_table(table_map, db_connection, 'visit_occurrence') %>% 
    filter(!!as.name(user_field(table_map, 'visit_occurrence','person_id')) == subject ) %>% 
    select(-matches('person_id|visit_concept_id|visit_type_concept_id|admitting_source_concept_id|discharge_to_concept_id|care_site_id|provider_id')) %>% 
    mutate_all(as.character) %>%
    {if (!is.null(visit_concepts) ) left_join(., visit_concepts) else .} %>% 
    {if (!is.null(visit_type_concepts) ) left_join(., visit_type_concepts) else .} %>% 
    {if (!is.null(admitting_source_concepts) ) left_join(., admitting_source_concepts) else .} %>% 
    {if (!is.null(care_site_concepts) ) left_join(., discharge_to_concepts) else .} %>% 
    {if (!is.null(care_site_concepts) ) left_join(., care_site_concepts) else .} %>% 
    # {if (!is.null(provider_concepts) ) left_join(., provider_concepts) else .} %>% 
    select(any_of(c(ID = user_field(table_map, 'visit_occurrence', 'visit_occurrence_id'), Visit = 'Visit', StartDate = user_field(table_map, 'visit_occurrence', 'visit_date'), StartDateTime = user_field(table_map, 'visit_occurrence', 'visit_start_datetime'), 
           EndDate = user_field(table_map, 'visit_occurrence', 'visit_end_date'), EndDateTime = user_field(table_map, 'visit_occurrence', 'visit_end_datetime'), Type = 'Type', Provider = 'Provider', CareSite = 'CareSite', AdmittingSource = 'AdmittingSource', DischargeTo = 'DischargeTo', 
           PrecedingVisit = user_field(table_map, 'visit_occurrence', 'preceding_visit_occurrence_id')))) %>% 
    mutate_all(as.character) %>% 
    arrange(as.numeric(.data$ID) ) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}
