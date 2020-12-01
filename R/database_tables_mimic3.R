#' MIMIC Tables
#'
#' Collection of functions to create prearranged views of MIMIC patient data for ReviewR.
#' 
#' @param table_map tibble containing a the CDM that most closely matches the user's database and a map of standard tables to user tables
#' @param db_connection Connection info received from the database setup module
#'
#' @rdname mimic3_tables
#' @keywords internal
#' @export
#' @importFrom dplyr select everything arrange matches mutate_all mutate_if rename_at collect filter inner_join vars 
#' @importFrom snakecase to_title_case
#' @importFrom stringr str_replace regex str_replace_all
#' @importFrom rlang .data

## MIMIC All Patient Table ----

mimic3_table_all_patients <- function(table_map, db_connection) {
  user_table(table_map, db_connection, 'PATIENTS') %>% 
    select(ID = user_field(table_map, 'PATIENTS', 'SUBJECT_ID'), 
           .data$GENDER, 
           Birth_Datetime = user_field(table_map, 'PATIENTS', 'DOB'), 
           Death_Datetime = user_field(table_map, 'PATIENTS', 'DOD'), 
           Death_Hospital_DB = user_field(table_map, 'PATIENTS', 'DOD_HOSP'),
           Death_Social_Security_DB = user_field(table_map, 'PATIENTS', 'DOD_SSN'),
           everything()
           ) %>% 
    select(-matches('row_id', ignore.case = T)) %>% 
    arrange(.data$ID) %>% 
    mutate_all(as.character) %>% 
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC Admissions Table ----
#' @param subject_id The selected subject 
#'
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_admissions <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'ADMISSIONS') %>% 
    select(person_id = user_field(table_map, 'ADMISSIONS', 'SUBJECT_ID'), ID = user_field(table_map, 'ADMISSIONS', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'ADMISSIONS','ADMITTIME'))) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC Callout Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_callout <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'CALLOUT') %>% 
    select(person_id = user_field(table_map, 'CALLOUT', 'SUBJECT_ID'), ID = user_field(table_map, 'CALLOUT', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    arrange(!!as.name(user_field(table_map, 'CALLOUT','ROW_ID'))) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Chart Events Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_chart_events <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'CHARTEVENTS') %>%
    select(person_id = user_field(table_map, 'CHARTEVENTS', 'SUBJECT_ID'), ID = user_field(table_map, 'CHARTEVENTS', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject)  %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'CHARTEVENTS','CHARTTIME'))) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC CPT Events Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_cpt_events <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'CPTEVENTS') %>% 
    select(person_id = user_field(table_map, 'CPTEVENTS', 'SUBJECT_ID'), ID = user_field(table_map, 'CPTEVENTS', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject)  %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'CPTEVENTS','CHARTDATE'))) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC Diagnoses ICD Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_diagnoses_icd <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'DIAGNOSES_ICD') %>% 
    select(person_id = user_field(table_map, 'DIAGNOSES_ICD', 'SUBJECT_ID'), ID = user_field(table_map, 'DIAGNOSES_ICD', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    inner_join(user_table(table_map, db_connection, 'D_ICD_DIAGNOSES') %>% 
                 select(-matches('row_id', ignore.case = T))
               ) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'DIAGNOSES_ICD','SEQ_NUM'))) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC DRG Codes Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_drg_codes <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'DRGCODES') %>% 
    select(person_id = user_field(table_map, 'DRGCODES', 'SUBJECT_ID'), ID = user_field(table_map, 'DRGCODES', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    arrange(!!as.name(user_field(table_map, 'DRGCODES','ROW_ID'))) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC ICU Stays Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_icu_stays <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'ICUSTAYS') %>% 
    select(person_id = user_field(table_map, 'ICUSTAYS', 'SUBJECT_ID'), ID = user_field(table_map, 'ICUSTAYS', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'ICUSTAYS','INTIME'))) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Lab Events Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_lab_events <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'LABEVENTS') %>% 
    select(person_id = user_field(table_map, 'LABEVENTS', 'SUBJECT_ID'), ID = user_field(table_map, 'LABEVENTS', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    inner_join(user_table(table_map, db_connection, 'D_LABITEMS') %>% 
                 select(-matches('row_id', ignore.case = T))
               ) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'LABEVENTS','CHARTTIME'))) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = '^value',ignore_case = T),replacement = 'Value ')) %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Microbiology Events Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_microbiology_events <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'MICROBIOLOGYEVENTS') %>% 
    select(person_id = user_field(table_map, 'MICROBIOLOGYEVENTS', 'SUBJECT_ID'), ID = user_field(table_map, 'MICROBIOLOGYEVENTS', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'MICROBIOLOGYEVENTS','CHARTTIME'))) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Note Events Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_note_events <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'NOTEEVENTS') %>% 
    select(person_id = user_field(table_map, 'NOTEEVENTS', 'SUBJECT_ID'), ID = user_field(table_map, 'NOTEEVENTS', 'HADM_ID'), iserror = user_field(table_map, 'NOTEEVENTS', 'ISERROR'),everything()) %>% 
    filter(.data$person_id == subject & is.na(.data$iserror)) %>% 
    select(-matches('person_id|row_id|iserror', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'NOTEEVENTS','CHARTDATE'))) %>%
    mutate_all(as.character) %>%
    collect() %>%
    mutate_if(is.character, str_replace_all, pattern = '\n', replacement = '<br>') %>% 
    rename_at(vars(-1), to_title_case)
  }

## MIMIC Prescriptions Events MV Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_prescriptions <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'PRESCRIPTIONS') %>% 
    select(person_id = user_field(table_map, 'PRESCRIPTIONS', 'SUBJECT_ID'), ID = user_field(table_map, 'PRESCRIPTIONS', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'PRESCRIPTIONS','STARTDATE'))) %>% 
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Procedure Events MV Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_procedure_events <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'PROCEDUREEVENTS_MV') %>% 
    select(person_id = user_field(table_map, 'PROCEDUREEVENTS_MV', 'SUBJECT_ID'), ID = user_field(table_map, 'PROCEDUREEVENTS_MV', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    inner_join(user_table(table_map, db_connection, 'D_ITEMS') %>% 
                 select(-matches('row_id', ignore.case = T))
    ) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'PROCEDUREEVENTS_MV','STARTTIME'))) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = '^value',ignore_case = T),replacement = 'Value ')) %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Procedures ICD Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_procedures_icd <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'PROCEDURES_ICD') %>% 
    select(person_id = user_field(table_map, 'PROCEDURES_ICD', 'SUBJECT_ID'), ID = user_field(table_map, 'PROCEDURES_ICD', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    inner_join(user_table(table_map, db_connection, 'D_ICD_PROCEDURES') %>% 
                 select(-matches('row_id', ignore.case = T))
    ) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(.data$ID, !!as.name(user_field(table_map, 'PROCEDURES_ICD','SEQ_NUM'))) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = '^value',ignore_case = T),replacement = 'Value ')) %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Services Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_services <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  SERVICE = c("CMED","CSURG","DENT","ENT","GU","GYN","MED","NB","NBB","NMED","NSURG","OBS","ORTHO","OMED", "PSURG","PSYCH","SURG","TRAUMA","TSURG","VSURG")
  DESCRIPTION = c("Cardiac Medical - for non-surgical cardiac related admissions", "Cardiac Surgery - for surgical cardiac admissions", "Dental - for dental/jaw related admissions", "Ear, nose, and throat - conditions primarily affecting these areas", "Genitourinary - reproductive organs/urinary system", "Gynecological - female reproductive systems and breasts", "Medical - general service for internal medicine", "Newborn - infants born at the hospital", "Newborn baby - infants born at the hospital", "Neurologic Medical - non-surgical, relating to the brain", "Neurologic Surgical - surgical, relating to the brain", "Obstetrics - conerned with childbirth and the care of women giving birth", "Orthopaedic - surgical, relating to the musculoskeletal system", "Orthopaedic medicine - non-surgical, relating to musculoskeletal system", "Plastic - restortation/reconstruction of the human body (including cosmetic or aesthetic)", "Psychiatric - mental disorders relating to mood, behaviour, cognition, or perceptions", "Surgical - general surgical service not classified elsewhere", "Trauma - injury or damage caused by physical harm from an external source", "Thoracic Surgical - surgery on the thorax, located between the neck and the abdomen", "Vascular Surgical - surgery relating to the circulatory system")
  service_labels <- tibble(SERVICE, DESCRIPTION)
  user_table(table_map, db_connection, 'SERVICES') %>% 
    select(person_id = user_field(table_map, 'SERVICES', 'SUBJECT_ID'), ID = user_field(table_map, 'SERVICES', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'SERVICES','TRANSFERTIME'))) %>%
    mutate_all(as.character) %>%
    collect() %>%
    left_join(service_labels, 
              by = setNames('SERVICE', user_field(table_map, 'SERVICES', 'PREV_SERVICE'))) %>%
    rename('previous_service_description' = DESCRIPTION) %>% 
    left_join(service_labels, 
              by = setNames('SERVICE', user_field(table_map, 'SERVICES', 'CURR_SERVICE'))) %>% 
    rename('CURRENT_SERVICE_DESCRIPTION' = DESCRIPTION) %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC Transfers Table ----
#' @rdname mimic3_tables
#' @keywords internal
#' @export
mimic3_table_transfers <- function(table_map, db_connection, subject_id) {
  subject <- as.integer(subject_id)
  user_table(table_map, db_connection, 'TRANSFERS') %>% 
    select(person_id = user_field(table_map, 'TRANSFERS', 'SUBJECT_ID'), ID = user_field(table_map, 'TRANSFERS', 'HADM_ID'), everything()) %>% 
    filter(.data$person_id == subject) %>% 
    arrange(!!as.name(user_field(table_map, 'TRANSFERS','ROW_ID'))) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>%
    mutate_all(as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}
