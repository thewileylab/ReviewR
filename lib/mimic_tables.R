## Source function to extract tables and field names from table_map
library(snakecase)

## MIMIC All Patient Table ----

mimic_table_all_patients <- function(table_map, db_connection) {
  req(table_map(), db_connection() )
  user_table(table_map, db_connection, 'patients') %>% 
    select('ID' = user_field(table_map, 'patients', 'subject_id'), 
           GENDER, 
           'Birth_Datetime' = user_field(table_map, 'patients', 'dob'), 
           'Death_Datetime' = user_field(table_map, 'patients', 'dod'), 
           'Death_Hospital_DB' = user_field(table_map, 'patients', 'dod_hosp'),
           'Death_Social_Security_DB' = user_field(table_map, 'patients', 'dod_ssn'),
           everything()
           ) %>% 
    select(-matches('row_id', ignore.case = T)) %>% 
    arrange(ID) %>% 
    mutate_if(is.integer, as.character) %>% 
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC Admissions Table ----

mimic_table_admissions <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'admissions') %>% 
    select('person_id' = user_field(table_map, 'admissions', 'subject_id'), 'ID' = user_field(table_map, 'admissions', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'admissions','admittime'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC Callout Table ----

mimic_table_callout <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'callout') %>% 
    select('person_id' = user_field(table_map, 'callout', 'subject_id'), 'ID' = user_field(table_map, 'callout', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    arrange(!!as.name(user_field(table_map, 'callout','row_id'))) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Chart Events Table ----

mimic_table_chart_events <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'chartevents') %>%
    select('person_id' = user_field(table_map, 'chartevents', 'subject_id'), 'ID' = user_field(table_map, 'chartevents', 'hadm_id'), everything()) %>% 
    filter(person_id == subject)  %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'chartevents','charttime'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC CPT Events Table ----
mimic_table_cpt_events <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'cptevents') %>% 
    select('person_id' = user_field(table_map, 'cptevents', 'subject_id'), 'ID' = user_field(table_map, 'cptevents', 'hadm_id'), everything()) %>% 
    filter(person_id == subject)  %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'cptevents','chartdate'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC Diagnoses ICD Table ----

mimic_table_diagnoses_icd <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'diagnoses_icd') %>% 
    select('person_id' = user_field(table_map, 'diagnoses_icd', 'subject_id'), 'ID' = user_field(table_map, 'diagnoses_icd', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    inner_join(user_table(table_map, db_connection, 'd_icd_diagnoses') %>% 
                 select(-matches('row_id', ignore.case = T))
               ) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'diagnoses_icd','seq_num'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC DRG Codes Table ----

mimic_table_drg_codes <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'drgcodes') %>% 
    select('person_id' = user_field(table_map, 'drgcodes', 'subject_id'), 'ID' = user_field(table_map, 'drgcodes', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    arrange(!!as.name(user_field(table_map, 'drgcodes','row_id'))) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC ICU Stays Table ----

mimic_table_icu_stays <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'icustays') %>% 
    select('person_id' = user_field(table_map, 'icustays', 'subject_id'), 'ID' = user_field(table_map, 'icustays', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'icustays','intime'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Lab Events Table ----

mimic_table_lab_events <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection() )
  subject <- as.integer(subject_id(), subject_id() )
  user_table(table_map, db_connection, 'labevents') %>% 
    select('person_id' = user_field(table_map, 'labevents', 'subject_id'), 'ID' = user_field(table_map, 'labevents', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    inner_join(user_table(table_map, db_connection, 'd_labitems') %>% 
                 select(-matches('row_id', ignore.case = T))
               ) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'labevents','charttime'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = '^value',ignore_case = T),replacement = 'Value ')) %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Microbiology Events Table ----

mimic_table_microbiology_events <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id(), subject_id() )
  user_table(table_map, db_connection, 'microbiologyevents') %>% 
    select('person_id' = user_field(table_map, 'microbiologyevents', 'subject_id'), 'ID' = user_field(table_map, 'microbiologyevents', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'microbiologyevents','charttime'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Note Events Table ----

mimic_table_note_events <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'noteevents') %>% 
    select('person_id' = user_field(table_map, 'noteevents', 'subject_id'), 'ID' = user_field(table_map, 'noteevents', 'hadm_id'), 'iserror' = user_field(table_map, 'noteevents', 'iserror'),everything()) %>% 
    filter(person_id == subject & is.na(iserror)) %>% 
    select(-matches('person_id|row_id|iserror', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'noteevents','chartdate'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>%
    mutate_if(is.character, str_replace_all, pattern = '\n', replacement = '<br>') %>% 
    rename_at(vars(-1), to_title_case)
  }

## MIMIC Prescriptions Events MV Table ----

mimic_table_prescriptions <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'prescriptions') %>% 
    select('person_id' = user_field(table_map, 'prescriptions', 'subject_id'), 'ID' = user_field(table_map, 'prescriptions', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'prescriptions','startdate'))) %>% 
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Procedure Events MV Table ----

mimic_table_procedure_events <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'procedureevents_mv') %>% 
    select('person_id' = user_field(table_map, 'procedureevents_mv', 'subject_id'), 'ID' = user_field(table_map, 'procedureevents_mv', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    inner_join(user_table(table_map, db_connection, 'd_items') %>% 
                 select(-matches('row_id', ignore.case = T))
    ) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'procedureevents_mv','starttime'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = '^value',ignore_case = T),replacement = 'Value ')) %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Procedures ICD Table ----

mimic_table_procedures_icd <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'procedures_icd') %>% 
    select('person_id' = user_field(table_map, 'procedures_icd', 'subject_id'), 'ID' = user_field(table_map, 'procedures_icd', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    inner_join(user_table(table_map, db_connection, 'd_icd_procedures') %>% 
                 select(-matches('row_id', ignore.case = T))
    ) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(ID, !!as.name(user_field(table_map, 'procedures_icd','seq_num'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = '^value',ignore_case = T),replacement = 'Value ')) %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

## MIMIC Services Table ----

mimic_table_services <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  SERVICE = c("CMED","CSURG","DENT","ENT","GU","GYN","MED","NB","NBB","NMED","NSURG","OBS","ORTHO","OMED", "PSURG","PSYCH","SURG","TRAUMA","TSURG","VSURG")
  DESCRIPTION = c("Cardiac Medical - for non-surgical cardiac related admissions", "Cardiac Surgery - for surgical cardiac admissions", "Dental - for dental/jaw related admissions", "Ear, nose, and throat - conditions primarily affecting these areas", "Genitourinary - reproductive organs/urinary system", "Gynecological - female reproductive systems and breasts", "Medical - general service for internal medicine", "Newborn - infants born at the hospital", "Newborn baby - infants born at the hospital", "Neurologic Medical - non-surgical, relating to the brain", "Neurologic Surgical - surgical, relating to the brain", "Obstetrics - conerned with childbirth and the care of women giving birth", "Orthopaedic - surgical, relating to the musculoskeletal system", "Orthopaedic medicine - non-surgical, relating to musculoskeletal system", "Plastic - restortation/reconstruction of the human body (including cosmetic or aesthetic)", "Psychiatric - mental disorders relating to mood, behaviour, cognition, or perceptions", "Surgical - general surgical service not classified elsewhere", "Trauma - injury or damage caused by physical harm from an external source", "Thoracic Surgical - surgery on the thorax, located between the neck and the abdomen", "Vascular Surgical - surgery relating to the circulatory system")
  service_labels <- tibble(SERVICE, DESCRIPTION)
  user_table(table_map, db_connection, 'services') %>% 
    select('person_id' = user_field(table_map, 'services', 'subject_id'), 'ID' = user_field(table_map, 'services', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>% 
    arrange(!!as.name(user_field(table_map, 'services','transfertime'))) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>%
    left_join(service_labels, 
              by = setNames('SERVICE', user_field(table_map, 'services', 'prev_service'))) %>%
    rename('previous_service_description' = DESCRIPTION) %>% 
    left_join(service_labels, 
              by = setNames('SERVICE', user_field(table_map, 'services', 'curr_service'))) %>% 
    rename('current_service_description' = DESCRIPTION) %>% 
    rename_at(vars(-1), to_title_case)
}

## MIMIC Transfers Table ----

mimic_table_transfers <- function(table_map, db_connection, subject_id) {
  req(table_map(), db_connection(), subject_id() )
  subject <- as.integer(subject_id() )
  user_table(table_map, db_connection, 'transfers') %>% 
    select('person_id' = user_field(table_map, 'transfers', 'subject_id'), 'ID' = user_field(table_map, 'transfers', 'hadm_id'), everything()) %>% 
    filter(person_id == subject) %>% 
    arrange(!!as.name(user_field(table_map, 'transfers','row_id'))) %>% 
    select(-matches('person_id|row_id', ignore.case = T)) %>%
    mutate_if(is.integer, as.character) %>%
    collect() %>% 
    rename_at(vars(-1), to_title_case) %>% 
    rename_at(vars(-1), ~ str_replace(string = ., pattern = regex(pattern = 'id$',ignore_case = T),replacement = ' ID'))
}

