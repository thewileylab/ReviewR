
format_table_name <- function(table_key, table_config, db_config) {
  paste0(db_config["schema"], ".", table_config[table_key])
}

query_admissions <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select row_id, hadm_id, admittime, dischtime, deathtime, admission_type, admission_location, discharge_location, insurance, language, religion, marital_status, ethnicity, edregtime, edouttime, diagnosis, hospital_expire_flag, has_chartevents_data from ",
                       format_table_name("admissions", table_config, db_config),
                       " where SUBJECT_ID = ", as.numeric(input$subject_id),
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(hadm_id = paste0("<a class='row_hadm_id' href='#'>", hadm_id, "</a>"))
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(hadm_id = paste0("<a class='row_hadm_id' href='#'>", hadm_id, "</a>"))
  }
}

query_callout <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select row_id, submit_wardid, submit_careunit, curr_wardid, curr_careunit, callout_wardid, callout_service, request_tele, request_resp, request_cdiff, request_mrsa, request_vre, callout_status, callout_outcome, discharge_wardid, acknowledge_status, createtime, updatetime, acknowledgetime, outcometime, firstreservationtime, currentreservationtime from ",
                       format_table_name("callout", table_config, db_config),
                       " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(row_id)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(row_id)
  }
}

query_chartevents <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select a.ROW_ID, a.ICUSTAY_ID, a.CHARTTIME, a.STORETIME, a.CGID, b.LABEL, a.VALUE, a.VALUENUM, a.VALUEUOM, a.WARNING, a.ERROR, a.RESULTSTATUS, a.STOPPED, b.DBSOURCE, b.CATEGORY, b.UNITNAME from ",
                       format_table_name("chartevents", table_config, db_config),
                       " a inner join ",
                       format_table_name("d_items", table_config, db_config),
                       " b on a.ITEMID=b.ITEMID where SUBJECT_ID = ", as.numeric(input$subject_id),
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      select(ROW_ID = a_ROW_ID, ICUSTAY_ID = a_ICUSTAY_ID, CHARTTIME = a_CHARTTIME, STORETIME = a_STORETIME, CGID = a_CGID, LABEL = b_LABEL, VALUE = a_VALUE, VALUENUM = a_VALUENUM, VALUEUOM = a_VALUEUOM, WARNING = a_WARNING, ERROR = a_ERROR, RESULTSTATUS = a_RESULTSTATUS, STOPPED = a_STOPPED, DBSOURCE = b_DBSOURCE, CATEGORY = b_CATEGORY, UNITNAME = b_UNITNAME) %>%
      mutate(LABEL = as.factor(LABEL), CATEGORY = as.factor(CATEGORY)) %>%
      arrange(CHARTTIME)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      select(row_id, icustay_id, charttime, storetime, cgid, label, value, valuenum, valueuom, warning, error, resultstatus, stopped, dbsource, category, unitname) %>%
      mutate(label = as.factor(label), category = as.factor(category)) %>%
      arrange(charttime)
  }
}

query_cptevents <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select row_id, costcenter, chartdate, cpt_cd, cpt_number, cpt_suffix, ticket_id_seq, sectionheader, subsectionheader, description from ",
                       format_table_name("cptevents", table_config, db_config),
                       " where SUBJECT_ID = ", as.numeric(input$subject_id),
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(chartdate)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(chartdate)
  }
}

query_diagnoses_icd <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select a.row_id, a.seq_num, a.icd9_code, b.long_title from ",
                       format_table_name("diagnoses_icd", table_config, db_config),
                       " a inner join ",
                       format_table_name("d_icd_diagnoses", table_config, db_config),
                       " b on a.ICD9_CODE = b.ICD9_CODE where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      select(row_id = a_row_id, seq_num = a_seq_num, icd9_code = a_icd9_code, long_title = b_long_title) %>% arrange(seq_num)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      arrange(seq_num)
  }
}

query_drgcodes <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select row_id, drg_type, drg_code, description, drg_severity, drg_mortality from ",
                       format_table_name("drgcodes", table_config, db_config),
                       " where SUBJECT_ID = ", 
                       as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(row_id)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(row_id)
  }
}

query_icustays <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select row_id, icustay_id, dbsource, first_careunit, last_careunit, first_wardid, last_wardid, intime, outtime, los from ",
                       format_table_name("icustays", table_config, db_config),
                       " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(row_id)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(row_id)
  }
}

query_labevents <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select a.ROW_ID, a.CHARTTIME, b.LABEL, b.FLUID, b.CATEGORY, a.VALUE, a.VALUENUM, a.VALUEUOM, a.FLAG from ",
                       format_table_name("labevents", table_config, db_config),
                       " a inner join ",
                       format_table_name("d_labitems", table_config, db_config),
                       " b on a.ITEMID=b.ITEMID where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(LABEL = as.factor(b_LABEL), CATEGORY = as.factor(b_CATEGORY)) %>%
      arrange(CHARTTIME)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(label = as.factor(label), category = as.factor(category)) %>%
      arrange(charttime)
  }
}

query_microbiologyevents <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select row_id, chartdate, charttime, spec_itemid, spec_type_desc, org_itemid, org_name, isolate_num, ab_itemid, ab_name, dilution_text, dilution_comparison, dilution_value, interpretation from ",
                       format_table_name("microbiologyevents", table_config, db_config),
                       " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      arrange(charttime)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      arrange(charttime)
  }
}

query_noteevent <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select chartdate, charttime, storetime, category, description, cgid, iserror, text from ",
                       format_table_name("noteevent", table_config, db_config),
                       " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      filter(is.na(iserror)) %>% 
      select(-iserror) %>% 
      group_by(chartdate, charttime, storetime, category, cgid, text) %>% 
      arrange(description) %>% 
      summarise(description = paste(description,collapse = "; ")) %>% 
      ungroup() %>% 
      mutate(category = as.factor(category), description = as.factor(description)) %>% 
      arrange(chartdate)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      filter(is.na(iserror)) %>% 
      select(-iserror) %>% 
      group_by(chartdate, charttime, storetime, category, cgid, text) %>% 
      arrange(description) %>% 
      summarise(description = paste(description,collapse = "; ")) %>% 
      ungroup() %>% 
      mutate(category = as.factor(category), description = as.factor(description)) %>% 
      arrange(chartdate)
  }
}

query_patients <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select row_id, gender, dob, dod, dod_hosp, dod_ssn, expire_flag from ",
                       format_table_name("patients", table_config, db_config),
                       " where SUBJECT_ID = ", as.numeric(input$subject_id))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

query_all_patients <- function(table_config, db_config, database_type, connection) {
  query_text <- paste0("select subject_id, gender, dob, dod, dod_hosp, dod_ssn, expire_flag from ",
                       format_table_name("patients", table_config, db_config))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(subject_id = paste0("<a class='row_subject_id' href='#'>", subject_id, "</a>"))
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(subject_id = paste0("<a class='row_subject_id' href='#'>", subject_id, "</a>"))
  }
}


query_prescriptions <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select row_id, icustay_id, startdate, enddate, drug_type, drug, drug_name_poe, drug_name_generic, formulary_drug_cd, gsn, ndc, prod_strength, dose_val_rx, dose_unit_rx, form_val_disp, form_unit_disp, route from ",
                       format_table_name("prescriptions", table_config, db_config),
                       " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(drug = as.factor(drug), form_unit_disp = as.factor(form_unit_disp), route = as.factor(route)) %>%
      arrange(startdate)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(drug = as.factor(drug), form_unit_disp = as.factor(form_unit_disp), route = as.factor(route)) %>%
      arrange(startdate)
  }
}

query_procedureevents_mv <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select a.ROW_ID, a.ICUSTAY_ID, a.STARTTIME, a.ENDTIME, b.LABEL, b.DBSOURCE, a.VALUE, a.VALUEUOM, a.LOCATION, a.LOCATIONCATEGORY, a.STORETIME, a.CGID, a.ORDERID, a.LINKORDERID, a.ORDERCATEGORYNAME, a.SECONDARYORDERCATEGORYNAME, a.ORDERCATEGORYDESCRIPTION, a.ISOPENBAG, a.CONTINUEINNEXTDEPT, a.CANCELREASON, a.STATUSDESCRIPTION, a.COMMENTS_EDITEDBY, a.COMMENTS_CANCELEDBY, a.COMMENTS_DATE from ",
                       format_table_name("procedureevents", table_config, db_config),
                       " a inner join ",
                       format_table_name("d_items", table_config, db_config),
                       " b on a.ITEMID = b.ITEMID where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(ORDERCATEGORYNAME = as.factor(a_ORDERCATEGORYNAME), SECONDARYORDERCATEGORYNAME = as.factor(a_SECONDARYORDERCATEGORYNAME), ORDERCATEGORYDESCRIPTION = as.factor(a_ORDERCATEGORYDESCRIPTION)) %>%
      arrange(STARTTIME)
    
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(ordercategoryname = as.factor(ordercategoryname), secondaryordercategoryname = as.factor(secondaryordercategoryname), ordercategorydescription = as.factor(ordercategorydescription)) %>%
      arrange(starttime)
  }
}

query_procedures_icd <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select a.ROW_ID, a.SEQ_NUM, a.ICD9_CODE, b.LONG_TITLE from ",
                       format_table_name("procedures_icd", table_config, db_config),
                       " a inner join ",
                       format_table_name("d_icd_procedures", table_config, db_config),
                       " b on a.ICD9_CODE = b.ICD9_CODE where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(a_SEQ_NUM)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(seq_num)
  }
}

query_services <- function(table_config, db_config, input, database_type, connection) {
  services_labels <- data.frame(
    SERVICE = c("CMED","CSURG","DENT","ENT","GU","GYN","MED","NB","NBB","NMED","NSURG","OBS","ORTHO","OMED", "PSURG","PSYCH","SURG","TRAUMA","TSURG","VSURG"),
    DESCRIPTION = c("Cardiac Medical - for non-surgical cardiac related admissions", "Cardiac Surgery - for surgical cardiac admissions", "Dental - for dental/jaw related admissions", "Ear, nose, and throat - conditions primarily affecting these areas", "Genitourinary - reproductive organs/urinary system", "Gynecological - female reproductive systems and breasts", "Medical - general service for internal medicine", "Newborn - infants born at the hospital", "Newborn baby - infants born at the hospital", "Neurologic Medical - non-surgical, relating to the brain", "Neurologic Surgical - surgical, relating to the brain", "Obstetrics - conerned with childbirth and the care of women giving birth", "Orthopaedic - surgical, relating to the musculoskeletal system", "Orthopaedic medicine - non-surgical, relating to musculoskeletal system", "Plastic - restortation/reconstruction of the human body (including cosmetic or aesthetic)", "Psychiatric - mental disorders relating to mood, behaviour, cognition, or perceptions", "Surgical - general surgical service not classified elsewhere", "Trauma - injury or damage caused by physical harm from an external source", "Thoracic Surgical - surgery on the thorax, located between the neck and the abdomen", "Vascular Surgical - surgery relating to the circulatory system"))
  query_text <- paste0("select row_id, transfertime, prev_service, curr_service from ",
                       format_table_name("services", table_config, db_config),
                       " where SUBJECT_ID = ",as.numeric(input$subject_id),
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  if (database_type == "bigquery") {
    query_result = query_exec(query_text, connection) %>% arrange(transfertime)
  }
  else {
    query_result = dbGetQuery(connection, query_text) %>% arrange(transfertime)
  }
  
  query_result %>%
    left_join(services_labels, by = c("prev_service" = "SERVICE")) %>%
    left_join(services_labels, by = c("curr_service" = "SERVICE")) %>%
    select(row_id, transfertime, prev_service, prev_service_desc = DESCRIPTION.x, curr_service, curr_service_desc = DESCRIPTION.y)
}

query_transfers <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select row_id, icustay_id, dbsource, eventtype, prev_careunit, curr_careunit, prev_wardid, curr_wardid, intime, outtime, los from ",
                       format_table_name("transfers", table_config, db_config),
                       " where SUBJECT_ID = ",as.numeric(input$subject_id),
                       ifelse(length(input$hadm_id) > 0, paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(row_id)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(row_id)
  }
}
