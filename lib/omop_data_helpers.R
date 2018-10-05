omop_get_review_table_names <- function() {
  c("condition_era", "condition_occurrence", "death", "device_exposure", "dose_era", "drug_era", "drug_exposure", "measurement", 
    "note", "note_nlp", "observation", "observation_period", "payer_plan_period", "person", "procedure_occurrence", "specimen", "visit_occurrence")
}

omop_format_table_name <- function(table_key, table_config, db_config) {
  paste0(db_config["schema"], ".", table_config[table_key])
}

omop_query_all_people <- function(table_config, db_config, database_type, connection) {
  query_text <- paste0("select person_id, person_source_value, person.gender_concept_id as gender_concept_id, gc.concept_name as gender_concept_name, person.gender_source_value as gender_source_value, person.gender_source_concept_id as gender_source_concept_id, person.year_of_birth as year_of_birth, month_of_birth, day_of_birth, birth_datetime, ",
                       "race_concept_id, rc.concept_name as race_concept_name, race_source_value, race_source_concept_id, ethnicity_concept_id, ec.concept_name as ethnicity_concept_name, ethnicity_source_value, ethnicity_source_concept_id, person.provider_id as provider_id, p.provider_name as provider_name ",
                       "from ", omop_format_table_name("person", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " gc ON gc.concept_id = gender_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " rc ON rc.concept_id = race_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " ec ON ec.concept_id = ethnicity_concept_id ",
                       "left outer join ", omop_format_table_name("provider", table_config, db_config), " p ON p.provider_id = ", omop_format_table_name("person", table_config, db_config), ".provider_id ")
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(person_id = paste0("<a class='row_subject_id' href='#'>", person_id, "</a>"))
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(person_id = paste0("<a class='row_subject_id' href='#'>", person_id, "</a>"))
  }
}

omop_query_condition_era <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select condition_era_id, condition_concept_id, cc.concept_name as condition_concept_name, ",
                       "condition_era_start_date, condition_era_end_date, condition_occurrence_count ",
                       "from ", omop_format_table_name("condition_era", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " cc on cc.concept_id = condition_concept_id ",
                       " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_condition_occurrence <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select condition_occurrence_id, condition_concept_id, c.concept_name as condition_concept_name, ",
        "condition_source_value, condition_source_concept_id, cs.concept_name as condition_source_concept_name, ",
        "condition_start_date, condition_start_datetime, condition_end_date, condition_end_datetime, ",
        "condition_type_concept_id, ct.concept_name as condition_type_concept_name, stop_reason, ",
        "condition_occurrence.provider_id as provider_id, provider_name, visit_occurrence_id, ",
        "condition_status_source_value, condition_status_concept_id, cst.concept_name as condition_status_concept_name ",
        "from ", omop_format_table_name("condition_occurrence", table_config, db_config), " ",
        "left outer join ", omop_format_table_name("concept", table_config, db_config), " c on c.concept_id = condition_concept_id ",
        "left outer join ", omop_format_table_name("concept", table_config, db_config), " ct on ct.concept_id = condition_type_concept_id ",
        "left outer join ", omop_format_table_name("concept", table_config, db_config), " cs on cs.concept_id = condition_source_concept_id ",
        "left outer join ", omop_format_table_name("concept", table_config, db_config), " cst on cst.concept_id = condition_status_concept_id ",
        "left outer join ", omop_format_table_name("provider", table_config, db_config), " p on p.provider_id = condition_occurrence.provider_id",
        " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_cost <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select cost_id, cost_event_id, cost_domain_id, cost_type_concept_id, ctc.concept_name as cost_type_concept_name, ",
                       "currency_concept_id, cc.concept_name as currency_concept_name, total_charge, total_paid, paid_by_payer, paid_by_patient, ",
                       "paid_patient_copay, paid_patient_coinsurance, paid_patient_deductible, paid_by_primary, paid_ingredient_cost, paid_dispensing_fee, ",
                       "payer_plan_period_id, amount_allowed, revenue_code_concept_id, rcc.concept_name as revenue_code_concept_name, reveue_code_source_value, ",
                       "drg_concept_id, dc.concept_name as drg_concept_name, drg_source_value ",
                       "from ", omop_format_table_name("cost", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " ctc on ctc.concept_id = cost_type_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " cc on cc.concept_id = currency_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " rcc on rcc.concept_id = revenue_code_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " dc on dc.concept_id = drg_concept_id ",
                       " where cost_id = ", as.numeric(input$item_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_device_exposure <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select device_exposure_id device_concept_id, dc.concept_name as device_concept_name, ",
                      "device_source_value, device_source_concept_id, dsc.concept_name as device_source_concept_name, ",
                      "device_exposure_start_date, device_exposure_start_datetime, device_exposure_end_date, device_exposure_end_datetime, ",
                      "device_type_concept_id, dtc.concept_name AS device_type_concept_name, unique_device_id, quantity, ",
                      "p.provider_id, p.provider_name as provider_name, visit_occurrence_id ",
                      "from ", omop_format_table_name("device_exposure", table_config, db_config), " ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config), " dc on dc.concept_id = device_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config), " dsc on dsc.concept_id = device_source_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config), " dtc on dtc.concept_id = device_type_concept_id ",
                      "left outer join ", omop_format_table_name("provider", table_config, db_config), " p on p.provider_id = ", omop_format_table_name("device_exposure", table_config, db_config), ".provider_id ",
                      " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_death <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select death_date, death_datetime, death_type_concept_id, dt.concept_name as death_type_concept_name, ",
                      "cause_concept_id, cc.concept_name as cause_concept_name, cause_source_value, ",
                      "cause_source_concept_id, cs.concept_name as cause_source_concept_name ",
                      "from ", omop_format_table_name("death", table_config, db_config), " ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," dt on dt.concept_id = death_type_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," cc on cc.concept_id = cause_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," cs on cs.concept_id = cause_source_concept_id ",
                      " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_dose_era <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select dose_era_id, drug_concept_id, dc.concept_name as drug_concept_name, unit_concept_id, uc.concept_name as unit_concept_name, ",
                      "dose_value, dose_era_start_date, dose_era_end_date ",
                      "from ", omop_format_table_name("dose_era", table_config, db_config), " ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," dc on dc.concept_id = drug_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," uc on uc.concept_id = unit_concept_id ",
                      " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_drug_era <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select drug_era_id, drug_concept_id, dc.concept_name as drug_concept_name, drug_era_start_date, drug_era_end_date, drug_exposure_count, gap_days  ",
                       "from ", omop_format_table_name("drug_era", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config)," dc on dc.concept_id = drug_concept_id ",
                       " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_drug_exposure <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select drug_exposure_id, drug_concept_id, dc.concept_name as drug_concept_name, drug_source_value, drug_source_concept_id, dsc.concept_name as drug_source_concept_name, ",
                      "drug_exposure_start_date, drug_exposure_start_datetime, drug_exposure_end_date, drug_exposure_end_datetime, verbatim_end_date, ",
                      "drug_type_concept_id, dtc.concept_name as drug_type_concept_name, stop_reason, refills, quantity, days_supply, sig, ",
                      "route_concept_id, rc.concept_name as route_concept_name, route_source_value, lot_number, p.provider_id, p.provider_name as provider_name, ",
                      "visit_occurrence_id, dose_unit_source_value ",
                      "from ", omop_format_table_name("drug_exposure", table_config, db_config), " ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," dc on dc.concept_id = drug_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," dsc on dsc.concept_id = drug_source_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," dtc on dtc.concept_id = drug_type_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," rc on rc.concept_id = route_concept_id ",
                      "left outer join ", omop_format_table_name("provider", table_config, db_config)," p on p.provider_id = ", omop_format_table_name("drug_exposure", table_config, db_config),".provider_id ",
                      " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_measurement <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select measurement_id, measurement_concept_id, mc.concept_name as measurement_concept_name, measurement_source_value, measurement_source_concept_id, msc.concept_name as measurement_source_concept_name, ",
                      "measurement_date, measurement_datetime, measurement_type_concept_id, mtc.concept_name as measurement_type_concept_name, operator_concept_id, oc.concept_name as operator_concept_name, ",
                      "value_as_number, value_as_concept_id, vc.concept_name as value_as_concept_name, value_source_value, unit_concept_id, uc.concept_name as unit_concept_name, unit_source_value, ",
                      "range_low, range_high, p.provider_id, p.provider_name as provider_name, visit_occurrence_id ",
                      "from ", omop_format_table_name("measurement", table_config, db_config), " ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," mc on mc.concept_id = measurement_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," msc on msc.concept_id = measurement_source_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," mtc on mtc.concept_id = measurement_type_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," oc on oc.concept_id = operator_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," vc on vc.concept_id = value_as_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," uc on uc.concept_id = unit_concept_id ",
                      "left outer join ", omop_format_table_name("provider", table_config, db_config)," p on p.provider_id = ", omop_format_table_name("measurement", table_config, db_config), ".provider_id ",
                      " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_note <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select note_id, note_date, note_datetime, note_type_concept_id, ntc.concept_name as note_type_concept_name, ",
                      "note_class_concept_id, ncc.concept_name as note_class_concept_name, note_title, note_text, ",
                      "encoding_concept_id, ec.concept_name as encoding_concept_name, language_concept_id, lc.concept_name as language_concept_name, ",
                      "p.provider_id, p.provider_name as provider_name, visit_occurrence_id, note_source_value ",
                      "from ", omop_format_table_name("note", table_config, db_config), " ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," ntc on ntc.concept_id = note_type_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," ncc on ncc.concept_id = note_class_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," ec on ec.concept_id = encoding_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," lc on lc.concept_id = language_concept_id ",
                      "left outer join ", omop_format_table_name("provider", table_config, db_config)," p on p.provider_id = ", omop_format_table_name("note", table_config, db_config), ".provider_id ",
                      " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_note_nlp <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select note_nlp_id, note.note_id as note_id, section_concept_id, sc.concept_name as section_concept_name, snippet, \"offset\", lexical_variant, ",
                       "note_nlp_concept_id, nc.concept_name as note_nlp_concept_name, note_nlp_source_concept_id, nsc.concept_name as note_nlp_source_concept_name, ",
                       "nlp_system, nlp_date, nlp_datetime, term_exists, term_temporal, term_modifiers ",
                       "from ", omop_format_table_name("note_nlp", table_config, db_config), " ",
                       "inner join ", omop_format_table_name("note", table_config, db_config)," on note.note_id = note_nlp.note_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config)," sc on sc.concept_id = section_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config)," nc on nc.concept_id = note_nlp_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config)," nsc on nsc.concept_id = note_nlp_source_concept_id ",
                       " where note.person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_observation_period <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select observation_period_id, observation_period_start_date, observation_period_end_date, period_type_concept_id, c.concept_name as period_type_concept_name ",
                       "from ", omop_format_table_name("observation_period", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " c ON c.concept_id = period_type_concept_id ",
                       " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_observation <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select observation_id, observation_concept_id, oc.concept_name as observation_concept_name, observation_date, observation_datetime, ",
                      "observation_type_concept_id, otc.concept_name as observation_type_concept_name, value_as_number, value_as_string, ",
                      "value_as_concept_id, vc.concept_name as value_as_concept_name, observation_source_value, observation_source_concept_id, osc.concept_name as observation_source_concept_name, ",
                      "qualifier_concept_id, qc.concept_name as qualifier_concept_name, qualifier_source_value, unit_concept_id, uc.concept_name as unit_concept_name, unit_source_value, ",
                      "p.provider_id, p.provider_name, visit_occurrence_id ",
                      "from ", omop_format_table_name("observation", table_config, db_config), " ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config), " oc on oc.concept_id = observation_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config), " otc on otc.concept_id = observation_type_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config), " vc on vc.concept_id = value_as_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config), " osc on osc.concept_id = observation_source_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config), " qc on qc.concept_id = qualifier_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config), " uc on uc.concept_id = unit_concept_id ",
                      "left outer join ", omop_format_table_name("provider", table_config, db_config), " p on p.provider_id = ", omop_format_table_name("observation", table_config, db_config), ".provider_id ",
                      " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_payer_plan_period <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select payer_plan_period_id, payer_plan_period_start_date, payer_plan_period_end_date, payer_source_value, plan_source_value, family_source_value ",
                       "from ", omop_format_table_name("payer_plan_period", table_config, db_config), " ",
                       " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_person <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select person_id, person_source_value, person.gender_concept_id as gender_concept_id, gc.concept_name as gender_concept_name, person.gender_source_value as gender_source_value, person.gender_source_concept_id as gender_source_concept_id, person.year_of_birth as year_of_birth, month_of_birth, day_of_birth, birth_datetime, ",
                       "race_concept_id, rc.concept_name as race_concept_name, race_source_value, race_source_concept_id, ethnicity_concept_id, ec.concept_name as ethnicity_concept_name, ethnicity_source_value, ethnicity_source_concept_id, person.provider_id as provider_id, p.provider_name as provider_name ",
                       "from ", omop_format_table_name("person", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " gc ON gc.concept_id = gender_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " rc ON rc.concept_id = race_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " ec ON ec.concept_id = ethnicity_concept_id ",
                       "left outer join ", omop_format_table_name("provider", table_config, db_config), " p ON p.provider_id = ", omop_format_table_name("person", table_config, db_config), ".provider_id ",
                       " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_procedure_occurrence <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select procedure_occurrence_id, procedure_concept_id, pc.concept_name as procedure_concept_name, procedure_source_value, procedure_source_concept_id, psc.concept_name as procedure_source_concept_name, ",
                       "procedure_date, procedure_datetime, procedure_type_concept_id, ptc.concept_name as procedure_type_concept_name, ",
                       "modifier_concept_id, mc.concept_name as modifier_concept_name, qualifier_source_value, quantity, ",
                       "p.provider_id, p.provider_name as provider_name, visit_occurrence_id ",
                       "from ", omop_format_table_name("procedure_occurrence", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config)," pc on pc.concept_id = procedure_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config)," psc on psc.concept_id = procedure_source_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config)," ptc on ptc.concept_id = procedure_type_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config)," mc on mc.concept_id = modifier_concept_id ",
                       "left outer join ", omop_format_table_name("provider", table_config, db_config)," p on p.provider_id = ", omop_format_table_name("procedure_occurrence", table_config, db_config), ".provider_id ",
                       " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_specimen <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select specimen_id, specimen_concept_id, sc.concept_name as specimen_concept_name, specimen_source_id, specimen_source_value, ",
                       "specimen_type_concept_id, stc.concept_name as specimen_type_concept_name, specimen_date, specimen_datetime, ",
                       "quantity, unit_concept_id, u.concept_name as unit_concept_name, unit_source_value, ",
                       "anatomic_site_concept_id, anc.concept_name as anatomic_site_concept_name, anatomic_site_source_value, ",
                       "disease_status_concept_id, ds.concept_Name as disease_status_concept_name, disease_status_source_value ",
                       "from ", omop_format_table_name("specimen", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " sc on sc.concept_id = specimen_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config)," stc on stc.concept_id = specimen_type_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " u on u.concept_id = unit_concept_id ", 
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " anc on anc.concept_id = anatomic_site_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " ds on ds.concept_id = disease_status_concept_id ",
                       " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_visit_occurrence <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select visit_occurrence_id, visit_concept_id, vc.concept_name as visit_concept_name, visit_source_value, visit_source_concept_id, vsc.concept_name as visit_source_concept_name, ",
                      "visit_start_date, visit_start_datetime, visit_end_date, visit_end_datetime, ",
                      "visit_type_concept_id, vtc.concept_name as visit_type_concept_name, ",
                      "p.provider_id, p.provider_name as provider_name, ",
                      "csn.care_site_id, csn.care_site_name as care_site_name, ",
                      "admitting_source_concept_id, ads.concept_name as admitting_source_concept_name, admitting_source_value, ",
                      "discharge_to_concept_id, dtc.concept_name as discharge_to_concept_name, discharge_to_source_value, ",
                      "preceding_visit_occurrence_id ",
                      "from ", omop_format_table_name("visit_occurrence", table_config, db_config), " ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," vc on vc.concept_id = visit_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," vsc on vsc.concept_id = visit_source_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," vtc on vtc.concept_id = visit_type_concept_id ",
                      "left outer join ", omop_format_table_name("provider", table_config, db_config)," p on p.provider_id = ", omop_format_table_name("visit_occurrence", table_config, db_config), ".provider_id ",
                      "left outer join ", omop_format_table_name("care_site", table_config, db_config)," csn on csn.care_site_id = ", omop_format_table_name("visit_occurrence", table_config, db_config), ".care_site_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," ads on ads.concept_id = admitting_source_concept_id ",
                      "left outer join ", omop_format_table_name("concept", table_config, db_config)," dtc on dtc.concept_id = discharge_to_concept_id ",
                      " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_render_data_tables <- function(table_config, db_config, input, output, database_type, connection) {
  if (is.null(connection)) { return(output) }
  
  condition_era_data <- reactive({omop_query_condition_era(table_config, db_config, input, database_type, connection)})
  condition_occurrence_data <- reactive({omop_query_condition_occurrence(table_config, db_config, input, database_type, connection)})
  death_data <- reactive({omop_query_death(table_config, db_config, input, database_type, connection)})
  device_exposure_data <- reactive({omop_query_device_exposure(table_config, db_config, input, database_type, connection)})
  dose_era_data <- reactive({omop_query_dose_era(table_config, db_config, input, database_type, connection)})
  drug_era_data <- reactive({omop_query_drug_era(table_config, db_config, input, database_type, connection)})
  drug_exposure_data <- reactive({omop_query_drug_exposure(table_config, db_config, input, database_type, connection)})
  measurement_data <- reactive({omop_query_measurement(table_config, db_config, input, database_type, connection)})
  note_data <- reactive({omop_query_note(table_config, db_config, input, database_type, connection)})
  note_nlp_data <- reactive({omop_query_note_nlp(table_config, db_config, input, database_type, connection)})
  observation_data <- reactive({omop_query_observation(table_config, db_config, input, database_type, connection)})
  observation_period_data <- reactive({omop_query_observation_period(table_config, db_config, input, database_type, connection)})
  payer_plan_period_data <- reactive({omop_query_payer_plan_period(table_config, db_config, input, database_type, connection)})
  person_data <- reactive({omop_query_person(table_config, db_config, input, database_type, connection)})
  procedure_occurrence_data <- reactive({omop_query_procedure_occurrence(table_config, db_config, input, database_type, connection)})
  specimen_data <- reactive({omop_query_specimen(table_config, db_config, input, database_type, connection)})
  visit_occurrence_data <- reactive({omop_query_visit_occurrence(table_config, db_config, input, database_type, connection)})

  output$all_patients_tbl <- DT::renderDataTable(
    omop_query_all_people(table_config, db_config, database_type, connection),
    options = list(paging = TRUE, pageLength = 20, searchHighlight = TRUE),
    escape=FALSE, rownames=F,
    callback = JS(
      'table.on("click", "tr td a.row_subject_id", function() {
      Shiny.onInputChange("subject_id", $(this).text());
      $(".main-sidebar li a").click();
    });'))
  output$person_tbl <- DT::renderDataTable(person_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$condition_era_tbl <- DT::renderDataTable(condition_era_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$condition_occurrence_tbl <- DT::renderDataTable(condition_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$person_tbl <- DT::renderDataTable(person_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$death_tbl <- DT::renderDataTable(death_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$device_exposure_tbl <- DT::renderDataTable(device_exposure_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$dose_era_tbl <- DT::renderDataTable(dose_era_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$drug_era_tbl <- DT::renderDataTable(drug_era_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$drug_exposure_tbl <- DT::renderDataTable(drug_exposure_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$measurement_tbl <- DT::renderDataTable(measurement_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$note_tbl <- DT::renderDataTable(note_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$note_nlp_tbl <- DT::renderDataTable(note_nlp_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$observation_tbl <- DT::renderDataTable(observation_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$observation_period_tbl <- DT::renderDataTable(observation_period_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$payer_plan_period_tbl <- DT::renderDataTable(payer_plan_period_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$procedure_occurrence_tbl <- DT::renderDataTable(procedure_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$specimen_tbl <- DT::renderDataTable(specimen_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$visit_occurrence_tbl <- DT::renderDataTable(visit_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output
}
