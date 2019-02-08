omop_get_review_table_names <- function() {
  c("person", "condition_era", "condition_occurrence", "death", "device_exposure", "dose_era", "drug_era", "drug_exposure", "measurement", 
    "note", "observation", "observation_period", "payer_plan_period", "procedure_occurrence", "specimen", "visit_occurrence")
}

omop_table_name <- function(canonical_name, table_map) {
  paste0(db_config["schema"], ".", table_config[table_key])
}

omop_table <- function(canonical_table, cfg) {
  table_name <- cfg$table_map %>%
    filter(table == canonical_table) %>%
    slice(1) %>%
    pull(user_database_table)
  tbl(cfg$connection, table_name)
}

omop_column <- function(canonical_name, cfg) {
  name_parts <- unlist(strsplit(canonical_name, "\\."))
  column_name <- cfg$table_map %>%
    filter(table == name_parts[1] & field == name_parts[2]) %>%
    slice(1) %>%
    pull(user_fields)
  #if (length(name_parts) > 2) { column_name <- paste(column_name, name_parts[-(1:2)], sep=".") }
  column_name
}

omop_query_all_people <- function(cfg) {
  # Cache commonly used tables and column names
  concept_id_col <- omop_column("concept.concept_id", cfg)
  concept_name_col <- omop_column("concept.concept_name", cfg)
  concept_tbl <- omop_table("concept", cfg)

  data_table <- omop_table("person", cfg) %>%
    left_join(select(concept_tbl, "gender_concept_name" = concept_name_col, "gcid" = concept_id_col), by=setNames("gcid", omop_column("person.gender_concept_id", cfg))) %>%
    left_join(select(concept_tbl, "race_concept_name" = concept_name_col, "rcid" = concept_id_col), by=setNames("rcid", omop_column("person.race_concept_id", cfg))) %>%
    left_join(select(concept_tbl, "ethnicity_concept_name" = concept_name_col, "ecid" = concept_id_col), by=setNames("ecid", omop_column("person.ethnicity_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("person.provider_id", cfg))) %>%
    select(omop_column("person.person_id", cfg), omop_column("person.person_source_value", cfg),
            "gender_concept_name", omop_column("person.gender_source_value", cfg),
            omop_column("person.year_of_birth", cfg), omop_column("person.month_of_birth", cfg), omop_column("person.day_of_birth", cfg),
            omop_column("person.birth_datetime", cfg), "race_concept_name", omop_column("person.race_source_value", cfg),
            "ethnicity_concept_name", omop_column("person.ethnicity_source_value", cfg),
            "Provider" = omop_column("provider.provider_name", cfg)) %>%
    arrange(person_id) %>%
    collect() %>%  
    mutate(person_id = paste0("<a class='row_subject_id' href='#'>", person_id, "</a>"))
  data_table
}

omop_query_condition_era <- function(input, cfg) {
  # Cache commonly used tables and column names
  concept_id_col <- omop_column("concept.concept_id", cfg)
  concept_name_col <- omop_column("concept.concept_name", cfg)
  concept_tbl <- omop_table("concept", cfg)
  
  data_table <- omop_table("condition_era", cfg) %>%
    filter(person_id == as.integer(input$subject_id)) %>%
    left_join(select(concept_tbl, "condition_concept_name" = concept_name_col, "ccid" = concept_id_col), by=setNames("ccid", omop_column("condition_era.condition_concept_id", cfg))) %>%
    select("Era" = omop_column("condition_era.condition_era_id", cfg), "Name" = omop_column("condition_era.condition_concept_name", cfg),
           "StartDate" = omop_column("condition_era.condition_era_start_date", cfg), "EndDate" = omop_column("condition_era.condition_era_end_date", cfg),
           "Count" = omop_column("condition_era.condition_occurrence_count", cfg)) %>%
    collect()
  data_table
}

omop_query_condition_occurrence <- function(input, cfg) {
  # Cache commonly used tables and column names
  concept_id_col <- omop_column("concept.concept_id", cfg)
  concept_name_col <- omop_column("concept.concept_name", cfg)
  concept_tbl <- omop_table("concept", cfg)
  
  data_table <- omop_table("condition_occurrence", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(concept_tbl, "condition_concept_name" = concept_name_col, "ccid" = concept_id_col), by=setNames("ccid", omop_column("condition_occurrence.condition_concept_id", cfg))) %>%
    left_join(select(concept_tbl, "condition_type_concept_name" = concept_name_col, "ctcid" = concept_id_col), by=setNames("ctcid", omop_column("condition_occurrence.condition_type_concept_id", cfg))) %>%
    left_join(select(concept_tbl, "condition_status_concept_name" = concept_name_col, "cscid" = concept_id_col), by=setNames("cscid", omop_column("condition_occurrence.condition_status_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("condition_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("condition_occurrence.condition_occurrence_id", cfg), "Concept" = "condition_concept_name",
           "Source Val" = omop_column("condition_occurrence.condition_source_value", cfg), "Start Date" = omop_column("condition_occurrence.condition_start_date", cfg),
           "Start Date/Time" = omop_column("condition_occurrence.condition_start_datetime", cfg), "End Date" = omop_column("condition_occurrence.condition_end_date", cfg),
           "End Date/Time" = omop_column("condition_occurrence.condition_end_datetime", cfg), "Type" = "condition_type_concept_name",
           "Stop Reason" = omop_column("condition_occurrence.stop_reason", cfg), "Provider" = omop_column("provider.provider_name", cfg)) %>%
    collect()
  data_table
}

omop_query_death <- function(input, cfg) {
  # Cache commonly used tables and column names
  concept_id_col <- omop_column("concept.concept_id", cfg)
  concept_name_col <- omop_column("concept.concept_name", cfg)
  concept_tbl <- omop_table("concept", cfg)
  
  data_table <- omop_table("death", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(concept_tbl, "death_type_concept_name" = concept_name_col, "dtcid" = concept_id_col), by=setNames("dtcid", omop_column("death.death_type_concept_id", cfg))) %>%
    left_join(select(concept_tbl, "cause_concept_name" = concept_name_col, "ccid" = concept_id_col), by=setNames("ccid", omop_column("death.cause_concept_id", cfg))) %>%
    select("Date" = omop_column("death.death_date", cfg), "DateTime" = omop_column("death.death_datetime", cfg),
           "Type" = "death_type_concept_name", "Cause" = omop_column("death.cause_concept_name", cfg),
           "SourceCause" = omop_column("death.cause_source_value", cfg)) %>%
    collect()
  data_table
}

omop_query_device_exposure <- function(input, cfg) {
  # Cache commonly used tables and column names
  concept_id_col <- omop_column("concept.concept_id", cfg)
  concept_name_col <- omop_column("concept.concept_name", cfg)
  concept_tbl <- omop_table("concept", cfg)
  
  data_table <- omop_table("device_exposure", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(concept_tbl, "device_concept_name" = concept_name_col, "dcid" = concept_id_col), by=setNames("dcid", omop_column("device_exposure.device_concept_id", cfg))) %>%
    left_join(select(concept_tbl, "device_type_concept_name" = concept_name_col, "dtcid" = concept_id_col), by=setNames("dtcid", omop_column("device_exposure.device_type_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("condition_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("device_exposure.device_exposure_id", cfg), "Name" = "device_concept_name", "SourceVal" = omop_column("device_exposure.device_source_value", cfg),
           "StartDate" = omop_column("device_exposure.device_exposure_start_date", cfg), "StartDateTime" = omop_column("device_exposure.device_exposure_start_datetime", cfg),
           "EndDate" = omop_column("device_exposure.device_exposure_end_date", cfg), "EndDateTime" = omop_column("device_exposure.device_exposure_end_datetime", cfg),
           "Type" = "device_type_concept_name", "Device ID" = omop_column("device_exposure.unique_device_id", cfg), "Quantity" = omop_column("device_exposure.quantity", cfg),
           "Provider" = omop_column("provider.provider_name", cfg)) %>%
    collect()
  data_table
}

omop_query_dose_era <- function(input, cfg) {
  # Cache commonly used tables and column names
  concept_id_col <- omop_column("concept.concept_id", cfg)
  concept_name_col <- omop_column("concept.concept_name", cfg)
  concept_tbl <- omop_table("concept", cfg)
  
  data_table <- omop_table("dose_era", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(concept_tbl, "drug_concept_name" = concept_name_col, "dcid" = concept_id_col), by=setNames("dcid", omop_column("dose_era.drug_concept_id", cfg))) %>%
    left_join(select(concept_tbl, "unit_concept_name" = concept_name_col, "ucid" = concept_id_col), by=setNames("ucid", omop_column("dose_era.unit_concept_id", cfg))) %>%
    select("ID" = omop_column("dose_era.dose_era_id", cfg), "Drug" = "drug_concept_name", "Unit" = "unit_concept_name",
           "DoseValue" = omop_column("dose_era.dose_value", cfg), "StartDate" = omop_column("dose_era.dose_era_start_date", cfg),
           "EndDate" = omop_column("dose_era.dose_era_end_date", cfg)) %>%
    collect()
  data_table
}

omop_query_drug_era <- function(input, cfg, cache) {
  data_table <- omop_table("drug_era", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(cache$concept_tbl, "drug_concept_name" = cache$concept_name_col, "dcid" = cache$concept_id_col), by=setNames("dcid", omop_column("drug_era.drug_concept_id", cfg))) %>%
    select("ID" = omop_column("drug_era.drug_era_id", cfg), "Drug" = "drug_concept_name", "StartDate" = omop_column("drug_era.drug_era_start_date", cfg),
           "EndDate" = omop_column("drug_era.drug_era_end_date", cfg), "ExposureCount" = omop_column("drug_era.drug_exposure_count", cfg),
           "GapDays" = omop_column("drug_era.gap_days", cfg)) %>%
    collect()
  data_table
}

omop_query_drug_exposure <- function(input, cfg, cache) {
  data_table <- omop_table("drug_exposure", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(cache$concept_tbl, "drug_concept_name" = cache$concept_name_col, "dcid" = cache$concept_id_col), by=setNames("dcid", omop_column("drug_exposure.drug_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "drug_type_concept_name" = cache$concept_name_col, "dtcid" = cache$concept_id_col), by=setNames("dtcid", omop_column("drug_exposure.drug_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "route_concept_name" = cache$concept_name_col, "rcid" = cache$concept_id_col), by=setNames("rcid", omop_column("drug_exposure.route_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("condition_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("drug_exposure.drug_exposure_id", cfg), "Drug" = "drug_concept_name", "SourceVal" = omop_column("drug_exposure.drug_source_value", cfg),
           "StartDate" = omop_column("drug_exposure.drug_exposure_start_date", cfg), "StartDateTime" = omop_column("drug_exposure.drug_exposure_start_datetime", cfg),
           "EndDate" = omop_column("drug_exposure.drug_exposure_end_date", cfg), "EndDateTime" = omop_column("drug_exposure.drug_exposure_end_datetime", cfg),
           "VerbatimEnd" = omop_column("drug_exposure.verbatim_end_date", cfg), "Type" = "drug_type_concept_name", "StopReason" = omop_column("drug_exposure.stop_reason", cfg),
           "Refills" = omop_column("drug_exposure.refills", cfg), "Quantity" = omop_column("drug_exposure.quantity", cfg), "DaysSupply" = omop_column("drug_exposure.days_supply", cfg),
           "Sig" = omop_column("drug_exposure.sig", cfg), "Route" = "route_concept_name",
           "Lot" = omop_column("drug_exposure.lot_number", cfg), "Provider" = omop_column("provider.provider_name", cfg)) %>%
    collect()
  data_table
}

omop_query_measurement <- function(input, cfg, cache) {
  data_table <- omop_table("measurement", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(cache$concept_tbl, "measurement_concept_name" = cache$concept_name_col, "mcid" = cache$concept_id_col), by=setNames("mcid", omop_column("measurement.measurement_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "measurement_type_concept_name" = cache$concept_name_col, "mtcid" = cache$concept_id_col), by=setNames("mtcid", omop_column("measurement.measurement_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "operator_concept_name" = cache$concept_name_col, "ocid" = cache$concept_id_col), by=setNames("ocid", omop_column("measurement.operator_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "value_as_concept_name" = cache$concept_name_col, "vacid" = cache$concept_id_col), by=setNames("vacid", omop_column("measurement.value_as_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "unit_concept_name" = cache$concept_name_col, "ucid" = cache$concept_id_col), by=setNames("ucid", omop_column("measurement.unit_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("condition_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("measurement.measurement_id", cfg), "Name" = "measurement_concept_name", "SourceVal" = omop_column("measurement.measurement_source_value", cfg),
           "Date" = omop_column("measurement.measurement_date", cfg), "DateTime" = omop_column("measurement.measurement_datetime", cfg),
           "Type" = "measurement_type_concept_name", "Operator" = "operator_concept_name", "ValueNum" = omop_column("measurement.value_as_number", cfg),
           "ValueConcept" = "value_as_concept_name", "ValueSource" = omop_column("measurement.value_source_value", cfg), "Unit" = "unit_concept_name",
           "RangeLow" = omop_column("measurement.range_low", cfg), "RangeHigh" = omop_column("measurement.range_high", cfg),
           "Provider" = omop_column("provider.provider_name", cfg)) %>%
    collect()
  data_table
}

omop_query_note <- function(input, cfg, cache) {
  data_table <- omop_table("note", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(cache$concept_tbl, "note_type_concept_name" = cache$concept_name_col, "ntcid" = cache$concept_id_col), by=setNames("ntcid", omop_column("note.note_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "note_class_concept_name" = cache$concept_name_col, "nccid" = cache$concept_id_col), by=setNames("nccid", omop_column("note.note_class_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "encoding_concept_name" = cache$concept_name_col, "ecid" = cache$concept_id_col), by=setNames("ecid", omop_column("note.encoding_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "language_concept_name" = cache$concept_name_col, "lcid" = cache$concept_id_col), by=setNames("lcid", omop_column("note.language_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("condition_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("note.note_id", cfg), "Date" = omop_column("note.note_date", cfg), "DateTime" = omop_column("note.note_datetime", cfg),
           "Type" = "note_type_concept_name", "Class" = "note_class_concept_name", "Title" = omop_column("note.note_title", cfg),
           "Text" = omop_column("note.note_text", cfg), "Encoding" = "encoding_concept_name", "Language" = "language_concept_name",
           "Provider" = omop_column("provider.provider_name", cfg)) %>%
    collect()
  data_table
}

# Refactor implementation when/if this table is needed
# omop_query_note_nlp <- function(input, cfg) {
#   data_table <- tbl(cfg$dbi_conn, "note_nlp") %>%
#     inner_join(tbl(cfg$dbi_conn, "note"), by=c("note_id" = "note_id")) %>%
#     left_join(tbl(cfg$dbi_conn, "concept"), by=c("section_concept_id" = "concept_id")) %>%
#     rename("section_concept_name" = "concept_name") %>%
#     left_join(tbl(cfg$dbi_conn, "concept"), by=c("note_nlp_concept_id" = "concept_id")) %>%
#     rename("note_nlp_concept_name" = "concept_name") %>%
#     left_join(tbl(cfg$dbi_conn, "concept"), by=c("note_nlp_source_concept_id" = "concept_id")) %>%
#     rename("note_nlp_source_concept_name" = "concept_name") %>%
#     filter(person_id == input$subject_id) %>%
#     select("note_nlp_id", "note_id", "section_concept_id", "section_concept_name", "snippet", "offset", "lexical_variant",
#            "note_nlp_concept_id", "note_nlp_concept_name", "note_nlp_source_concept_id", "note_nlp_source_concept_name",
#            "nlp_system", "nlp_date", "nlp_datetime", "term_exists", "term_temporal", "term_modifiers") %>%
#     collect()
#   data_table
# }

omop_query_observation <- function(input, cfg, cache) {
  data_table <- omop_table("observation", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(cache$concept_tbl, "observation_concept_name" = cache$concept_name_col, "ocid" = cache$concept_id_col), by=setNames("ocid", omop_column("observation.observation_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "observation_type_concept_name" = cache$concept_name_col, "otcid" = cache$concept_id_col), by=setNames("otcid", omop_column("observation.observation_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "value_as_concept_name" = cache$concept_name_col, "vacid" = cache$concept_id_col), by=setNames("vacid", omop_column("observation.value_as_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "qualifier_concept_name" = cache$concept_name_col, "qcid" = cache$concept_id_col), by=setNames("qcid", omop_column("observation.qualifier_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "unit_concept_name" = cache$concept_name_col, "ucid" = cache$concept_id_col), by=setNames("ucid", omop_column("observation.unit_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("condition_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("observation.", cfg), "Name" = "observation_concept_name", "Date" = omop_column("observation.observation_date", cfg),
           "DateTime" = omop_column("observation.observation_datetime", cfg), "Type" = "observation_type_concept_name", "ValueNum" = omop_column("observation.value_as_number", cfg),
           "ValueString" = omop_column("observation.value_as_string", cfg), "ValueConcept" = "value_as_concept_name", "SourceVal" = omop_column("observation.observation_source_value", cfg),
           "Qualifier" = "qualifier_concept_name", "Unit" = "unit_concept_name", "Provider" = omop_column("provider.provider_name", cfg)) %>%
    collect()
  data_table
}

omop_query_observation_period <- function(input, cfg) {
  data_table <- tbl(cfg$dbi_conn, "observation_period") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("period_type_concept_id" = "concept_id")) %>%
    rename("period_type_concept_name" = "concept_name") %>%
    filter(person_id == input$subject_id) %>%
    select("observation_period_id", "observation_period_start_date", "observation_period_end_date", "period_type_concept_id", "period_type_concept_name") %>%
    collect()
  data_table
}

omop_query_payer_plan_period <- function(input, cfg) {
  data_table <- tbl(cfg$dbi_conn, "payer_plan_period") %>%
    filter(person_id == input$subject_id) %>%
    select("payer_plan_period_id", "payer_plan_period_start_date", "payer_plan_period_end_date", "payer_source_value",
           "plan_source_value", "family_source_value") %>%
    collect()
  data_table
}

omop_query_person <- function(input, cfg) {
  # Cache commonly used tables and column names
  concept_id_col <- omop_column("concept.concept_id", cfg)
  concept_name_col <- omop_column("concept.concept_name", cfg)
  concept_tbl <- omop_table("concept", cfg)
  
  data_table <- omop_table("person", cfg) %>%
    filter(person_id == input$subject_id) %>%
    left_join(select(concept_tbl, "gender_concept_name" = concept_name_col, "gcid" = concept_id_col), by=setNames("gcid", omop_column("person.gender_concept_id", cfg))) %>%
    left_join(select(concept_tbl, "race_concept_name" = concept_name_col, "rcid" = concept_id_col), by=setNames("rcid", omop_column("person.race_concept_id", cfg))) %>%
    left_join(select(concept_tbl, "ethnicity_concept_name" = concept_name_col, "ecid" = concept_id_col), by=setNames("ecid", omop_column("person.ethnicity_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("person.provider_id", cfg))) %>%
    select(omop_column("person.person_id", cfg), omop_column("person.person_source_value", cfg),
           "gender_concept_name", omop_column("person.gender_source_value", cfg),
           omop_column("person.year_of_birth", cfg), omop_column("person.month_of_birth", cfg), omop_column("person.day_of_birth", cfg),
           omop_column("person.birth_datetime", cfg), "race_concept_name", omop_column("person.race_source_value", cfg),
           "ethnicity_concept_name", omop_column("person.ethnicity_source_value", cfg),
           "Provider" = omop_column("provider.provider_name", cfg)) %>%
    arrange(person_id) %>%
    collect()
  data_table
}

omop_query_procedure_occurrence <- function(input, cfg) {
  data_table <- tbl(cfg$dbi_conn, "procedure_occurrence") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("procedure_concept_id" = "concept_id")) %>%
    rename("procedure_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("procedure_source_concept_id" = "concept_id")) %>%
    rename("procedure_source_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("procedure_type_concept_id" = "concept_id")) %>%
    rename("procedure_type_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("modifier_concept_id" = "concept_id")) %>%
    rename("modifier_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("procedure_occurrence_id", "procedure_concept_id", "procedure_concept_name", "procedure_source_value", "procedure_source_concept_id",
          "procedure_source_concept_name", "procedure_date", "procedure_datetime", "procedure_type_concept_id", "procedure_type_concept_name",
          "modifier_concept_id", "modifier_concept_name", "qualifier_source_value", "quantity", "provider_id", "provider_name",
          "visit_occurrence_id") %>%
    collect()
  data_table
}

omop_query_specimen <- function(input, cfg) {
  data_table <- tbl(cfg$dbi_conn, "specimen") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("specimen_concept_id" = "concept_id")) %>%
    rename("specimen_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("specimen_type_concept_id" = "concept_id")) %>%
    rename("specimen_type_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("unit_concept_id" = "concept_id")) %>%
    rename("unit_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("anatomic_site_concept_id" = "concept_id")) %>%
    rename("anatomic_site_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("disease_status_concept_id" = "concept_id")) %>%
    rename("disease_status_concept_name" = "concept_name") %>%
    filter(person_id == input$subject_id) %>%
    select("specimen_id", "specimen_concept_id", "specimen_concept_name", "specimen_source_id", "specimen_source_value", "specimen_type_concept_id",
          "specimen_type_concept_name", "specimen_date", "specimen_datetime", "quantity", "unit_concept_id", "unit_concept_name",
          "unit_source_value", "anatomic_site_concept_id", "anatomic_site_concept_name", "anatomic_site_source_value", "disease_status_concept_id",
          "disease_status_concept_name", "disease_status_source_value") %>%
    collect()
  data_table
}

omop_query_visit_occurrence <- function(input, cfg) {
  data_table <- tbl(cfg$dbi_conn, "visit_occurrence") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("visit_concept_id" = "concept_id")) %>%
    rename("visit_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("visit_source_concept_id" = "concept_id")) %>%
    rename("visit_source_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("visit_type_concept_id" = "concept_id")) %>%
    rename("visit_type_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("admitting_source_concept_id" = "concept_id")) %>%
    rename("admitting_source_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "concept"), by=c("discharge_to_concept_id" = "concept_id")) %>%
    rename("discharge_to_concept_name" = "concept_name") %>%
    left_join(tbl(cfg$dbi_conn, "care_site"), by=c("care_site_id" = "care_site_id"), suffix = c(".p", ".cs")) %>%
    left_join(tbl(cfg$dbi_conn, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("visit_occurrence_id", "visit_concept_id", "visit_concept_name", "visit_source_value", "visit_source_concept_id",
          "visit_source_concept_name", "visit_start_date", "visit_start_datetime", "visit_end_date", "visit_end_datetime",
          "visit_type_concept_id", "visit_type_concept_name", "provider_id", "provider_name", "care_site_id.p", "care_site_name",
          "admitting_source_concept_id", "admitting_source_concept_name", "admitting_source_value", "discharge_to_concept_id",
          "discharge_to_concept_name", "discharge_to_source_value", "preceding_visit_occurrence_id") %>%
    collect()
  data_table
}

omop_render_data_tables <- function(input, output, cfg) {
  if (is.null(cfg)) { return(output) }
  
  # Cache commonly used tables and column names
  table_cache = list(concept_id_col = omop_column("concept.concept_id", cfg),
                      concept_name_col = omop_column("concept.concept_name", cfg),
                      concept_tbl = omop_table("concept", cfg))
  
  condition_era_data <- reactive({omop_query_condition_era(input, cfg)})
  condition_occurrence_data <- reactive({omop_query_condition_occurrence(input, cfg)})
  death_data <- reactive({omop_query_death(input, cfg)})
  device_exposure_data <- reactive({omop_query_device_exposure(input, cfg)})
  dose_era_data <- reactive({omop_query_dose_era(input, cfg)})
  drug_era_data <- reactive({omop_query_drug_era(input, cfg, table_cache)})
  drug_exposure_data <- reactive({omop_query_drug_exposure(input, cfg, table_cache)})
  measurement_data <- reactive({omop_query_measurement(input, cfg, table_cache)})
  note_data <- reactive({omop_query_note(input, cfg, table_cache)})
  #note_nlp_data <- reactive({omop_query_note_nlp(input, cfg)})
  observation_data <- reactive({omop_query_observation(input, cfg, table_cache)})
  observation_period_data <- reactive({omop_query_observation_period(input, cfg)})
  payer_plan_period_data <- reactive({omop_query_payer_plan_period(input, cfg)})
  person_data <- reactive({omop_query_person(input, cfg)})
  procedure_occurrence_data <- reactive({omop_query_procedure_occurrence(input, cfg)})
  specimen_data <- reactive({omop_query_specimen(input, cfg)})
  visit_occurrence_data <- reactive({omop_query_visit_occurrence(input, cfg)})

  output$all_patients_tbl <- DT::renderDataTable(
    omop_query_all_people(cfg),
    options = list(paging = TRUE, pageLength = 20, searchHighlight = TRUE),
    escape=FALSE, rownames=F, selection='none',
    callback = JS(
      'table.on("click", "tr td a.row_subject_id", function() {
      Shiny.onInputChange("subject_id", $(this).text());
      $(".main-sidebar li a").click();
    });'))
  output$condition_era_tbl <- DT::renderDataTable(condition_era_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$condition_occurrence_tbl <- DT::renderDataTable(condition_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$death_tbl <- DT::renderDataTable(death_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$device_exposure_tbl <- DT::renderDataTable(device_exposure_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$dose_era_tbl <- DT::renderDataTable(dose_era_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$drug_era_tbl <- DT::renderDataTable(drug_era_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$drug_exposure_tbl <- DT::renderDataTable(drug_exposure_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$measurement_tbl <- DT::renderDataTable(measurement_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$note_tbl <- DT::renderDataTable(note_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  #output$note_nlp_tbl <- DT::renderDataTable(note_nlp_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$observation_tbl <- DT::renderDataTable(observation_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$observation_period_tbl <- DT::renderDataTable(observation_period_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$payer_plan_period_tbl <- DT::renderDataTable(payer_plan_period_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$person_tbl <- DT::renderDataTable(person_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$procedure_occurrence_tbl <- DT::renderDataTable(procedure_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$specimen_tbl <- DT::renderDataTable(specimen_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$visit_occurrence_tbl <- DT::renderDataTable(visit_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output
}
