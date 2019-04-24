omop_get_review_table_names <- function() {
  c("person", "condition_era", "condition_occurrence", "death", "device_exposure", "dose_era", "drug_era", "drug_exposure", "measurement", 
    "note", "observation", "observation_period", "payer_plan_period", "procedure_occurrence", "specimen", "visit_occurrence")
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
  column_name
}

# This is a different implementation from omop_query_all_people because we are strictly limiting to just
# the IDs for our patient records
omop_get_all_people_for_list <- function(cfg) {
  data_table <- omop_table("person", cfg) %>%
    select("ID" = omop_column("person.person_id", cfg)) %>%
    collect()
  data_table
}

omop_query_all_people <- function(cfg, cache) {
  data_table <- omop_table("person", cfg) %>%
    left_join(select(cache$concept_tbl, "gender_concept_name" = cache$concept_name_col, "gcid" = cache$concept_id_col), by=setNames("gcid", omop_column("person.gender_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "race_concept_name" = cache$concept_name_col, "rcid" = cache$concept_id_col), by=setNames("rcid", omop_column("person.race_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "ethnicity_concept_name" = cache$concept_name_col, "ecid" = cache$concept_id_col), by=setNames("ecid", omop_column("person.ethnicity_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("person.provider_id", cfg))) %>%
    select(person_id = omop_column("person.person_id", cfg), "SourceVal" = omop_column("person.person_source_value", cfg),
            "Gender" = "gender_concept_name",
            "BirthYear" = omop_column("person.year_of_birth", cfg), "BirthMonth" = omop_column("person.month_of_birth", cfg), "BirthDay" = omop_column("person.day_of_birth", cfg),
            "BirthDateTime" = omop_column("person.birth_datetime", cfg), "Race" = "race_concept_name",
            "Ethnicity" = "ethnicity_concept_name",
            "Provider" = omop_column("provider.provider_name", cfg)) %>%
    arrange(person_id) %>%
    collect() %>%
    #mutate(person_id = paste0("<a class='row_subject_id' href='#'>", person_id, "</a>")) %>%
    rename("ID" = person_id)
  data_table
}

omop_query_condition_era <- function(input, cfg, cache) {
  data_table <- omop_table("condition_era", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "condition_concept_name" = cache$concept_name_col, "ccid" = cache$concept_id_col), by=setNames("ccid", omop_column("condition_era.condition_concept_id", cfg))) %>%
    select("Era" = omop_column("condition_era.condition_era_id", cfg), "Name" = omop_column("condition_era.condition_concept_name", cfg),
           "StartDate" = omop_column("condition_era.condition_era_start_date", cfg), "EndDate" = omop_column("condition_era.condition_era_end_date", cfg),
           "Count" = omop_column("condition_era.condition_occurrence_count", cfg)) %>%
    collect()
  data_table
}

omop_query_condition_occurrence <- function(input, cfg, cache) {
  data_table <- omop_table("condition_occurrence", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "condition_concept_name" = cache$concept_name_col, "ccid" = cache$concept_id_col), by=setNames("ccid", omop_column("condition_occurrence.condition_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "condition_type_concept_name" = cache$concept_name_col, "ctcid" = cache$concept_id_col), by=setNames("ctcid", omop_column("condition_occurrence.condition_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "condition_status_concept_name" = cache$concept_name_col, "cscid" = cache$concept_id_col), by=setNames("cscid", omop_column("condition_occurrence.condition_status_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("condition_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("condition_occurrence.condition_occurrence_id", cfg), "Concept" = "condition_concept_name",
           "SourceVal" = omop_column("condition_occurrence.condition_source_value", cfg), "StartDate" = omop_column("condition_occurrence.condition_start_date", cfg),
           "StartDateTime" = omop_column("condition_occurrence.condition_start_datetime", cfg), "EndDate" = omop_column("condition_occurrence.condition_end_date", cfg),
           "EndDateTime" = omop_column("condition_occurrence.condition_end_datetime", cfg), "Type" = "condition_type_concept_name",
           "StopReason" = omop_column("condition_occurrence.stop_reason", cfg), "Provider" = omop_column("provider.provider_name", cfg), "Visit" = omop_column("condition_occurrence.visit_occurrence_id", cfg)) %>%
    collect()
  data_table
}

omop_query_death <- function(input, cfg, cache) {
  data_table <- omop_table("death", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "death_type_concept_name" = cache$concept_name_col, "dtcid" = cache$concept_id_col), by=setNames("dtcid", omop_column("death.death_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "cause_concept_name" = cache$concept_name_col, "ccid" = cache$concept_id_col), by=setNames("ccid", omop_column("death.cause_concept_id", cfg))) %>%
    select("Date" = omop_column("death.death_date", cfg), "DateTime" = omop_column("death.death_datetime", cfg),
           "Type" = "death_type_concept_name", "Cause" = omop_column("death.cause_concept_name", cfg),
           "SourceCause" = omop_column("death.cause_source_value", cfg)) %>%
    collect()
  data_table
}

omop_query_device_exposure <- function(input, cfg, cache) {
  data_table <- omop_table("device_exposure", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "device_concept_name" = cache$concept_name_col, "dcid" = cache$concept_id_col), by=setNames("dcid", omop_column("device_exposure.device_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "device_type_concept_name" = cache$concept_name_col, "dtcid" = cache$concept_id_col), by=setNames("dtcid", omop_column("device_exposure.device_type_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("condition_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("device_exposure.device_exposure_id", cfg), "Name" = "device_concept_name", "SourceVal" = omop_column("device_exposure.device_source_value", cfg),
           "StartDate" = omop_column("device_exposure.device_exposure_start_date", cfg), "StartDateTime" = omop_column("device_exposure.device_exposure_start_datetime", cfg),
           "EndDate" = omop_column("device_exposure.device_exposure_end_date", cfg), "EndDateTime" = omop_column("device_exposure.device_exposure_end_datetime", cfg),
           "Type" = "device_type_concept_name", "DeviceID" = omop_column("device_exposure.unique_device_id", cfg), "Quantity" = omop_column("device_exposure.quantity", cfg),
           "Provider" = omop_column("provider.provider_name", cfg), "Visit" = omop_column("device_exposure.visit_occurrence_id", cfg)) %>%
    collect()
  data_table
}

omop_query_dose_era <- function(input, cfg, cache) {
  data_table <- omop_table("dose_era", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "drug_concept_name" = cache$concept_name_col, "dcid" = cache$concept_id_col), by=setNames("dcid", omop_column("dose_era.drug_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "unit_concept_name" = cache$concept_name_col, "ucid" = cache$concept_id_col), by=setNames("ucid", omop_column("dose_era.unit_concept_id", cfg))) %>%
    select("ID" = omop_column("dose_era.dose_era_id", cfg), "Drug" = "drug_concept_name", "Unit" = "unit_concept_name",
           "DoseValue" = omop_column("dose_era.dose_value", cfg), "StartDate" = omop_column("dose_era.dose_era_start_date", cfg),
           "EndDate" = omop_column("dose_era.dose_era_end_date", cfg)) %>%
    collect()
  data_table
}

omop_query_drug_era <- function(input, cfg, cache) {
  data_table <- omop_table("drug_era", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "drug_concept_name" = cache$concept_name_col, "dcid" = cache$concept_id_col), by=setNames("dcid", omop_column("drug_era.drug_concept_id", cfg))) %>%
    select("ID" = omop_column("drug_era.drug_era_id", cfg), "Drug" = "drug_concept_name", "StartDate" = omop_column("drug_era.drug_era_start_date", cfg),
           "EndDate" = omop_column("drug_era.drug_era_end_date", cfg), "ExposureCount" = omop_column("drug_era.drug_exposure_count", cfg),
           "GapDays" = omop_column("drug_era.gap_days", cfg)) %>%
    collect()
  data_table
}

omop_query_drug_exposure <- function(input, cfg, cache) {
  data_table <- omop_table("drug_exposure", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "drug_concept_name" = cache$concept_name_col, "dcid" = cache$concept_id_col), by=setNames("dcid", omop_column("drug_exposure.drug_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "drug_type_concept_name" = cache$concept_name_col, "dtcid" = cache$concept_id_col), by=setNames("dtcid", omop_column("drug_exposure.drug_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "route_concept_name" = cache$concept_name_col, "rcid" = cache$concept_id_col), by=setNames("rcid", omop_column("drug_exposure.route_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("drug_exposure.provider_id", cfg))) %>%
    select("ID" = omop_column("drug_exposure.drug_exposure_id", cfg), "Drug" = "drug_concept_name", "SourceVal" = omop_column("drug_exposure.drug_source_value", cfg),
           "StartDate" = omop_column("drug_exposure.drug_exposure_start_date", cfg), "StartDateTime" = omop_column("drug_exposure.drug_exposure_start_datetime", cfg),
           "EndDate" = omop_column("drug_exposure.drug_exposure_end_date", cfg), "EndDateTime" = omop_column("drug_exposure.drug_exposure_end_datetime", cfg),
           "VerbatimEnd" = omop_column("drug_exposure.verbatim_end_date", cfg), "Type" = "drug_type_concept_name", "StopReason" = omop_column("drug_exposure.stop_reason", cfg),
           "Refills" = omop_column("drug_exposure.refills", cfg), "Quantity" = omop_column("drug_exposure.quantity", cfg), "DaysSupply" = omop_column("drug_exposure.days_supply", cfg),
           "Sig" = omop_column("drug_exposure.sig", cfg), "Route" = "route_concept_name",
           "Lot" = omop_column("drug_exposure.lot_number", cfg), "Provider" = omop_column("provider.provider_name", cfg), "Visit" = omop_column("drug_exposure.visit_occurrence_id", cfg)) %>%
    collect()
  data_table
}

omop_query_measurement <- function(input, cfg, cache) {
  data_table <- omop_table("measurement", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "measurement_concept_name" = cache$concept_name_col, "mcid" = cache$concept_id_col), by=setNames("mcid", omop_column("measurement.measurement_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "measurement_type_concept_name" = cache$concept_name_col, "mtcid" = cache$concept_id_col), by=setNames("mtcid", omop_column("measurement.measurement_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "operator_concept_name" = cache$concept_name_col, "ocid" = cache$concept_id_col), by=setNames("ocid", omop_column("measurement.operator_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "value_as_concept_name" = cache$concept_name_col, "vacid" = cache$concept_id_col), by=setNames("vacid", omop_column("measurement.value_as_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "unit_concept_name" = cache$concept_name_col, "ucid" = cache$concept_id_col), by=setNames("ucid", omop_column("measurement.unit_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("measurement.provider_id", cfg))) %>%
    select("ID" = omop_column("measurement.measurement_id", cfg), "Name" = "measurement_concept_name", "SourceVal" = omop_column("measurement.measurement_source_value", cfg),
           "Date" = omop_column("measurement.measurement_date", cfg), "DateTime" = omop_column("measurement.measurement_datetime", cfg),
           "Type" = "measurement_type_concept_name", "Operator" = "operator_concept_name", "ValueNum" = omop_column("measurement.value_as_number", cfg),
           "ValueConcept" = "value_as_concept_name", "ValueSource" = omop_column("measurement.value_source_value", cfg), "Unit" = "unit_concept_name",
           "RangeLow" = omop_column("measurement.range_low", cfg), "RangeHigh" = omop_column("measurement.range_high", cfg),
           "Provider" = omop_column("provider.provider_name", cfg), "Visit" = omop_column("measurement.visit_occurrence_id", cfg)) %>%
    collect()
  data_table
}

omop_query_note <- function(input, cfg, cache) {
  data_table <- omop_table("note", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "note_type_concept_name" = cache$concept_name_col, "ntcid" = cache$concept_id_col), by=setNames("ntcid", omop_column("note.note_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "note_class_concept_name" = cache$concept_name_col, "nccid" = cache$concept_id_col), by=setNames("nccid", omop_column("note.note_class_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "encoding_concept_name" = cache$concept_name_col, "ecid" = cache$concept_id_col), by=setNames("ecid", omop_column("note.encoding_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "language_concept_name" = cache$concept_name_col, "lcid" = cache$concept_id_col), by=setNames("lcid", omop_column("note.language_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("condition_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("note.note_id", cfg), "Date" = omop_column("note.note_date", cfg), "DateTime" = omop_column("note.note_datetime", cfg),
           "Type" = "note_type_concept_name", "Class" = "note_class_concept_name", "Title" = omop_column("note.note_title", cfg),
           "Text" = omop_column("note.note_text", cfg), "Encoding" = "encoding_concept_name", "Language" = "language_concept_name",
           "Provider" = omop_column("provider.provider_name", cfg), "Visit" = omop_column("note.visit_occurrence_id", cfg)) %>%
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
#     filter(person_id == as.integer(local(input$subject_id))) %>%
#     select("note_nlp_id", "note_id", "section_concept_id", "section_concept_name", "snippet", "offset", "lexical_variant",
#            "note_nlp_concept_id", "note_nlp_concept_name", "note_nlp_source_concept_id", "note_nlp_source_concept_name",
#            "nlp_system", "nlp_date", "nlp_datetime", "term_exists", "term_temporal", "term_modifiers") %>%
#     collect()
#   data_table
# }

omop_query_observation <- function(input, cfg, cache) {
  data_table <- omop_table("observation", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "observation_concept_name" = cache$concept_name_col, "ocid" = cache$concept_id_col), by=setNames("ocid", omop_column("observation.observation_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "observation_type_concept_name" = cache$concept_name_col, "otcid" = cache$concept_id_col), by=setNames("otcid", omop_column("observation.observation_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "value_as_concept_name" = cache$concept_name_col, "vacid" = cache$concept_id_col), by=setNames("vacid", omop_column("observation.value_as_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "qualifier_concept_name" = cache$concept_name_col, "qcid" = cache$concept_id_col), by=setNames("qcid", omop_column("observation.qualifier_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "unit_concept_name" = cache$concept_name_col, "ucid" = cache$concept_id_col), by=setNames("ucid", omop_column("observation.unit_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("observation.provider_id", cfg))) %>%
    select("ID" = omop_column("observation.observation_id", cfg), "Name" = "observation_concept_name", "Date" = omop_column("observation.observation_date", cfg),
           "DateTime" = omop_column("observation.observation_datetime", cfg), "Type" = "observation_type_concept_name", "ValueNum" = omop_column("observation.value_as_number", cfg),
           "ValueString" = omop_column("observation.value_as_string", cfg), "ValueConcept" = "value_as_concept_name", "SourceVal" = omop_column("observation.observation_source_value", cfg),
           "Qualifier" = "qualifier_concept_name", "Unit" = "unit_concept_name", "Provider" = omop_column("provider.provider_name", cfg), "Visit" = omop_column("observation.visit_occurrence_id", cfg)) %>%
    collect()
  data_table
}

omop_query_observation_period <- function(input, cfg, cache) {
  data_table <- omop_table("observation_period", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "period_type_concept_name" = cache$concept_name_col, "ptcid" = cache$concept_id_col), by=setNames("ptcid", omop_column("observation_period.period_type_concept_id", cfg))) %>%
    select("ID" = omop_column("observation_period.observation_period_id", cfg), "StartDate" = omop_column("observation_period.observation_period_start_date", cfg),
           "EndDate" = omop_column("observation_period.observation_period_end_date", cfg), "Type" = "period_type_concept_name") %>%
    collect()
  data_table
}

omop_query_payer_plan_period <- function(input, cfg, cache) {
  data_table <- omop_table("payer_plan_period", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    select("ID" = omop_column("payer_plan_period.payer_plan_period_id", cfg), "StartDate" = omop_column("payer_plan_period.payer_plan_period_start_date", cfg),
           "EndDate" = omop_column("payer_plan_period.period_plan_period_end_date", cfg), "Payer" = omop_column("payer_plan_period.payer_source_value", cfg),
           "Plan" = omop_column("payer_plan_period.plan_source_value", cfg), "Family" = omop_column("payer_plan_period.family_source_value", cfg)) %>%
    collect()
  data_table
}

omop_query_person <- function(input, cfg, cache) {
  data_table <- omop_table("person", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "gender_concept_name" = cache$concept_name_col, "gcid" = cache$concept_id_col), by=setNames("gcid", omop_column("person.gender_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "race_concept_name" = cache$concept_name_col, "rcid" = cache$concept_id_col), by=setNames("rcid", omop_column("person.race_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "ethnicity_concept_name" = cache$concept_name_col, "ecid" = cache$concept_id_col), by=setNames("ecid", omop_column("person.ethnicity_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("person.provider_id", cfg))) %>%
    select("ID" = omop_column("person.person_id", cfg), "SourceVal" = omop_column("person.person_source_value", cfg),
           "Gender" = "gender_concept_name",
           "BirthYear" = omop_column("person.year_of_birth", cfg), "BirthMonth" = omop_column("person.month_of_birth", cfg), "BirthDay" = omop_column("person.day_of_birth", cfg),
           "BirthDateTime" = omop_column("person.birth_datetime", cfg), "Race" = "race_concept_name",
           "Ethnicity" = "ethnicity_concept_name",
           "Provider" = omop_column("provider.provider_name", cfg)) %>%
    arrange(person_id) %>%
    collect()
  data_table
}

omop_query_procedure_occurrence <- function(input, cfg, cache) {
  data_table <- omop_table("procedure_occurrence", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "procedure_concept_name" = cache$concept_name_col, "pcid" = cache$concept_id_col), by=setNames("pcid", omop_column("procedure_occurrence.procedure_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "procedure_type_concept_name" = cache$concept_name_col, "ptcid" = cache$concept_id_col), by=setNames("ptcid", omop_column("procedure_occurrence.procedure_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "modifier_concept_name" = cache$concept_name_col, "mcid" = cache$concept_id_col), by=setNames("mcid", omop_column("procedure_occurrence.modifier_concept_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("procedure_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("procedure_occurrence.procedure_occurrence_id", cfg), "Procedure" = "procedure_concept_name", "SourceVal" = omop_column("procedure_occurrence.procedure_source_value", cfg),
           "Date" = omop_column("procedure_occurrence.procedure_date", cfg), omop_column("procedure_occurrence.procedure_datetime", cfg), "Type" = "procedure_type_concept_name",
           "Modifier" = "modifier_concept_name", "Qualifier" = omop_column("procedure_occurrence.qualifier_source_value", cfg), "Quantity" = omop_column("procedure_occurrence.quantity", cfg),
           "Provider" = omop_column("provider.provider_name", cfg), "Visit" = omop_column("procedure_occurrence.visit_occurrence_id", cfg)) %>%
    collect()
  data_table
}

omop_query_specimen <- function(input, cfg, cache) {
  data_table <- omop_table("specimen", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "specimen_concept_name" = cache$concept_name_col, "scid" = cache$concept_id_col), by=setNames("scid", omop_column("specimen.specimen_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "specimen_type_concept_name" = cache$concept_name_col, "stcid" = cache$concept_id_col), by=setNames("stcid", omop_column("specimen.specimen_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "unit_concept_name" = cache$concept_name_col, "ucid" = cache$concept_id_col), by=setNames("ucid", omop_column("specimen.unit_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "anatomic_site_concept_name" = cache$concept_name_col, "ascid" = cache$concept_id_col), by=setNames("ascid", omop_column("specimen.anatomic_site_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "disease_status_concept_name" = cache$concept_name_col, "dscid" = cache$concept_id_col), by=setNames("dscid", omop_column("specimen.disease_status_concept_id", cfg))) %>%
    select("ID" = omop_column("specimen.specimen_id", cfg), "Specimen" = "specimen_concept_name", "SourceVal" = omop_column("specimen.specimen_source_value", cfg),
           "Type" = "specimen_type_concept_name", "Date" = omop_column("specimen.specimen_date", cfg), "DateTime" = omop_column("specimen.specimen_datetime", cfg),
           "Quantity" = omop_column("specimen.quantity", cfg), "Unit" = "unit_concept_name", "AnatomicSite" = "anatomic_site_concept_name", "DiseaseStatus" = "disease_status_concept_name") %>%
    collect()
  data_table
}

omop_query_visit_occurrence <- function(input, cfg, cache) {
  data_table <- omop_table("visit_occurrence", cfg) %>%
    filter(person_id == as.integer(local(input$subject_id))) %>%
    left_join(select(cache$concept_tbl, "visit_concept_name" = cache$concept_name_col, "vcid" = cache$concept_id_col), by=setNames("vcid", omop_column("visit_occurrence.visit_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "visit_type_concept_name" = cache$concept_name_col, "vtcid" = cache$concept_id_col), by=setNames("vtcid", omop_column("visit_occurrence.visit_type_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "admitting_source_concept_name" = cache$concept_name_col, "ascid" = cache$concept_id_col), by=setNames("ascid", omop_column("visit_occurrence.admitting_source_concept_id", cfg))) %>%
    left_join(select(cache$concept_tbl, "discharge_to_concept_name" = cache$concept_name_col, "dtcid" = cache$concept_id_col), by=setNames("dtcid", omop_column("visit_occurrence.discharge_to_concept_id", cfg))) %>%
    left_join(select(omop_table("care_site", cfg), "care_site_name" = omop_column("care_site.care_site_name", cfg), "csid" = omop_column("care_site.care_site_id", cfg)), by=setNames("csid", omop_column("visit_occurrence.care_site_id", cfg))) %>%
    left_join(select(omop_table("provider", cfg), omop_column("provider.provider_name", cfg), "prvid" = omop_column("provider.provider_id", cfg)), by=setNames("prvid", omop_column("visit_occurrence.provider_id", cfg))) %>%
    select("ID" = omop_column("visit_occurrence.visit_occurrence_id", cfg), "Visit" = "visit_concept_name", "StartDate" = omop_column("visit_occurrence.visit_start_date", cfg),
           "StartDateTime" = omop_column("visit_occurrence.visit_start_datetime", cfg), "EndDate" = omop_column("visit_occurrence.visit_end_date", cfg),
           "EndDateTime" = omop_column("visit_occurrence.visit_end_datetime", cfg), "Type" = "visit_type_concept_name", "Provider" = omop_column("provider.provider_name", cfg),
           "CareSite" = "care_site_name", "AdmittingSource" = "admitting_source_concept_name", "DischargeTo" = "discharge_to_concept_name", "PrecedingVisit" = omop_column("visit_occurrence.preceding_visit_occurrence_id", cfg)) %>%
    collect()
  data_table
}

omop_render_data_tables <- function(input, output, cfg) {
  if (is.null(cfg)) { return(output) }
  
  # Cache commonly used tables and column names
  table_cache = list(concept_id_col = omop_column("concept.concept_id", cfg),
                      concept_name_col = omop_column("concept.concept_name", cfg),
                      concept_tbl = omop_table("concept", cfg))
  
  condition_era_data <- reactive({omop_query_condition_era(input, cfg, table_cache)})
  condition_occurrence_data <- reactive({omop_query_condition_occurrence(input, cfg, table_cache)})
  death_data <- reactive({omop_query_death(input, cfg, table_cache)})
  device_exposure_data <- reactive({omop_query_device_exposure(input, cfg, table_cache)})
  dose_era_data <- reactive({omop_query_dose_era(input, cfg, table_cache)})
  drug_era_data <- reactive({omop_query_drug_era(input, cfg, table_cache)})
  drug_exposure_data <- reactive({omop_query_drug_exposure(input, cfg, table_cache)})
  measurement_data <- reactive({omop_query_measurement(input, cfg, table_cache)})
  note_data <- reactive({omop_query_note(input, cfg, table_cache)})
  #note_nlp_data <- reactive({omop_query_note_nlp(input, cfg)})
  observation_data <- reactive({omop_query_observation(input, cfg, table_cache)})
  observation_period_data <- reactive({omop_query_observation_period(input, cfg, table_cache)})
  payer_plan_period_data <- reactive({omop_query_payer_plan_period(input, cfg, table_cache)})
  person_data <- reactive({omop_query_person(input, cfg, table_cache)})
  procedure_occurrence_data <- reactive({omop_query_procedure_occurrence(input, cfg, table_cache)})
  specimen_data <- reactive({omop_query_specimen(input, cfg, table_cache)})
  visit_occurrence_data <- reactive({omop_query_visit_occurrence(input, cfg, table_cache)})

  #all_people <- omop_query_all_people(cfg, table_cache)
  output$all_patients_tbl <- DT::renderDataTable(
    omop_query_all_people(cfg, table_cache) %>% mutate(ID = paste0("<a class='row_subject_id' href='#'>", ID, "</a>")),
    options = list(paging = TRUE, pageLength = 20, searchHighlight = TRUE,scrollY = TRUE,search = list(regex = TRUE, caseInsensitive = TRUE)),
    escape=FALSE, rownames=F, selection='none',
    callback = JS(
      'table.on("click", "tr td a.row_subject_id", function() {
      Shiny.onInputChange("subject_id", $(this).text());
      $(".main-sidebar li a").click();
    });'))

  #output$subject_id <- renderUI({selectInput("subject_id", label = NULL, selectize=TRUE, selected = local(input$subject_id), choices = all_people$ID)})
  output$condition_era_tbl <- DT::renderDataTable(condition_era_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$condition_occurrence_tbl <- DT::renderDataTable(condition_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$death_tbl <- DT::renderDataTable(death_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$device_exposure_tbl <- DT::renderDataTable(device_exposure_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$dose_era_tbl <- DT::renderDataTable(dose_era_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$drug_era_tbl <- DT::renderDataTable(drug_era_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$drug_exposure_tbl <- DT::renderDataTable(drug_exposure_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$measurement_tbl <- DT::renderDataTable(measurement_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$note_tbl <- DT::renderDataTable(note_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  #output$note_nlp_tbl <- DT::renderDataTable(note_nlp_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE), rownames=F, selection='none')
  output$observation_tbl <- DT::renderDataTable(observation_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$observation_period_tbl <- DT::renderDataTable(observation_period_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$payer_plan_period_tbl <- DT::renderDataTable(payer_plan_period_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$person_tbl <- DT::renderDataTable(person_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$procedure_occurrence_tbl <- DT::renderDataTable(procedure_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$specimen_tbl <- DT::renderDataTable(specimen_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output$visit_occurrence_tbl <- DT::renderDataTable(visit_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE, scrollX = TRUE, scrollY = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), rownames=F, selection='none')
  output
}
