omop_get_review_table_names <- function() {
  c("condition_era", "condition_occurrence", "death", "device_exposure", "dose_era", "drug_era", "drug_exposure", "measurement", 
    "note", "note_nlp", "observation", "observation_period", "payer_plan_period", "person", "procedure_occurrence", "specimen", "visit_occurrence")
}

omop_format_table_name <- function(table_key, table_config, db_config) {
  paste0(db_config["schema"], ".", table_config[table_key])
}

omop_query_all_people <- function(connection) {
  data_table <- tbl(connection, "person") %>%
    left_join(tbl(connection, "concept"), by=c("gender_concept_id" = "concept_id"), suffix = c(".p", ".gc")) %>%
    rename("gender_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("race_concept_id" = "concept_id"), suffix = c(".p", ".rc")) %>%
    rename("race_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("ethnicity_concept_id" = "concept_id"), suffix = c(".p", ".ec")) %>%
    rename("ethnicity_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    select("person_id", "person_source_value", "gender_concept_id" = "gender_concept_id.p", "gender_concept_name",
           "gender_source_value" = "gender_source_value.p", "gender_source_concept_id" = "gender_source_concept_id.p",
           "year_of_birth" = "year_of_birth.p", "month_of_birth", "day_of_birth",
           "birth_datetime", "race_concept_id", "race_concept_name", "race_source_value", "race_source_concept_id",
           "ethnicity_concept_id", "ethnicity_concept_name", "ethnicity_source_value", "ethnicity_source_concept_id",
           "provider_id", "provider_name") %>%
    collect() %>%
    mutate(person_id = paste0("<a class='row_subject_id' href='#'>", person_id, "</a>"))
  data_table
}

omop_query_condition_era <- function(input, connection) {
  data_table <- tbl(connection, "condition_era") %>%
    left_join(tbl(connection, "concept"), by=c("condition_concept_id" = "concept_id")) %>%
    filter(person_id == input$subject_id) %>%
    select("condition_era_id", "condition_concept_id", "condition_concept_name" = "concept_name",
           "condition_era_start_date", "condition_era_end_date", "condition_occurrence_count") %>%
    collect()
  data_table
}

omop_query_condition_occurrence <- function(input, connection) {
  data_table <- tbl(connection, "condition_occurrence") %>%
    left_join(tbl(connection, "concept"), by=c("condition_concept_id" = "concept_id")) %>%
    rename("condition_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("condition_type_concept_id" = "concept_id")) %>%
    rename("condition_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("condition_source_concept_id" = "concept_id")) %>%
    rename("condition_source_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("condition_status_concept_id" = "concept_id")) %>%
    rename("condition_status_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("condition_occurrence_id", "condition_concept_id", "condition_concept_name",
           "condition_source_value", "condition_source_concept_id", "condition_source_concept_name",
           "condition_start_date", "condition_start_datetime", "condition_end_date", "condition_end_datetime",
           "condition_type_concept_id", "condition_type_concept_name", "stop_reason",
           "provider_id", "provider_name", "visit_occurrence_id",
           "condition_status_source_value", "condition_status_concept_id", "condition_status_concept_name") %>%
    collect()
  data_table
}

omop_query_death <- function(input, connection) {
  data_table <- tbl(connection, "death") %>%
    left_join(tbl(connection, "concept"), by=c("death_type_concept_id" = "concept_id")) %>%
    rename("death_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("cause_concept_id" = "concept_id")) %>%
    rename("cause_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("cause_source_concept_id" = "concept_id")) %>%
    rename("cause_source_concept_name" = "concept_name") %>%
    filter(person_id == input$subject_id) %>%
    select("death_date", "death_datetime", "death_type_concept_id", "death_type_concept_name",
          "cause_concept_id", "cause_concept_name", "cause_source_value",
          "cause_source_concept_id", "cause_source_concept_name") %>%
    collect()
  data_table
}

omop_query_device_exposure <- function(input, connection) {
  data_table <- tbl(connection, "device_exposure") %>%
    left_join(tbl(connection, "concept"), by=c("device_concept_id" = "concept_id")) %>%
    rename("device_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("device_source_concept_id" = "concept_id")) %>%
    rename("device_source_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("device_type_concept_id" = "concept_id")) %>%
    rename("device_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("device_exposure_id", "device_concept_id", "device_concept_name",
          "device_source_value", "device_source_concept_id", "device_source_concept_name",
          "device_exposure_start_date", "device_exposure_start_datetime", "device_exposure_end_date", "device_exposure_end_datetime",
          "device_type_concept_id", "device_type_concept_name", "unique_device_id", "quantity",
          "provider_id", "provider_name", "visit_occurrence_id") %>%
    collect()
  data_table
}

omop_query_dose_era <- function(input, connection) {
  data_table <- tbl(connection, "dose_era") %>%
    left_join(tbl(connection, "concept"), by=c("drug_concept_id" = "concept_id")) %>%
    rename("drug_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("unit_concept_id" = "concept_id")) %>%
    rename("unit_concept_name" = "concept_name") %>%
    filter(person_id == input$subject_id) %>%
    select("dose_era_id", "drug_concept_id", "drug_concept_name", "unit_concept_id", "unit_concept_name",
          "dose_value", "dose_era_start_date", "dose_era_end_date") %>%
    collect()
  data_table
}

omop_query_drug_era <- function(input, connection) {
  data_table <- tbl(connection, "drug_era") %>%
    left_join(tbl(connection, "concept"), by=c("drug_concept_id" = "concept_id")) %>%
    rename("drug_concept_name" = "concept_name") %>%
    filter(person_id == input$subject_id) %>%
    select("drug_era_id", "drug_concept_id", "drug_concept_name", "drug_era_start_date", "drug_era_end_date",
           "drug_exposure_count", "gap_days") %>%
    collect()
  data_table
}

omop_query_drug_exposure <- function(input, connection) {
  data_table <- tbl(connection, "drug_exposure") %>%
    left_join(tbl(connection, "concept"), by=c("drug_concept_id" = "concept_id")) %>%
    rename("drug_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("drug_source_concept_id" = "concept_id")) %>%
    rename("drug_source_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("drug_type_concept_id" = "concept_id")) %>%
    rename("drug_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("route_concept_id" = "concept_id")) %>%
    rename("route_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("drug_exposure_id", "drug_concept_id", "drug_concept_name", "drug_source_value", "drug_source_concept_id", "drug_source_concept_name",
          "drug_exposure_start_date", "drug_exposure_start_datetime", "drug_exposure_end_date", "drug_exposure_end_datetime", "verbatim_end_date",
          "drug_type_concept_id", "drug_type_concept_name", "stop_reason", "refills", "quantity", "days_supply", "sig",
          "route_concept_id", "route_concept_name", "route_source_value", "lot_number", "provider_id", "provider_name",
          "visit_occurrence_id", "dose_unit_source_value") %>%
    collect()
  data_table
}

omop_query_measurement <- function(input, connection) {
  data_table <- tbl(connection, "measurement") %>%
    left_join(tbl(connection, "concept"), by=c("measurement_concept_id" = "concept_id")) %>%
    rename("measurement_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("measurement_source_concept_id" = "concept_id")) %>%
    rename("measurement_source_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("measurement_type_concept_id" = "concept_id")) %>%
    rename("measurement_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("operator_concept_id" = "concept_id")) %>%
    rename("operator_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("value_as_concept_id" = "concept_id")) %>%
    rename("value_as_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("unit_concept_id" = "concept_id")) %>%
    rename("unit_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("measurement_id", "measurement_concept_id", "measurement_concept_name", "measurement_source_value", "measurement_source_concept_id",
           "measurement_source_concept_name", "measurement_date", "measurement_datetime", "measurement_type_concept_id", 
           "measurement_type_concept_name", "operator_concept_id", "operator_concept_name", "value_as_number", "value_as_concept_id", 
           "value_as_concept_name", "value_source_value", "unit_concept_id", "unit_concept_name", "unit_source_value", "range_low", "range_high",
           "provider_id", "provider_name", "visit_occurrence_id") %>%
    collect()
  data_table
}

omop_query_note <- function(input, connection) {
  data_table <- tbl(connection, "note") %>%
    left_join(tbl(connection, "concept"), by=c("note_type_concept_id" = "concept_id")) %>%
    rename("note_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("note_class_concept_id" = "concept_id")) %>%
    rename("note_class_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("encoding_concept_id" = "concept_id")) %>%
    rename("encoding_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("language_concept_id" = "concept_id")) %>%
    rename("language_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("note_id", "note_date", "note_datetime", "note_type_concept_id", "note_type_concept_name",
            "note_class_concept_id", "note_class_concept_name", "note_title", "note_text",
            "encoding_concept_id", "encoding_concept_name", "language_concept_id", "language_concept_name",
            "provider_id", "provider_name", "visit_occurrence_id", "note_source_value") %>%
    collect()
  data_table
}

omop_query_note_nlp <- function(input, connection) {
  data_table <- tbl(connection, "note_nlp") %>%
    inner_join(tbl(connection, "note"), by=c("note_id" = "note_id")) %>%
    left_join(tbl(connection, "concept"), by=c("section_concept_id" = "concept_id")) %>%
    rename("section_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("note_nlp_concept_id" = "concept_id")) %>%
    rename("note_nlp_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("note_nlp_source_concept_id" = "concept_id")) %>%
    rename("note_nlp_source_concept_name" = "concept_name") %>%
    filter(person_id == input$subject_id) %>%
    select("note_nlp_id", "note_id", "section_concept_id", "section_concept_name", "snippet", "offset", "lexical_variant",
           "note_nlp_concept_id", "note_nlp_concept_name", "note_nlp_source_concept_id", "note_nlp_source_concept_name",
           "nlp_system", "nlp_date", "nlp_datetime", "term_exists", "term_temporal", "term_modifiers") %>%
    collect()
  data_table
}

omop_query_observation <- function(input, connection) {
  data_table <- tbl(connection, "observation") %>%
    left_join(tbl(connection, "concept"), by=c("observation_concept_id" = "concept_id")) %>%
    rename("observation_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("observation_type_concept_id" = "concept_id")) %>%
    rename("observation_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("value_as_concept_id" = "concept_id")) %>%
    rename("value_as_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("observation_source_concept_id" = "concept_id")) %>%
    rename("observation_source_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("qualifier_concept_id" = "concept_id")) %>%
    rename("qualifier_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("unit_concept_id" = "concept_id")) %>%
    rename("unit_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("observation_id", "observation_concept_id", "observation_concept_name", "observation_date", "observation_datetime",
          "observation_type_concept_id", "observation_type_concept_name", "value_as_number", "value_as_string",
          "value_as_concept_id", "value_as_concept_name", "observation_source_value", "observation_source_concept_id",
          "observation_source_concept_name", "qualifier_concept_id", "qualifier_concept_name", "qualifier_source_value",
          "unit_concept_id", "unit_concept_name", "unit_source_value", "provider_id", "provider_name", "visit_occurrence_id") %>%
    collect()
  data_table
}

omop_query_observation_period <- function(input, connection) {
  data_table <- tbl(connection, "observation_period") %>%
    left_join(tbl(connection, "concept"), by=c("period_type_concept_id" = "concept_id")) %>%
    rename("period_type_concept_name" = "concept_name") %>%
    filter(person_id == input$subject_id) %>%
    select("observation_period_id", "observation_period_start_date", "observation_period_end_date", "period_type_concept_id", "period_type_concept_name") %>%
    collect()
  data_table
}

omop_query_payer_plan_period <- function(input, connection) {
  data_table <- tbl(connection, "payer_plan_period") %>%
    filter(person_id == input$subject_id) %>%
    select("payer_plan_period_id", "payer_plan_period_start_date", "payer_plan_period_end_date", "payer_source_value",
           "plan_source_value", "family_source_value") %>%
    collect()
  data_table
}

omop_query_person <- function(input, connection) {
  data_table <- tbl(connection, "person") %>%
    left_join(tbl(connection, "concept"), by=c("gender_concept_id" = "concept_id")) %>%
    rename("gender_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("race_concept_id" = "concept_id")) %>%
    rename("race_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("ethnicity_concept_id" = "concept_id")) %>%
    rename("ethnicity_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("person_id", "person_source_value", "gender_concept_id.p", "gender_concept_name", "gender_source_value.p", "gender_source_concept_id.p",
          "year_of_birth.p", "month_of_birth", "day_of_birth", "birth_datetime", "race_concept_id", "race_concept_name", "race_source_value",
          "race_source_concept_id", "ethnicity_concept_id", "ethnicity_concept_name", "ethnicity_source_value", "ethnicity_source_concept_id",
          "provider_id", "provider_name") %>%
    collect()
  data_table
}

omop_query_procedure_occurrence <- function(input, connection) {
  data_table <- tbl(connection, "procedure_occurrence") %>%
    left_join(tbl(connection, "concept"), by=c("procedure_concept_id" = "concept_id")) %>%
    rename("procedure_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("procedure_source_concept_id" = "concept_id")) %>%
    rename("procedure_source_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("procedure_type_concept_id" = "concept_id")) %>%
    rename("procedure_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("modifier_concept_id" = "concept_id")) %>%
    rename("modifier_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("procedure_occurrence_id", "procedure_concept_id", "procedure_concept_name", "procedure_source_value", "procedure_source_concept_id",
          "procedure_source_concept_name", "procedure_date", "procedure_datetime", "procedure_type_concept_id", "procedure_type_concept_name",
          "modifier_concept_id", "modifier_concept_name", "qualifier_source_value", "quantity", "provider_id", "provider_name",
          "visit_occurrence_id") %>%
    collect()
  data_table
}

omop_query_specimen <- function(input, connection) {
  data_table <- tbl(connection, "specimen") %>%
    left_join(tbl(connection, "concept"), by=c("specimen_concept_id" = "concept_id")) %>%
    rename("specimen_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("specimen_type_concept_id" = "concept_id")) %>%
    rename("specimen_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("unit_concept_id" = "concept_id")) %>%
    rename("unit_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("anatomic_site_concept_id" = "concept_id")) %>%
    rename("anatomic_site_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("disease_status_concept_id" = "concept_id")) %>%
    rename("disease_status_concept_name" = "concept_name") %>%
    filter(person_id == input$subject_id) %>%
    select("specimen_id", "specimen_concept_id", "specimen_concept_name", "specimen_source_id", "specimen_source_value", "specimen_type_concept_id",
          "specimen_type_concept_name", "specimen_date", "specimen_datetime", "quantity", "unit_concept_id", "unit_concept_name",
          "unit_source_value", "anatomic_site_concept_id", "anatomic_site_concept_name", "anatomic_site_source_value", "disease_status_concept_id",
          "disease_status_concept_name", "disease_status_source_value") %>%
    collect()
  data_table
}

omop_query_visit_occurrence <- function(input, connection) {
  data_table <- tbl(connection, "visit_occurrence") %>%
    left_join(tbl(connection, "concept"), by=c("visit_concept_id" = "concept_id")) %>%
    rename("visit_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("visit_source_concept_id" = "concept_id")) %>%
    rename("visit_source_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("visit_type_concept_id" = "concept_id")) %>%
    rename("visit_type_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("admitting_source_concept_id" = "concept_id")) %>%
    rename("admitting_source_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "concept"), by=c("discharge_to_concept_id" = "concept_id")) %>%
    rename("discharge_to_concept_name" = "concept_name") %>%
    left_join(tbl(connection, "care_site"), by=c("care_site_id" = "care_site_id"), suffix = c(".p", ".cs")) %>%
    left_join(tbl(connection, "provider"), by=c("provider_id" = "provider_id"), suffix = c(".p", ".prv")) %>%
    filter(person_id == input$subject_id) %>%
    select("visit_occurrence_id", "visit_concept_id", "visit_concept_name", "visit_source_value", "visit_source_concept_id",
          "visit_source_concept_name", "visit_start_date", "visit_start_datetime", "visit_end_date", "visit_end_datetime",
          "visit_type_concept_id", "visit_type_concept_name", "provider_id", "provider_name", "care_site_id.p", "care_site_name",
          "admitting_source_concept_id", "admitting_source_concept_name", "admitting_source_value", "discharge_to_concept_id",
          "discharge_to_concept_name", "discharge_to_source_value", "preceding_visit_occurrence_id") %>%
    collect()
  data_table
}

omop_render_data_tables <- function(input, output, connection) {
  if (is.null(connection)) { return(output) }
  
  condition_era_data <- reactive({omop_query_condition_era(input, connection)})
  condition_occurrence_data <- reactive({omop_query_condition_occurrence(input, connection)})
  death_data <- reactive({omop_query_death(input, connection)})
  device_exposure_data <- reactive({omop_query_device_exposure(input, connection)})
  dose_era_data <- reactive({omop_query_dose_era(input, connection)})
  drug_era_data <- reactive({omop_query_drug_era(input, connection)})
  drug_exposure_data <- reactive({omop_query_drug_exposure(input, connection)})
  measurement_data <- reactive({omop_query_measurement(input, connection)})
  note_data <- reactive({omop_query_note(input, connection)})
  note_nlp_data <- reactive({omop_query_note_nlp(input, connection)})
  observation_data <- reactive({omop_query_observation(input, connection)})
  observation_period_data <- reactive({omop_query_observation_period(input, connection)})
  payer_plan_period_data <- reactive({omop_query_payer_plan_period(input, connection)})
  person_data <- reactive({omop_query_person(input, connection)})
  procedure_occurrence_data <- reactive({omop_query_procedure_occurrence(input, connection)})
  specimen_data <- reactive({omop_query_specimen(input, connection)})
  visit_occurrence_data <- reactive({omop_query_visit_occurrence(input, connection)})

  output$all_patients_tbl <- DT::renderDataTable(
    omop_query_all_people(connection),
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
  output$note_nlp_tbl <- DT::renderDataTable(note_nlp_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$observation_tbl <- DT::renderDataTable(observation_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$observation_period_tbl <- DT::renderDataTable(observation_period_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$payer_plan_period_tbl <- DT::renderDataTable(payer_plan_period_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$person_tbl <- DT::renderDataTable(person_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$procedure_occurrence_tbl <- DT::renderDataTable(procedure_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$specimen_tbl <- DT::renderDataTable(specimen_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output$visit_occurrence_tbl <- DT::renderDataTable(visit_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F, selection='none')
  output
}
