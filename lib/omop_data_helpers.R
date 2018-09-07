omop_get_review_table_names <- function() {
  c("person", "observation_period", "specimen", "death", "visit_detail", "visit_occurrence", "procedure_diagnosis",
    "drug_exposure", "device_exposure", "condition_occurrence", "measurement", "note", "note_nlp", "observation",
    "fact_relationship")
}

omop_format_table_name <- function(table_key, table_config, db_config) {
  paste0(db_config["schema"], ".", table_config[table_key])
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
  # query_text <- paste0("select * from ",
  #                      omop_format_table_name("condition_occurrence", table_config, db_config),
  #                      " where person_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_all_people <- function(table_config, db_config, database_type, connection) {
  query_text <- paste0("select person_id, person_source_value, person.gender_concept_id as gender_concept_id, gc.concept_name as gender_concept_name, person.gender_source_value as gender_source_value, person.gender_source_concept_id as gender_source_concept_id, person.year_of_birth as year_of_birth, month_of_birth, day_of_birth, birth_datetime, ",
                       "race_concept_id, rc.concept_name as race_concept_name, race_source_value, race_source_concept_id, ethnicity_concept_id, ec.concept_name as ethnicity_concept_name, ethnicity_source_value, ethnicity_source_concept_id, person.provider_id as provider_id, p.provider_name as provider_name ",
                       "from ", omop_format_table_name("person", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " gc ON gc.concept_id = person.gender_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " rc ON rc.concept_id = person.race_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " ec ON ec.concept_id = person.ethnicity_concept_id ",
                       "left outer join ", omop_format_table_name("provider", table_config, db_config), " p ON p.provider_id = person.provider_id ")
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(person_id = paste0("<a class='row_subject_id' href='#'>", person_id, "</a>"))
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(person_id = paste0("<a class='row_subject_id' href='#'>", person_id, "</a>"))
  }
}

omop_query_person <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select person_id, person_source_value, person.gender_concept_id as gender_concept_id, gc.concept_name as gender_concept_name, person.gender_source_value as gender_source_value, person.gender_source_concept_id as gender_source_concept_id, person.year_of_birth as year_of_birth, month_of_birth, day_of_birth, birth_datetime, ",
                       "race_concept_id, rc.concept_name as race_concept_name, race_source_value, race_source_concept_id, ethnicity_concept_id, ec.concept_name as ethnicity_concept_name, ethnicity_source_value, ethnicity_source_concept_id, person.provider_id as provider_id, p.provider_name as provider_name ",
                       "from ", omop_format_table_name("person", table_config, db_config), " ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " gc ON gc.concept_id = person.gender_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " rc ON rc.concept_id = person.race_concept_id ",
                       "left outer join ", omop_format_table_name("concept", table_config, db_config), " ec ON ec.concept_id = person.ethnicity_concept_id ",
                       "left outer join ", omop_format_table_name("provider", table_config, db_config), " p ON p.provider_id = person.provider_id ",
                       " where person_id = ", as.numeric(input$subject_id))
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

omop_render_data_tables <- function(table_config, db_config, input, output, database_type, connection) {
  if (is.null(connection)) { return(output) }
  
  condition_occurrence_data <- reactive({omop_query_condition_occurrence(table_config, db_config, input, database_type, connection)})
  condition_era_data <- reactive({omop_query_condition_era(table_config, db_config, input, database_type, connection)})
  person_data <- reactive({omop_query_person(table_config, db_config, input, database_type, connection)})
  observation_period_data <- reactive({omop_query_observation_period(table_config, db_config, input, database_type, connection)})
  specimen_data <- reactive({omop_query_specimen(table_config, db_config, input, database_type, connection)})
  death_data <- reactive({omop_query_death(table_config, db_config, input, database_type, connection)})
  #"visit_detail", "visit_occurrence", "procedure_diagnosis",
  #"drug_exposure", "device_exposure", "measurement", "note", "note_nlp", "observation",
  #"fact_relationship"
  
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
  output$condition_occurrence_tbl <- DT::renderDataTable(condition_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$observation_period_tbl <- DT::renderDataTable(observation_period_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$specimen_tbl <- DT::renderDataTable(specimen_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$death_tbl <- DT::renderDataTable(death_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output
}
