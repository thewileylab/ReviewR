omop_get_review_table_names <- function() {
  c("person", "observation_period", "specimen", "death", "visit_detail", "visit_occurrence", "procedure_diagnosis",
    "drug_exposure", "device_exposure", "condition_occurrence", "measurement", "note", "note_nlp", "observation",
    "fact_relationship")
}

omop_format_table_name <- function(table_key, table_config, db_config) {
  paste0(db_config["schema"], ".", table_config[table_key])
}

omop_render_data_tables <- function(table_config, db_config, input, output, database_type, connection) {
  if (is.null(connection)) { return(output) }
  condition_occurrence_data <- reactive({query_condition_occurrence(table_config, db_config, input, app_config["database"], connection)})
  #condition_era_data <- reactive({query_condition_era(table_config, db_config, input, app_config["database"], connection)})
  
  output$all_patients_tbl <- DT::renderDataTable(
    query_all_people(table_config, db_config, app_config["database"], connection),
    options = list(paging = TRUE, pageLength = 20, searchHighlight = TRUE),
    escape=FALSE, rownames=F,
    callback = JS(
      'table.on("click", "tr td a.row_subject_id", function() {
      Shiny.onInputChange("subject_id", $(this).text());
      $(".main-sidebar li a").click();
});'))
    
  output$condition_occurrence_tbl <- DT::renderDataTable(condition_occurrence_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$admissions_tbl <- DT::renderDataTable(admissions_data(), options = list(paging = FALSE, searchHighlight = TRUE),
  #                                              escape=FALSE, rownames=F,
  #                                              callback = JS(
  #                                                'table.on("click", "tr td a.row_hadm_id", function() {
  #                                                Shiny.onInputChange("hadm_id", $(this).text());
  #                                                });'))
  # output$callout_tbl <- DT::renderDataTable(callout_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$chartevents_tbl <- DT::renderDataTable(chartevents_data(), filter = 'top', options = list(paging = TRUE, searchHighlight = TRUE), rownames=F)
  # output$cptevents_tbl <- DT::renderDataTable(cptevents_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F) 
  # output$diagnoses_icd_tbl <- DT::renderDataTable(diagnoses_icd_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$drgcodes_tbl <- DT::renderDataTable(drgcodes_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$icustays_tbl <- DT::renderDataTable(icustays_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F) 
  # output$labevents_tbl <- DT::renderDataTable(labevents_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$microbiologyevents_tbl <- DT::renderDataTable(microbiologyevents_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$noteevent_tbl <- DT::renderDataTable(noteevent_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$all_patients_tbl <- DT::renderDataTable(
  #   query_all_patients(table_config, db_config, app_config["database"], connection),
  #   options = list(paging = TRUE, pageLength = 20, searchHighlight = TRUE),
  #   escape=FALSE, rownames=F,
  #   callback = JS(
  #     'table.on("click", "tr td a.row_subject_id", function() {
  #        Shiny.onInputChange("subject_id", $(this).text());
  #        $(".main-sidebar li a").click();
  #      });'))
  # output$patients_tbl <- DT::renderDataTable(patients_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$prescriptions_tbl <- DT::renderDataTable(prescriptions_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$procedureevents_mv_tbl <- DT::renderDataTable(procedureevents_mv_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$procedures_icd_tbl <- DT::renderDataTable(procedures_icd_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$services_labels_tbl <- renderTable(services_labels)
  # output$services_tbl <-  DT::renderDataTable(services_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  # output$transfers_tbl <- DT::renderDataTable(transfers_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output
}

omop_query_condition_occurrence <- function(table_config, db_config, input, database_type, connection) {
  # query_text <- paste0("select * from ",
  #                      omop_format_table_name("condition_occurrence", table_config, db_config),
  #                      " where subject_id = ", as.numeric(input$subject_id))
  # if (database_type == "bigquery") {
  #   query_exec(query_text, connection)
  # }
  # else {
  #   dbGetQuery(connection, query_text)
  # }
  
  data.frame(condition_occurrence_id=c(1:5),
             condition_concept=c("Diabetes mellitus", "Diabetes mellitus", "Hypertension", "Diabetes mellitus", "Hypertension"),
             condition_start=c("1/1/1900", "5/1/1900", "5/1/1900", "12/1/1900", "12/1/1900"),
             condition_end=c("", "", "", "", ""))
}

omop_query_condition_era <- function(table_config, db_config, input, database_type, connection) {
  query_text <- paste0("select * from ",
                       omop_format_table_name("condition_era", table_config, db_config),
                       " where subject_id = ", as.numeric(input$subject_id))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

omop_query_all_people <- function(table_config, db_config, database_type, connection) {
  df <- data.frame(person_id=c(1:18),
             gender=c("Female", "Male", "Male", "Female", "Female", "Male","Female", "Male", "Male", "Female", "Female", "Male","Female", "Male", "Male", "Female", "Female", "Male"),
             birth_datetime=c("1/1/1900", "1/1/1900", "1/1/1900", "1/1/1900", "1/1/1900", "1/1/1900","1/1/1900", "1/1/1900", "1/1/1900", "1/1/1900", "1/1/1900", "1/1/1900","1/1/1900", "1/1/1900", "1/1/1900", "1/1/1900", "1/1/1900", "1/1/1900"),
             race=c("White", "Unknown", "Native Hawaiian or Other Pacific Islander", "White", "Unknown", "White","White", "Unknown", "Native Hawaiian or Other Pacific Islander", "White", "Unknown", "White","White", "Unknown", "Native Hawaiian or Other Pacific Islander", "White", "Unknown", "White"),
             ethnicity=c("Hispanic or Latino", "Hispanic or Latino", "Not Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Not Hispanic or Latino","Hispanic or Latino", "Hispanic or Latino", "Not Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Not Hispanic or Latino","Hispanic or Latino", "Hispanic or Latino", "Not Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Not Hispanic or Latino"))
  df %>%
    mutate(person_id = paste0("<a class='row_subject_id' href='#'>", person_id, "</a>"))
}

omop_query_all_people_tmp <- function(table_config, db_config, database_type, connection) {
  query_text <- paste0("select * from ",
                       omop_format_table_name("person", table_config, db_config))
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}