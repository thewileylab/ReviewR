chart_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(id = 'patient_chart',type = 'tabs',
                tabPanel(title = 'Condition Era', dataTableOutput(ns('condition_era_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Condition Occurrence', dataTableOutput(ns('condition_occurrence_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Death', dataTableOutput(ns('death_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Device Exposure', dataTableOutput(ns('device_exposure_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Dose Era', dataTableOutput(ns('dose_era_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Drug Era', dataTableOutput(ns('drug_era_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Drug Exposure', dataTableOutput(ns('drug_exposure_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Measurement', dataTableOutput(ns('measurement_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Note', dataTableOutput(ns('note_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Observation', dataTableOutput(ns('observation_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Observation Period', dataTableOutput(ns('observation_period_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Payer Plan Period', dataTableOutput(ns('payer_plan_period_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Procedure Occurrence', dataTableOutput(ns('procedure_occurrence_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Specimen', dataTableOutput(ns('specimen_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Visit Occurrence', dataTableOutput(ns('visit_occurrence_dt')) %>% withSpinner(type = 6))
                )
  )
}

chart_review_logic <- function(input, output, session, table_map, db_connection, subject_id) {
  source('lib/reviewr_datatable.R')
  ns <- session$ns
  
  ## Define Reactive tibbles which update every time the subject_id() variable changes -----
  condition_era <- reactive({
    req(subject_id() )
    omop_table_condition_era(table_map, db_connection, subject_id)
    })
  
  condition_occurrence <- reactive({
    req(subject_id() )
    omop_table_condition_occurrence(table_map, db_connection, subject_id)
  })
  
  death <- reactive({
    req(subject_id() )
    omop_table_death(table_map, db_connection, subject_id)
  })
  
  device_exposure <- reactive({
    req(subject_id() )
    omop_table_device_exposure(table_map, db_connection, subject_id)
  })
  
  dose_era <- reactive({
    req(subject_id() )
    omop_table_dose_era(table_map, db_connection, subject_id)
  })
  
  drug_era <- reactive({
    req(subject_id() )
    omop_table_drug_era(table_map, db_connection, subject_id)
  })
  
  drug_exposure <- reactive({
    req(subject_id() )
    omop_table_drug_exposure(table_map, db_connection, subject_id)
  })
  
  measurement <- reactive({
    req(subject_id() )
    omop_table_measurement(table_map, db_connection, subject_id)
  })
  
  note <- reactive({
    req(subject_id() )
    omop_table_note(table_map, db_connection, subject_id)
  })
  
  observation <- reactive({
    req(subject_id() )
    omop_table_observation(table_map, db_connection, subject_id)
  })
  
  observation_period <- reactive({
    req(subject_id() )
    omop_table_observation_period(table_map, db_connection, subject_id)
  })
  
  payer_plan_period <- reactive({
    req(subject_id() )
    omop_table_payer_plan_period(table_map, db_connection, subject_id)
  })
  
  procedure_occurrence <- reactive({
    req(subject_id() )
    omop_table_procedure_occurrence(table_map, db_connection, subject_id)
  })
  
  specimen <- reactive({
    req(subject_id() )
    omop_table_specimen(table_map, db_connection, subject_id)
  })
  
  visit_occurrence <- reactive({
    omop_table_visit_occurrence(table_map, db_connection, subject_id)
  })
  
  ## Render the tibbles as datatables -----
  output$condition_era_dt <- renderDataTable({
    req(condition_era())
    condition_era() %>% 
      reviewr_datatable()
  })
  
  output$condition_occurrence_dt <- renderDataTable({
    req(condition_occurrence() )
    condition_occurrence() %>% 
      reviewr_datatable()
  })
  
  output$death_dt <- renderDataTable({
    req(death() )
    death() %>% 
      reviewr_datatable()
  })
  
  output$device_exposure_dt <- renderDataTable({
    req(device_exposure() )
    device_exposure() %>% 
      reviewr_datatable()
  })
  
  output$dose_era_dt <- renderDataTable({
    req(dose_era() )
    dose_era() %>% 
      reviewr_datatable()
  })
  
  output$drug_era_dt <- renderDataTable({
    req(drug_era() )
    drug_era() %>% 
      reviewr_datatable()
  })
  
  output$drug_exposure_dt <- renderDataTable({
    req(drug_exposure() )
    drug_exposure() %>% 
      reviewr_datatable()
  })
  
  output$measurement_dt <- renderDataTable({
    req(measurement() )
    measurement() %>% 
      reviewr_datatable()
  })
  
  output$note_dt <- renderDataTable({
    req(note() )
    note() %>% 
      reviewr_datatable()
  })
  
  output$observation_dt <- renderDataTable({
    req(observation() )
    observation() %>% 
      reviewr_datatable()
  })
  
  output$observation_period_dt <- renderDataTable({
    req(observation_period() )
    observation_period() %>% 
      reviewr_datatable()
  })
  
  output$payer_plan_period_dt <- renderDataTable({
    req(payer_plan_period() )
    payer_plan_period() %>% 
      reviewr_datatable()
  })
  
  output$procedure_occurrence_dt <- renderDataTable({
    req(procedure_occurrence() )
    procedure_occurrence() %>% 
      reviewr_datatable()
  })
  
  output$specimen_dt <- renderDataTable({
    req(specimen() )
    specimen() %>% 
      reviewr_datatable()
  })
  
  output$visit_occurrence_dt <- renderDataTable({
    req(visit_occurrence() )
    visit_occurrence() %>% 
      reviewr_datatable()
  })
}
