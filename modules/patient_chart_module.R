#
# Patient Chart Module
#
source('lib/reviewr_datatable.R',keep.source = F)

## OMOP ----

omop_chart_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 12px} ")
      ),
    tabsetPanel(id = 'patient_chart',type = 'tabs',
                tabPanel(title = 'Condition Era', dataTableOutput(ns('condition_era_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Condition Occurrence', dataTableOutput(ns('condition_occurrence_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Death', dataTableOutput(ns('death_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Device Exposure', dataTableOutput(ns('device_exposure_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Dose Era', dataTableOutput(ns('dose_era_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Drug Era', dataTableOutput(ns('drug_era_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Drug Exposure', dataTableOutput(ns('drug_exposure_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Measurement', dataTableOutput(ns('measurement_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Note', dataTableOutput(ns('note_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Observation', dataTableOutput(ns('observation_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Observation Period', dataTableOutput(ns('observation_period_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Payer Plan Period', dataTableOutput(ns('payer_plan_period_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Procedure Occurrence', dataTableOutput(ns('procedure_occurrence_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Specimen', dataTableOutput(ns('specimen_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Visit Occurrence', dataTableOutput(ns('visit_occurrence_dt')) %>% withSpinner(type = 6, proxy.height = '760px'))
                )
  )
}

omop_chart_review_logic <- function(input, output, session, table_map, db_connection, subject_id) {
  ns <- session$ns
  ## Define Reactive OMOP tibbles -----
  ## Update every time the subject_id() variable changes
  condition_era <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop'){
    omop_table_condition_era(table_map, db_connection, subject_id)
    } else { return(NULL) }
    })
  
  condition_occurrence <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_condition_occurrence(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  death <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_death(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  device_exposure <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_device_exposure(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  dose_era <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_dose_era(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  drug_era <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_drug_era(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  drug_exposure <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_drug_exposure(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  measurement <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_measurement(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  note <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_note(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  observation <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_observation(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  observation_period <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_observation_period(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  payer_plan_period <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_payer_plan_period(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  procedure_occurrence <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_procedure_occurrence(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  specimen <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_specimen(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  visit_occurrence <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_visit_occurrence(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  ## Render OMOP tibbles as datatables -----
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

## MIMIC ----

mimic_chart_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 12px} ")
    ),
    tabsetPanel(id = 'patient_chart',type = 'tabs',
                tabPanel(title = 'Admissions', dataTableOutput(ns('admissions_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Callout', dataTableOutput(ns('callout_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Chart Events', dataTableOutput(ns('chart_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'CPT Events', dataTableOutput(ns('cpt_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Diagnoses ICD', dataTableOutput(ns('diagnoses_icd_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'DRG Codes', dataTableOutput(ns('drg_codes_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'ICU Stays', dataTableOutput(ns('icu_stays_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Lab Events', dataTableOutput(ns('lab_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Microbiology Events', dataTableOutput(ns('microbiology_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Note', dataTableOutput(ns('note_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Prescriptions Events MV', dataTableOutput(ns('prescriptions_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Procedure Events MV', dataTableOutput(ns('procedure_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Procedures ICD', dataTableOutput(ns('procedures_icd_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Services', dataTableOutput(ns('services_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Transfers', dataTableOutput(ns('transfers_dt')) %>% withSpinner(type = 6, proxy.height = '760px'))
                )
    )
  }

mimic_chart_review_logic <- function(input, output, session, table_map, db_connection, subject_id) {
  ns <- session$ns
  
  ## Define Reactive MIMIC3 tibbles -----
  ## Update every time the subject_id() variable changes
  admissions <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3'){
      mimic_table_admissions(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  callout <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_callout(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  chart_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_chart_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  cpt_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_cpt_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  diagnoses_icd <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_diagnoses_icd(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  drg_codes <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_drg_codes(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  icu_stays <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_icu_stays(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  lab_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_lab_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  microbiology_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_microbiology_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  note <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_note_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  prescriptions_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_prescriptions(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  procedure_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_procedure_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  procedures_icd <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_procedures_icd(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  services <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_services(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  transfers <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_transfers(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  ## Render MIMIC3 tibbles as datatables -----
  output$admissions_dt <- renderDataTable({
    req(admissions())
    admissions() %>% 
      reviewr_datatable()
  })
  
  output$callout_dt <- renderDataTable({
    req(callout() )
    callout() %>% 
      reviewr_datatable()
  })
  
  output$chart_events_dt <- renderDataTable({
    req(chart_events() )
    chart_events() %>% 
      reviewr_datatable()
  })
  
  output$cpt_events_dt <- renderDataTable({
    req(cpt_events() )
    cpt_events() %>% 
      reviewr_datatable()
  })
  
  output$diagnoses_icd_dt <- renderDataTable({
    req(diagnoses_icd() )
    diagnoses_icd() %>% 
      reviewr_datatable()
  })
  
  output$drg_codes_dt <- renderDataTable({
    req(drg_codes() )
    drg_codes() %>% 
      reviewr_datatable()
  })
  
  output$icu_stays_dt <- renderDataTable({
    req(icu_stays() )
    icu_stays() %>% 
      reviewr_datatable()
  })
  
  output$lab_events_dt <- renderDataTable({
    req(lab_events() )
    lab_events() %>% 
      reviewr_datatable()
  })
  
  output$microbiology_events_dt <- renderDataTable({
    req(microbiology_events() )
    microbiology_events() %>% 
      reviewr_datatable()
  })
  
  output$note_dt <- renderDataTable({
    req(note() )
    note() %>% 
      reviewr_datatable()
  })
  
  output$prescriptions_events_dt <- renderDataTable({
    req(prescriptions_events() )
    prescriptions_events() %>% 
      reviewr_datatable()
  })
  
  output$procedure_events_dt <- renderDataTable({
    req(procedure_events() )
    procedure_events() %>% 
      reviewr_datatable()
  })
  
  output$procedures_icd_dt <- renderDataTable({
    req(procedures_icd() )
    procedures_icd() %>% 
      reviewr_datatable()
  })
  
  output$services_dt <- renderDataTable({
    req(services() )
    services() %>% 
      reviewr_datatable()
  })
  
  output$transfers_dt <- renderDataTable({
    req(transfers() )
    transfers() %>% 
      reviewr_datatable()
  })
}

## Other ----

