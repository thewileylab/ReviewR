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
                tabPanel(title = 'Condition Era', dataTableOutput(ns('omop_condition_era_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Condition Occurrence', dataTableOutput(ns('omop_condition_occurrence_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Death', dataTableOutput(ns('omop_death_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Device Exposure', dataTableOutput(ns('omop_device_exposure_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Dose Era', dataTableOutput(ns('omop_dose_era_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Drug Era', dataTableOutput(ns('omop_drug_era_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Drug Exposure', dataTableOutput(ns('omop_drug_exposure_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Measurement', dataTableOutput(ns('omop_measurement_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Note', dataTableOutput(ns('omop_note_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Observation', dataTableOutput(ns('omop_observation_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Observation Period', dataTableOutput(ns('omop_observation_period_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Payer Plan Period', dataTableOutput(ns('omop_payer_plan_period_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Procedure Occurrence', dataTableOutput(ns('omop_procedure_occurrence_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Specimen', dataTableOutput(ns('omop_specimen_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Visit Occurrence', dataTableOutput(ns('omop_visit_occurrence_dt')) %>% withSpinner(type = 6, proxy.height = '760px'))
                )
  )
}

omop_chart_review_logic <- function(input, output, session, table_map, db_connection, subject_id) {
  ns <- session$ns
  ## Define Reactive OMOP tibbles -----
  ## Update every time the subject_id() variable changes
  omop_condition_era <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop'){
    omop_table_condition_era(table_map, db_connection, subject_id)
    } else { return(NULL) }
    })
  
  omop_condition_occurrence <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_condition_occurrence(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_death <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_death(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_device_exposure <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_device_exposure(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_dose_era <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_dose_era(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_drug_era <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_drug_era(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_drug_exposure <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_drug_exposure(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_measurement <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_measurement(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_note <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_note(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_observation <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_observation(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_observation_period <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_observation_period(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_payer_plan_period <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_payer_plan_period(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_procedure_occurrence <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_procedure_occurrence(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_specimen <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_specimen(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  omop_visit_occurrence <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'omop') {
    omop_table_visit_occurrence(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  ## Render OMOP tibbles as datatables -----
  output$omop_condition_era_dt <- renderDataTable({
    req(omop_condition_era())
    omop_condition_era() %>% 
      reviewr_datatable()
  })
  
  output$omop_condition_occurrence_dt <- renderDataTable({
    req(omop_condition_occurrence() )
    omop_condition_occurrence() %>% 
      reviewr_datatable()
  })
  
  output$omop_death_dt <- renderDataTable({
    req(omop_death() )
    omop_death() %>% 
      reviewr_datatable()
  })
  
  output$omop_device_exposure_dt <- renderDataTable({
    req(omop_device_exposure() )
    omop_device_exposure() %>% 
      reviewr_datatable()
  })
  
  output$omop_dose_era_dt <- renderDataTable({
    req(omop_dose_era() )
    omop_dose_era() %>% 
      reviewr_datatable()
  })
  
  output$omop_drug_era_dt <- renderDataTable({
    req(omop_drug_era() )
    omop_drug_era() %>% 
      reviewr_datatable()
  })
  
  output$omop_drug_exposure_dt <- renderDataTable({
    req(omop_drug_exposure() )
    omop_drug_exposure() %>% 
      reviewr_datatable()
  })
  
  output$omop_measurement_dt <- renderDataTable({
    req(omop_measurement() )
    omop_measurement() %>% 
      reviewr_datatable()
  })
  
  output$omop_note_dt <- renderDataTable({
    req(omop_note() )
    omop_note() %>% 
      reviewr_datatable()
  })
  
  output$omop_observation_dt <- renderDataTable({
    req(omop_observation() )
    omop_observation() %>% 
      reviewr_datatable()
  })
  
  output$omop_observation_period_dt <- renderDataTable({
    req(omop_observation_period() )
    omop_observation_period() %>% 
      reviewr_datatable()
  })
  
  output$omop_payer_plan_period_dt <- renderDataTable({
    req(omop_payer_plan_period() )
    omop_payer_plan_period() %>% 
      reviewr_datatable()
  })
  
  output$omop_procedure_occurrence_dt <- renderDataTable({
    req(omop_procedure_occurrence() )
    omop_procedure_occurrence() %>% 
      reviewr_datatable()
  })
  
  output$omop_specimen_dt <- renderDataTable({
    req(omop_specimen() )
    omop_specimen() %>% 
      reviewr_datatable()
  })
  
  output$omop_visit_occurrence_dt <- renderDataTable({
    req(omop_visit_occurrence() )
    omop_visit_occurrence() %>% 
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
                tabPanel(title = 'Admissions', dataTableOutput(ns('mimic_admissions_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Callout', dataTableOutput(ns('mimic_callout_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Chart Events', dataTableOutput(ns('mimic_chart_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'CPT Events', dataTableOutput(ns('mimic_cpt_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Diagnoses ICD', dataTableOutput(ns('mimic_diagnoses_icd_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'DRG Codes', dataTableOutput(ns('mimic_drg_codes_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'ICU Stays', dataTableOutput(ns('mimic_icu_stays_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Lab Events', dataTableOutput(ns('mimic_lab_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Microbiology Events', dataTableOutput(ns('mimic_microbiology_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Note', dataTableOutput(ns('mimic_note_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Prescriptions Events MV', dataTableOutput(ns('mimic_prescriptions_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Procedure Events MV', dataTableOutput(ns('mimic_procedure_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Procedures ICD', dataTableOutput(ns('mimic_procedures_icd_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Services', dataTableOutput(ns('mimic_services_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Transfers', dataTableOutput(ns('mimic_transfers_dt')) %>% withSpinner(type = 6, proxy.height = '760px'))
                )
    )
  }

mimic_chart_review_logic <- function(input, output, session, table_map, db_connection, subject_id) {
  ns <- session$ns
  
  ## Define Reactive MIMIC3 tibbles -----
  ## Update every time the subject_id() variable changes
  mimic_admissions <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3'){
      mimic_table_admissions(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_callout <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_callout(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_chart_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_chart_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_cpt_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_cpt_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_diagnoses_icd <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_diagnoses_icd(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_drg_codes <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_drg_codes(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_icu_stays <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_icu_stays(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_lab_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_lab_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_microbiology_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_microbiology_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_note <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_note_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_prescriptions_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_prescriptions(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_procedure_events <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_procedure_events(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_procedures_icd <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_procedures_icd(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_services <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_services(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  mimic_transfers <- reactive({
    req(table_map(), db_connection(), subject_id() )
    if(table_map()$data_model == 'mimic3') {
      mimic_table_transfers(table_map, db_connection, subject_id)
    } else { return(NULL) }
  })
  
  ## Render MIMIC3 tibbles as datatables -----
  output$mimic_admissions_dt <- renderDataTable({
    req(mimic_admissions())
    mimic_admissions() %>% 
      reviewr_datatable()
  })
  
  output$mimic_callout_dt <- renderDataTable({
    req(mimic_callout() )
    mimic_callout() %>% 
      reviewr_datatable()
  })
  
  output$mimic_chart_events_dt <- renderDataTable({
    req(mimic_chart_events() )
    mimic_chart_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_cpt_events_dt <- renderDataTable({
    req(mimic_cpt_events() )
    mimic_cpt_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_diagnoses_icd_dt <- renderDataTable({
    req(mimic_diagnoses_icd() )
    mimic_diagnoses_icd() %>% 
      reviewr_datatable()
  })
  
  output$mimic_drg_codes_dt <- renderDataTable({
    req(mimic_drg_codes() )
    mimic_drg_codes() %>% 
      reviewr_datatable()
  })
  
  output$mimic_icu_stays_dt <- renderDataTable({
    req(mimic_icu_stays() )
    mimic_icu_stays() %>% 
      reviewr_datatable()
  })
  
  output$mimic_lab_events_dt <- renderDataTable({
    req(mimic_lab_events() )
    mimic_lab_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_microbiology_events_dt <- renderDataTable({
    req(mimic_microbiology_events() )
    mimic_microbiology_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_note_dt <- renderDataTable({
    req(mimic_note() )
    mimic_note() %>% 
      reviewr_datatable()
  })
  
  output$mimic_prescriptions_events_dt <- renderDataTable({
    req(mimic_prescriptions_events() )
    mimic_prescriptions_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_procedure_events_dt <- renderDataTable({
    req(mimic_procedure_events() )
    mimic_procedure_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_procedures_icd_dt <- renderDataTable({
    req(mimic_procedures_icd() )
    mimic_procedures_icd() %>% 
      reviewr_datatable()
  })
  
  output$mimic_services_dt <- renderDataTable({
    req(mimic_services() )
    mimic_services() %>% 
      reviewr_datatable()
  })
  
  output$mimic_transfers_dt <- renderDataTable({
    req(mimic_transfers() )
    mimic_transfers() %>% 
      reviewr_datatable()
  })
}

## Other ----

