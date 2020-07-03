#' Patient Chart Module
#'
#' This module will render a pre coordinated table with multiple tabsets containing patient information. If configured, a chart abstraction UI will also be presented.
#'
#' @param id The namespace id for the UI output
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_patient_chart_module
#' 
#' @keywords internal
#' @export
#' @import shiny 
#' @importFrom shinycssloaders withSpinner
#' 

## OMOP ----

omop_chart_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 12px} ")
      ),
    tabsetPanel(id = 'patient_chart',type = 'tabs',
                tabPanel(title = 'Condition Era', DT::dataTableOutput(ns('omop_condition_era_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Condition Occurrence', DT::dataTableOutput(ns('omop_condition_occurrence_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Death', DT::dataTableOutput(ns('omop_death_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Device Exposure', DT::dataTableOutput(ns('omop_device_exposure_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Dose Era', DT::dataTableOutput(ns('omop_dose_era_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Drug Era', DT::dataTableOutput(ns('omop_drug_era_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Drug Exposure', DT::dataTableOutput(ns('omop_drug_exposure_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Measurement', DT::dataTableOutput(ns('omop_measurement_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Note', DT::dataTableOutput(ns('omop_note_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Observation', DT::dataTableOutput(ns('omop_observation_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Observation Period', DT::dataTableOutput(ns('omop_observation_period_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Payer Plan Period', DT::dataTableOutput(ns('omop_payer_plan_period_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Procedure Occurrence', DT::dataTableOutput(ns('omop_procedure_occurrence_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Specimen', DT::dataTableOutput(ns('omop_specimen_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Visit Occurrence', DT::dataTableOutput(ns('omop_visit_occurrence_dt')) %>% withSpinner(type = 6, proxy.height = '760px'))
                )
  )
}

## OMOP Chart Review Logic ----

#' @rdname mod_patient_chart_module
#' @param table_map tibble containing a the cdm that most closely matches the user's database and a map of standard tables to user tables
#' @param db_connection Connection info received from the database setup module
#' @param subject_id The selected subject
#' @keywords internal
#' @export


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
  output$omop_condition_era_dt <- DT::renderDataTable({
    req(omop_condition_era())
    omop_condition_era() %>% 
      reviewr_datatable()
  })
  
  output$omop_condition_occurrence_dt <- DT::renderDataTable({
    req(omop_condition_occurrence() )
    omop_condition_occurrence() %>% 
      reviewr_datatable()
  })
  
  output$omop_death_dt <- DT::renderDataTable({
    req(omop_death() )
    omop_death() %>% 
      reviewr_datatable()
  })
  
  output$omop_device_exposure_dt <- DT::renderDataTable({
    req(omop_device_exposure() )
    omop_device_exposure() %>% 
      reviewr_datatable()
  })
  
  output$omop_dose_era_dt <- DT::renderDataTable({
    req(omop_dose_era() )
    omop_dose_era() %>% 
      reviewr_datatable()
  })
  
  output$omop_drug_era_dt <- DT::renderDataTable({
    req(omop_drug_era() )
    omop_drug_era() %>% 
      reviewr_datatable()
  })
  
  output$omop_drug_exposure_dt <- DT::renderDataTable({
    req(omop_drug_exposure() )
    omop_drug_exposure() %>% 
      reviewr_datatable()
  })
  
  output$omop_measurement_dt <- DT::renderDataTable({
    req(omop_measurement() )
    omop_measurement() %>% 
      reviewr_datatable()
  })
  
  output$omop_note_dt <- DT::renderDataTable({
    req(omop_note() )
    omop_note() %>% 
      reviewr_datatable()
  })
  
  output$omop_observation_dt <- DT::renderDataTable({
    req(omop_observation() )
    omop_observation() %>% 
      reviewr_datatable()
  })
  
  output$omop_observation_period_dt <- DT::renderDataTable({
    req(omop_observation_period() )
    omop_observation_period() %>% 
      reviewr_datatable()
  })
  
  output$omop_payer_plan_period_dt <- DT::renderDataTable({
    req(omop_payer_plan_period() )
    omop_payer_plan_period() %>% 
      reviewr_datatable()
  })
  
  output$omop_procedure_occurrence_dt <- DT::renderDataTable({
    req(omop_procedure_occurrence() )
    omop_procedure_occurrence() %>% 
      reviewr_datatable()
  })
  
  output$omop_specimen_dt <- DT::renderDataTable({
    req(omop_specimen() )
    omop_specimen() %>% 
      reviewr_datatable()
  })
  
  output$omop_visit_occurrence_dt <- DT::renderDataTable({
    req(omop_visit_occurrence() )
    omop_visit_occurrence() %>% 
      reviewr_datatable()
  })
}



## MIMIC ----

#' @rdname mod_patient_chart_module
#' @param id The namespace id for the UI output
#' @export
#' @keywords internal
#' 
mimic_chart_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 12px} ")
    ),
    tabsetPanel(id = 'patient_chart',type = 'tabs',
                tabPanel(title = 'Admissions', DT::dataTableOutput(ns('mimic_admissions_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Callout', DT::dataTableOutput(ns('mimic_callout_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Chart Events', DT::dataTableOutput(ns('mimic_chart_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'CPT Events', DT::dataTableOutput(ns('mimic_cpt_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Diagnoses ICD', DT::dataTableOutput(ns('mimic_diagnoses_icd_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'DRG Codes', DT::dataTableOutput(ns('mimic_drg_codes_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'ICU Stays', DT::dataTableOutput(ns('mimic_icu_stays_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Lab Events', DT::dataTableOutput(ns('mimic_lab_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Microbiology Events', DT::dataTableOutput(ns('mimic_microbiology_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Note', DT::dataTableOutput(ns('mimic_note_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Prescriptions Events MV', DT::dataTableOutput(ns('mimic_prescriptions_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Procedure Events MV', DT::dataTableOutput(ns('mimic_procedure_events_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Procedures ICD', DT::dataTableOutput(ns('mimic_procedures_icd_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Services', DT::dataTableOutput(ns('mimic_services_dt')) %>% withSpinner(type = 6, proxy.height = '760px')),
                tabPanel(title = 'Transfers', DT::dataTableOutput(ns('mimic_transfers_dt')) %>% withSpinner(type = 6, proxy.height = '760px'))
                )
    )
  }
## MIMIC Chart Review Logic ----

#' @rdname mod_patient_chart_module
#' @param table_map tibble containing a the cdm that most closely matches the user's database and a map of standard tables to user tables
#' @param db_connection Connection info received from the database setup module
#' @param subject_id The selected subject
#' @export
#' @keywords internal
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
  output$mimic_admissions_dt <- DT::renderDataTable({
    req(mimic_admissions())
    mimic_admissions() %>% 
      reviewr_datatable()
  })
  
  output$mimic_callout_dt <- DT::renderDataTable({
    req(mimic_callout() )
    mimic_callout() %>% 
      reviewr_datatable()
  })
  
  output$mimic_chart_events_dt <- DT::renderDataTable({
    req(mimic_chart_events() )
    mimic_chart_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_cpt_events_dt <- DT::renderDataTable({
    req(mimic_cpt_events() )
    mimic_cpt_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_diagnoses_icd_dt <- DT::renderDataTable({
    req(mimic_diagnoses_icd() )
    mimic_diagnoses_icd() %>% 
      reviewr_datatable()
  })
  
  output$mimic_drg_codes_dt <- DT::renderDataTable({
    req(mimic_drg_codes() )
    mimic_drg_codes() %>% 
      reviewr_datatable()
  })
  
  output$mimic_icu_stays_dt <- DT::renderDataTable({
    req(mimic_icu_stays() )
    mimic_icu_stays() %>% 
      reviewr_datatable()
  })
  
  output$mimic_lab_events_dt <- DT::renderDataTable({
    req(mimic_lab_events() )
    mimic_lab_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_microbiology_events_dt <- DT::renderDataTable({
    req(mimic_microbiology_events() )
    mimic_microbiology_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_note_dt <- DT::renderDataTable({
    req(mimic_note() )
    mimic_note() %>% 
      reviewr_datatable()
  })
  
  output$mimic_prescriptions_events_dt <- DT::renderDataTable({
    req(mimic_prescriptions_events() )
    mimic_prescriptions_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_procedure_events_dt <- DT::renderDataTable({
    req(mimic_procedure_events() )
    mimic_procedure_events() %>% 
      reviewr_datatable()
  })
  
  output$mimic_procedures_icd_dt <- DT::renderDataTable({
    req(mimic_procedures_icd() )
    mimic_procedures_icd() %>% 
      reviewr_datatable()
  })
  
  output$mimic_services_dt <- DT::renderDataTable({
    req(mimic_services() )
    mimic_services() %>% 
      reviewr_datatable()
  })
  
  output$mimic_transfers_dt <- DT::renderDataTable({
    req(mimic_transfers() )
    mimic_transfers() %>% 
      reviewr_datatable()
  })
}

## Chart Review UI ----
#' @rdname mod_patient_chart_module
#' @export
#' @keywords internal

chart_review_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('chart_review_ui'))
  )
}

#' @rdname mod_patient_chart_module
#' @param abstraction_vars a list containing data abstraction variables (REDCap currently: url, api token, connection, connect button press)
#' @param table_map tibble containing a the cdm that most closely matches the user's database and a map of standard tables to user tables
#' @param instrument_selection Which REDCap instrument in the project to use
#' @export
#' @keywords internal

chart_review_ui_logic <- function(input, output, session, abstraction_vars, table_map, instrument_selection) {
  ns <- session$ns
  
  ## Change layout based on presence or absence of abstraction connection info
  output$chart_review_ui <- renderUI({ 
    req(table_map$table_map())
    ## Revisit -- Valid RC Connection info is now responsible for swapping btwn abstraction/not. 
    tryCatch({
      abstraction_vars$rc_con() ## On ReviewR load, rc_con() throws silent error. Proceed to 'error function' (no abstraction). Do not collect $200
      if(abstraction_vars$rc_con() == '') { ## No Abstraction (after initial RC connect/disconnect cycle)
        box(width = '100%',
            status = 'primary',
            ## Select patient chart ui based on data model
            if(table_map$table_map()$data_model == 'omop') {
              omop_chart_review_ui('chart_review')
            } else if (table_map$table_map()$data_model == 'mimic3') {
              mimic_chart_review_ui('chart_review')
            } else {return(NULL)}
        ) 
      } else { ## Abstraction ----
        fluidRow(
          column(
            width = 9,
            box(width = '100%',
                status = 'primary',
                ## Select patient chart ui based on data model
                if(table_map$table_map()$data_model == 'omop') {
                  omop_chart_review_ui('chart_review')
                } else if (table_map$table_map()$data_model == 'mimic3') {
                  mimic_chart_review_ui('chart_review')
                } else {return(NULL)}
            )
          ),
          column(
            width = 3,
            box(
              title = instrument_selection$rc_instrument_selection(),
              width = '100%',
              status = 'danger',
              redcap_instrument_ui('chart_review_abstraction'),
              ## CSS to scroll the abstraction instrument, if necessary
              tags$head(
                tags$style("#chart_review_abstraction-redcap_form{color:black; font-size:12px; overflow-y:scroll; max-height: 598px;}")
              )
            ),
            box(
              title = 'Save Form',
              width = '100&',
              status = 'danger',
              instrument_complete_ui('chart_review_upload')
            )
          )
        )
      }
    },
    error=function(error_cond) { ## No Abstraction
      box(width = '100%',
          status = 'primary',
          ## Select patient chart ui based on data model
          if(table_map$table_map()$data_model == 'omop') {
            omop_chart_review_ui('chart_review')
          } else if (table_map$table_map()$data_model == 'mimic3') {
            mimic_chart_review_ui('chart_review')
          } else {return(NULL)}
      )
    })
  })
}
