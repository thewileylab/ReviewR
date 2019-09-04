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
                tabPanel(title = 'Measurement', dataTableOutput(ns('measurement_dt')) %>% withSpinner(type = 6))
                
                )
  )
}

chart_review_logic <- function(input, output, session, table_map, db_connection, subject_id) {
  source('lib/reviewr_datatable.R')
  ns <- session$ns
  
  ## Define Reactive tibbles which update every time the subject_id() variable changes
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
  
  ## Render the tibbles as datatables
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
  
  
}
