chart_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(id = 'patient_chart',type = 'tabs',
                tabPanel(title = 'Condition Era', dataTableOutput(ns('condition_era_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Condition Occurrence', dataTableOutput(ns('condition_occurrence_dt')) %>% withSpinner(type = 6)),
                tabPanel(title = 'Death', dataTableOutput(ns('death_dt')) %>% withSpinner(type = 6))
                )
  )
}

chart_review_logic <- function(input, output, session, table_map, db_connection, subject_id) {
  library(DT)
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
  
  ## Render the tibbles as datatables
  output$condition_era_dt <- renderDataTable({
    req(condition_era())
    condition_era() %>% 
      datatable(extensions = list('Scroller' = NULL
      ),
      options = list(scrollX = TRUE,
                     deferRender = TRUE,
                     scrollY = '600px',
                     scroller = TRUE,
                     searchHighlight = TRUE, 
                     search = list(regex = TRUE, 
                                   caseInsensitive = TRUE)
      ),
      rownames = F, 
      selection = 'single',
      escape = F,
      filter = 'top',
      class = 'cell-border strip hover'
      )
  })
  
  output$condition_occurrence_dt <- renderDataTable({
    req(condition_occurrence() )
    condition_occurrence() %>% 
      datatable(extensions = list('Scroller' = NULL
      ),
      options = list(scrollX = TRUE,
                     deferRender = TRUE,
                     scrollY = '600px',
                     scroller = TRUE,
                     searchHighlight = TRUE, 
                     search = list(regex = TRUE, 
                                   caseInsensitive = TRUE)
      ),
      rownames = F, 
      selection = 'single',
      escape = F,
      filter = 'top',
      class = 'cell-border strip hover'
      )
  })
  
  output$death_dt <- renderDataTable({
    req(death() )
    death() %>% 
      datatable(extensions = list('Scroller' = NULL
      ),
      options = list(scrollX = TRUE,
                     deferRender = TRUE,
                     scrollY = '600px',
                     scroller = TRUE,
                     searchHighlight = TRUE, 
                     search = list(regex = TRUE, 
                                   caseInsensitive = TRUE)
      ),
      rownames = F, 
      selection = 'single',
      escape = F,
      filter = 'top',
      class = 'cell-border strip hover'
      )
  })
}
