chart_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(id = 'patient_chart',type = 'tabs',
                tabPanel(title = 'Condition Era', dataTableOutput(ns('condition_era_dt')) %>% withSpinner()),
                tabPanel(title = 'Condition Occurrence', dataTableOutput(ns('condition_occurrence_dt')) %>% withSpinner())
                )
  )
}

chart_review_logic <- function(input, output, session, table_map, db_connection, subject_id) {
  library(DT)
  ns <- session$ns
  
  condition_era <- reactive({
    req(subject_id() )
    condition_era_omop(table_map, db_connection, subject_id)
    })
  
  condition_occurrence <- reactive({
    req(subject_id() )
    condition_occurrence_omop(table_map, db_connection, subject_id)
  })
  
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
}
