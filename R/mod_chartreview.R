# UI ----
#' Chart Review
#'
#' @param id The namespace id for the UI output
#'
#' @rdname mod_chartreview
#' 
#' @keywords internal
#' @export
#'
chartreview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns('debug'), label = "I'm alive!"),
    uiOutput(ns('view_review')),
    uiOutput(ns('patient_chart'))
  )
}

# Server ----
#' Chart Review
#'
#' @param id The namespace id for the UI output
#' @param database_vars Database variables returned from user selected database setup module
#' @param datamodel_vars Datamodel variables returned from mod_datamodel_setup
#' @param abstract_vars Abstraction variables returned from user selected abstraction module
#' @param navigation_vars Navigation variables returned from mod_navigation
#'
#' @rdname mod_chartreview
#' 
#' @keywords internal
#' @export
#' @importFrom magrittr %>% 
#' @importFrom purrr map map2 iwalk imap
#' @importFrom snakecase to_title_case
mod_chartreview_server <- function(id, database_vars, datamodel_vars, abstract_vars, navigation_vars) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      # Debug ----
      observeEvent(input$debug, {
        browser()
        })
      
      # Chart Review Vars ----
      chart_review_vars <- reactiveValues(
        table_args = NULL,
        patient_tables = NULL,
        tabset_panel = NULL
        )
      
      # Render View/Review ----
      ## Determine if View or Review interface is needed based on presence or absence of 
      ## a configured Abstraction Module. 
      ## Both View and Review require a connected database.
      view_review <- reactive({
        req(database_vars()$is_connected == 'yes')
        ## Abstraction Module configured 
        if(database_vars()$is_connected == 'yes' & abstract_vars()$is_configured == 'yes') {
          tagList(
            column(width = 9,
                   box(width = '100%',
                       status = 'primary'
                       ## Patient Chart
                       )
                   ),
            column(width = 3,
                   ## Abstraction Instrument
                   abstraction_instrument_ui('abs-selector')
                   )
            )
          ## Abstraction Module NOT configured
          } else {
            tagList(
              box(width = '100%',
                  status = 'primary'
                  ## Patient Chart
                  )
              )
            }
        })
      
      # Patient Chart ----
      ## Big Thanks: https://tbradley1013.github.io/2018/08/10/create-a-dynamic-number-of-ui-elements-in-shiny-with-purrr/
      observeEvent(navigation_vars$selected_subject_id, {
        req(navigation_vars$selected_subject_id)
        ## Create Arguments for database table functions
        chart_review_vars$table_args <- list(table_map = datamodel_vars$table_map,
                                             db_connection = database_vars()$db_con,
                                             subject_id = navigation_vars$selected_subject_id
                                             )
        ## Create Reactives containing tables for the detected datamodel
        chart_review_vars$patient_tables <- datamodel_vars$subject_tables %>%
          mutate(tab_name = snakecase::to_title_case(.data$table_name),
                 reactive = map(.data$function_name,
                                ~reactive({
                                  rlang::exec(.x, !!!chart_review_vars$table_args)
                                  })
                                )
                 ) %>% 
          pull(reactive)
        
        ## Create a DT Outputs
        purrr::iwalk(chart_review_vars$patient_tables, ~{
          output_name <- glue::glue('dt_{.y}')
          output[[output_name]] <- DT::renderDataTable(rlang::exec(.x) %>% reviewr_datatable())
          })
        
        })
      
      ## Test ---- 
      output$patient_chart <- renderUI({
        req(chart_review_vars$patient_tables)
        # browser()
        
        chart_review_vars$tabset_panels <- datamodel_vars$subject_tables %>% 
          mutate(tab_name = snakecase::to_title_case(.data$table_name),
                 dt_list = imap(chart_review_vars$patient_tables, ~{
                   tagList(
                     DT::DTOutput(outputId = ns(glue::glue('dt_{.y}') )) %>% withSpinner(type = 6, proxy.height = '760px')
                   )
                 }),
                 tab_panels = purrr::map2(
                   .data$tab_name, 
                   .data$dt_list,
                   ~tabPanel(title = .x, .y)
                 )
          ) %>% 
          pull(tab_panels)
        do.call(tabsetPanel, chart_review_vars$tabset_panels)
        })
      
      
      # Output UI ----
      output$view_review <- renderUI({ view_review() })
    }
  )
}