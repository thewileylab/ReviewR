#' SQLite Module
#'
#' This module will create a SQLite database in memory to allow a user to preview the functionality of ReviewR.
#' 
#' @param id The namespace id for the UI output
#'
#' @rdname mod_SQLite_module
#' 
#' @keywords internal
#' @return A UI for connecting to the demo data
#' @export
#' @importFrom shiny NS
#'
sqlite_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('sqlite_setup_ui'))
  )
}

#' SQLite Module Server Logic
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param disconnect A press of the database disconnect button
#' 
#' @rdname mod_SQLite_module
#' 
#' @keywords internal
#' @return
#' @export
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom tibble enframe
#' @importFrom dplyr mutate 
#' @importFrom stringr str_remove
#' @importFrom purrr map2
#'
sqlite_logic <- function(input, output, session, disconnect) {
  ns <- session$ns
  
  # Create a list of rda files and table names to load into our SQLite DB
  table_list <- list.files(app_sys('extdata'),pattern = '(.rda)$',full.names = T) %>% 
    tibble::enframe(name = NULL,value = 'file_name') %>% 
    dplyr::mutate(table = stringr::str_remove(basename(.data$file_name), '.rda')
           )
  
  sqlite_setup_ui <- reactive({
    tagList(
      div(
        'Connect to a demonstration SQLite database containing synpuf data.'
        ),
      br(),
      actionButton(
        inputId = ns('connect'),
        label = 'Connect',
        icon = icon(name = 'database')
      )
    )
  })
  
  # Create a SQLite DB to hold our data
  db_connection <- eventReactive(input$connect, {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  })
  
  observeEvent(input$connect, {
    message('creating sqlite db')
    # Load each rda file in sequence, and write the table to SQLite
    # for(i in 1:nrow(table_list)) {
    #   load(table_list$file_name[i])
    #   DBI::dbWriteTable(conn = db_connection(), name = table_list$table[i],value = table,overwrite = T)
    #   rm(table)
    #   }
    map2(.x = table_list$file_name, 
         .y = table_list$table, 
         ~ write_sqlite_table(table_location = .x, 
                              table_name = .y,
                              db_connection = db_connection()
                              )
         )
    })
  
  observeEvent(disconnect(), {
    DBI::dbDisconnect(db_connection() )
  })
  connect_press <- reactive({ input$connect })
  
  output$sqlite_setup_ui <- renderUI({ sqlite_setup_ui() })
  return(
    list(
      'db_connection' = db_connection,
      'connect_press' = connect_press
    )
  )
}
