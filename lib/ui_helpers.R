library(snakecase)

append_data_panel <- function(tab_id, data_table_id) {
  appendTab(inputId = "data_tabs", tabPanel(to_upper_camel_case(tab_id, sep_out = " "), DT::dataTableOutput(data_table_id)))
}

create_data_panel <- function(tab_id, data_table_id) {
  tabPanel(to_upper_camel_case(tab_id, sep_out = " "), DT::dataTableOutput(data_table_id))
}


toggleShinyDivs <- function(showDiv, hideDiv) {
  shinyjs::hide(hideDiv)
  shinyjs::show(showDiv)
}