append_data_panel <- function(tab_id, data_table_id) {
  print("Appending")
  appendTab(inputId = "data_tabs", tabPanel(tab_id, withSpinner(DT::dataTableOutput(data_table_id))))
}

create_data_panel <- function(tab_id, data_table_id) {
  tabPanel(tab_id, withSpinner(DT::dataTableOutput(data_table_id)))
}