source('lib/omop_data_helpers.R')
source('lib/mimic_data_helpers.R')
source('lib/project_helpers.R')
source('lib/ui_helpers.r')

get_review_table_names <- function(data_model) {
  data_model = tolower(data_model)
  if (data_model == "omop") {
    omop_get_review_table_names()
  }
  else if (data_model == "mimic") {
    mimic_get_review_table_names()
  }
}