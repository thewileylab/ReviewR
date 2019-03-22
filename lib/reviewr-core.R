source('lib/omop.R')
#source('lib/mimic.R')
#source('lib/project_helpers.R')
source('lib/ui_helpers.r')
source('lib/table_map.R')
source('lib/render_redcap.R')

library(tidyverse)

# Create widget map - used by REDCap initialization process
redcap_field_type <- c("text","text","text","dropdown","truefalse","yesno","radio","checkbox","notes")
redcap_field_val <- c(NA,"date_mdy","integer",NA,NA,NA,NA,NA,NA)
reviewr_redcap_widget_function <- c("reviewr_text","reviewr_date","reviewr_integer","reviewr_dropdown","reviewr_truefalse","reviewr_yesno","reviewr_radio","reviewr_checkbox","reviewr_notes")
redcap_widget_map <- tibble(redcap_field_type, redcap_field_val, reviewr_redcap_widget_function)


load_reviewr_config <- function() {
  tryCatch({
    read.config(file="config.yml")
  },
  error=function(e) { print(e); NULL },
  warning=function(w) { print(w); NULL })
}

get_review_table_names <- function(data_model) {
  data_model = tolower(data_model)
  if (data_model == "omop") {
    omop_get_review_table_names()
  }
  else if (data_model == "mimic") {
    mimic_get_review_table_names()
  }
}

get_render_data_tables <- function(data_model) {
  if (tolower(data_model) == "mimic") {
    render_data_tables = mimic_render_data_tables
  }
  else if (tolower(data_model) == "omop") {
    render_data_tables = omop_render_data_tables
  }
  render_data_tables
}

get_all_people_for_list <- function(data_model) {
  if (tolower(data_model) == "mimic") {
    all_people_for_list = mimic_get_all_people_for_list
  }
  else if (tolower(data_model) == "omop") {
    all_people_for_list = omop_get_all_people_for_list
  }
  all_people_for_list
}

# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
# Courtesy of smithdanielle - https://gist.github.com/smithdanielle/9913897
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, library, character.only = TRUE)
}

# Primary initialization function for ReviewR.  This will create the database connection, based
# on the configuration information sent in.
initialize <- function(config) {
  db_type = tolower(config$db_type)
  connection <- NULL
  if (db_type == 'postgres' | db_type == 'postgresql' | db_type == 'pgsql') {
    check.packages("RPostgreSQL")
    connection = dbConnect(RPostgreSQL::PostgreSQL(),
                           user=config$user, password=config$password, dbname=config$database,
                           host=config$host, port=config$port)
    
    # Clear the credentials once we have made a connection
    config$user = NULL
    config$password = NULL
  }
  else if (db_type == 'bigquery') {
    check.packages("bigrquery")
    connection = dbConnect(bigrquery::bigquery(),
                           project=config$project, dataset=config$dataset)
  }
  
  config$connection = connection
  config$table_map = table_map(db_type, config$data_model, config$connection)
  config
}

# config
#   $redcap_api_url
#   $redcap_api_token
#
# Modifications
#   $redcap_connection
initialize_redcap <- function(config) {
  connection <- redcapConnection(url = config$redcap_api_url, token = config$redcap_api_token)
  # Extract Instrument metadata
  instrument <- exportMetaData(connection)
  record_id_field <- instrument %>% slice(1)
  
  instrument %<>% 
    slice(-1) %>%   # We drop the first row, as it most likely is the auto-increment field used in REDCap
    filter(!field_type %in% c('slider','calc','descriptive'))
  # Further filter to give the list of text fields - this is our candidate list of patient_id fields
  # to choose from
  text_fields <- instrument %>%
    filter(field_type == 'text') %>%
    select(field_label, field_name)

  # Join REDCap Instrument with redcap_widget_map to get our UI widgets
  instrument %<>% 
    # If some information is not defined within REDCap, it will convert those to logical types by default.  We are
    # assuming that they will be all character values, so we need to perform explicit casting to continue with that
    # assumption.
    mutate_if(is.logical, as.character) %>%
    left_join(redcap_widget_map, by = c("field_type" = "redcap_field_type", "text_validation_type_or_show_slider_number" = "redcap_field_val")) %>% 
    mutate(reviewr_inputID = paste0(field_name,"_", reviewr_redcap_widget_function)) %>% 
    rownames_to_column()
  
  # Retrieve all of the REDCap data saved so far.  This will be cached locally (note that it's one time, as we're assuming single reviewer
  # at a time).
  redcap_records <- exportRecords(connection)
  
  # Determine the next auto-incrementing record ID, since we are expected to provide it for the API
  next_record_id <<- max(as.numeric(redcap_records[,1])) + 1  #First column will be the auto incrementing field
  
  # Determine what variables are needed to store information       
  temp1 <- instrument %>%
    filter(is.na(reviewr_redcap_widget_function) == F) %>% 
    select(reviewr_inputID)
  form_fields <<- temp1$reviewr_inputID

  config$redcap_instrument <- instrument
  config$redcap_connection <- connection
  config$redcap_text_fields <- text_fields
  config$redcap_records <- redcap_records
  config$redcap_next_record_id <- next_record_id
  config$redcap_record_id_field <- record_id_field
  config$redcap_form_fields <- form_fields
  config
}
