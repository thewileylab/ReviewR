source('lib/omop.R')
#source('lib/mimic.R')
#source('lib/project_helpers.R')
source('lib/ui_helpers.r')

table_case_types <- list(LOWER = "lower", UPPER = "upper", UNKNOWN = "unknown")

get_review_table_names <- function(data_model) {
  data_model = tolower(data_model)
  if (data_model == "omop") {
    omop_get_review_table_names()
  }
  else if (data_model == "mimic") {
    mimic_get_review_table_names()
  }
}

case_string <- function(string, case_type) {
  if (case_type == table_case_types$LOWER) {
    tolower(string)
  }
  else if (case_type == table_case_types$UPPER) {
    toupper(string)
  }
  else {
    string
  }
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
  db_engine = tolower(config$db_engine)
  connection <- NULL
  if (db_engine == 'postgres' | db_engine == 'postgresql' | db_engine == 'pgsql') {
    check.packages("RPostgreSQL")
    connection = dbConnect(RPostgreSQL::PostgreSQL(),
                           user=config$user, password=config$password, dbname=config$database,
                           host=config$host, port=config$port)
  }
  else if (db_engine == 'bigquery') {
    check.packages("bigrquery")
    connection = dbConnect(bigrquery::bigquery(),
                           project=config$project, dataset=config$dataset)
  }
  
  data_model = tolower(config$data_model)
  case_type <- table_case_types$UNKNOWN
  if (data_model == "omop") {
    case_type = omop_get_table_casing(connection)
  }
  else if (data_model == "mimic") {
  }
  
  list(dbi_conn = connection, case_type = case_type)
}