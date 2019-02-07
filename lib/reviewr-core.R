source('lib/omop.R')
#source('lib/mimic.R')
#source('lib/project_helpers.R')
source('lib/ui_helpers.r')
source('lib/table_map.R')

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
    
    # Clear the credentials once we have made a connection
    config$user = NULL
    config$password = NULL
  }
  else if (db_engine == 'bigquery') {
    check.packages("bigrquery")
    connection = dbConnect(bigrquery::bigquery(),
                           project=config$project, dataset=config$dataset)
  }
  
  config$connection = connection
  config$table_map = table_map(config$db_type, config$data_model, config$connection)
  config
}