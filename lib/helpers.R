source('lib/omop_data_helpers.R')
#source('lib/mimic_data_helpers.R')
#source('lib/project_helpers.R')
#source('lib/ui_helpers.r')

get_review_table_names <- function(data_model) {
  data_model = tolower(data_model)
  if (data_model == "omop") {
    omop_get_review_table_names()
  }
  else if (data_model == "mimic") {
    mimic_get_review_table_names()
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

# Primary initialization function for ReviewR
initialize <- function(db_engine,...) {
  params <- list(...)
  db_engine = tolower(db_engine)
  connection <- NULL
  if (db_engine == 'postgres' | db_engine == 'postgresql' | db_engine == 'pgsql') {
    check.packages("RPostgreSQL")
    connection = dbConnect(RPostgreSQL::PostgreSQL(),
                           user=params$user, password=params$password, dbname=params$database,
                           host=params$host, port=params$port)
  }
  else if (db_engine == 'bigquery') {
    check.packages("bigrquery")
    connection = dbConnect(bigrquery::bigquery(),
                           project=params$project, dataset=params$dataset)
  }
  connection
}