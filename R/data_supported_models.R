#' ReviewR Supported Database Schemas
#'
#' A dataset containing data model information along with the corresponding
#' version and schema.
#' 
#' @docType data
#'
#' @format A data frame with 12 rows and 4 variables:
#' \describe{
#'   \item{file_path}{Where schema was imported from}
#'   \item{data_model}{Data model name, OMOP or MIMIC}
#'   \item{model_version}{Version of the data model}
#'   \item{data}{Nested database schemas, including included table and field mappings}
#'   ...
#' }
#' @source \url{https://github.com/OHDSI/CommonDataModel/}
#' @source \url{https://github.com/MIT-LCP/mimic-code}
"supported_models"