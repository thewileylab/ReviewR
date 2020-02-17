#' REDCap Widget Map
#'
#' A dataset that maps REDCap question types and common validations
#'  to native shiny widgets through custom functions. 
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{redcap_field_type}{A REDCap Question Type}
#'   \item{redcap_field_val}{Custom REDCap Question Type Validation}
#'   \item{reviewr_redcap_widget_function}{ReviewR functon to use when mapping to native Shiny widget}
#'   ...
#' }
"redcap_widget_map"