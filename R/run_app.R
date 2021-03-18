#' Run the ReviewR Application
#'
#' @param ... A list of options to pass to golem
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' 
#' @return No return value, called to start the ReviewR Shiny Application!
#' 
#  __________            .__              __________ 
#  \______   \ _______  _|__| ______  _  _\______   \
#   |       _// __ \  \/ /  |/ __ \ \/ \/ /|       _/
#   |    |   \  ___/\   /|  \  ___/\     / |    |   \
#   |____|_  /\___  >\_/ |__|\___  >\/\_/  |____|_  /
#          \/     \/             \/               \/ 
#                                       by WileyLab
#
# Making manual record review fun since 2019!
#

#
# authors:  Laura Wiley, Luke Rasmussen, David Mayer
#
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, 
                   server = app_server,
                   options = list(port=1410,launch.browser=T)), 
    golem_opts = list(...)
  )
}

#' ReviewR: A light-weight, portable tool for reviewing individual patient records
#'
#' ReviewR is a portable Shiny tool to help you explore data across different 
#' data models. Within ReviewR, you can browse patient data stored in either 
#' the OMOP or MIMIC-III data model. In addition to viewing patient data, you 
#' may also connect to a REDCap project to perform a chart review.
#' 
#'
#' @docType package
#' @name ReviewR
NULL
#> NULL
