#' Run the ReviewR Application
#'
#' @param ... A list of options to pass to golem
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
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
                   options = list(port = 8100)), 
    golem_opts = list(...)
  )
}
