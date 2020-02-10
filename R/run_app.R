#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
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
  options(shiny.port = 8100)
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list(...)
  )
}
