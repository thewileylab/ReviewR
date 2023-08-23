#' Run Application
#' 
#' @description 
#' Start the ReviewR Application in a browser on port 1410. 
#' 
#' \preformatted{
#'  __________            .__              __________ 
#'  \______   \ _______  _|__| ______  _  _\______   \
#'   |       _// __ \  \/ /  |/ __ \ \/ \/ /|       _/
#'   |    |   \  ___/\   /|  \  ___/\     / |    |   \
#'   |____|_  /\___  >\_/ |__|\___  >\/\_/  |____|_  /
#'          \/     \/             \/               \/ 
#'                                       by WileyLab
#'
#' Making manual record review fun since 2019!
#' 
#' authors:  Laura Wiley, Luke Rasmussen, David Mayer}
#'
#' @param ... A list of options to pass to the app including:
#' \itemize{
#' \item{secrets_json: A string, containing a file path to a Google OAuth 2.0 
#' Client ID JSON}
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' 
#' @return No return value, called to start the ReviewR Shiny Application!
#' 
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, 
                   server = app_server,
                   options = list(port=1410,launch.browser=T)), 
    golem_opts = list(...)
  )
}
