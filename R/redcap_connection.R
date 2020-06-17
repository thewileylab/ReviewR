#' REDCap Connection
#' 
#' A 'safe' wrapper for redcapAPI::redcapConnection(). Will return diagnostic error codes in case incorrect URL or token are provided.
#'
#' @param url The API URL for your institution's REDCap instance
#' @param token The API token for your REDCap project
#' @keywords internal
#' @return If the URL and token are correct, return a redcapAPI connection object. Else, return diagnostic error. 
#' @export
#' @importFrom redcapAPI exportProjectInformation redcapConnection
#' @importFrom stringr str_detect regex
#'
redcap_connection <- function(url, token) {
  connection_status <- tryCatch({
    project_info <- redcapAPI::exportProjectInformation(redcapAPI::redcapConnection(url, token))
    if(nrow(project_info) == 1) {
      return(redcapAPI::redcapConnection(url,token))
      } else {
        return('redcap_unknown_error')
        }
  }, 
  error=function(error_cond) {
    if(str_detect(as.character(error_cond), pattern = regex('Could not resolve host:', ignore_case = T)) ) {
      message("Incorrect REDCap API URL. If Macbook is 2015-2020 model year, check for stuck keys. Otherwise, make sure you used the correct URL.")
      return('redcap_url_error') 
    } else if (str_detect(as.character(error_cond), pattern = regex('You do not have permissions to use the API', ignore_case = T)) ) {
      message('Incorrect API key. Please ensure you have enabled API access to your project and/or double check your API credentials.')
      return('redcap_token_error')
    } else {
      message("An unexpected server response was received, please verify that a REDCap Instance exists at the specified URL.")
      return('redcap_unknown_error')
    }
  })
  return(connection_status)
}
