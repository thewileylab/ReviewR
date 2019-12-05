#' img_uir
#' This function will generate a uri for a locally hosted image so that it will display inline in a DT
#' @param img_location A fully qualified path to the desired image file
#' @param img_height Height, in pixels, for the image to be displayed.
#'
#' @return image uri as a string. for display in DT, ensure that escape = F is set in DT
#'
#' @examples img_uri('www/status_unverified.png', 15)
img_uri <- function(img_location, img_height) { 
  sprintf(paste('<img src="%s" height="',y,'px"/>'), knitr::image_uri(x)) 
  }
