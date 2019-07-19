library(httr)
library(bigrquery)
if (interactive()) {
  # For use locally, ensure the app is running on port 8100
  APP_URL <- "http://localhost:8100/"
} else {
  # deployed URL
  APP_URL <- "http://localhost:3838/ReviewR/"
}

# Note that secret is not really secret, and it's fine to include inline
  app <- oauth_app(appname = "ReviewR_Google_Auth",
                   key = "241672600185-vbmn82ge65skhe9gd9uihkah51eir82l.apps.googleusercontent.com", ## coursera-development-admin project, currently
                   secret = "H_i96QZYXzkgKxIcCWt21VfY",
                   redirect_uri = APP_URL
                   )

# Define Google as the endpoint (this one is canned)
api <- oauth_endpoints("google")

# Always request the minimal scope needed. Here, we are requesting access to BigQuery
scope <- "https://www.googleapis.com/auth/bigquery"

# Craft some URL's
url <- oauth2.0_authorize_url(api, app, scope = scope)
redirect <- sprintf("location.replace(\"%s\");", url)

#url_home <- 'http://localhost:8100/'
redirect_home <- sprintf("window.location.replace(\"%s\");", APP_URL)
