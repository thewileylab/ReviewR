FROM rocker/shiny:4.0.3

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  git-core \
  libxml2-dev \
  libcairo2-dev \
  libgit2-dev \
  libicu-dev \
  libsqlite3-dev \
  libmariadbd-dev \
  libmariadbclient-dev \
  libpq-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libsodium-dev \
  libssh2-1-dev \
  unixodbc-dev \
  make \
  pandoc \
  pandoc-citeproc \
  && install2.r --error \
    #--deps TRUE \
    bigrquery \
    config \
    dashboardthemes \
    DBI \
    dbplyr \
    dplyr \
    DT \
    gargle \
    glue \
    golem \
    httr \
    jsonlite \
    magrittr \
    pkgload \
    purrr \
    redcapAPI \
    REDCapR \
    rlang \
    RPostgres \
    RSQLite \
    shiny \
    shinycssloaders \
    shinydashboard \
    shinydashboardPlus \
    shinyjs \
    shinyWidgets \
    snakecase \
    stringr \
    tibble \
    tidyr \
      BiocManager \
  && rm -rf /tmp/downloaded_packages
