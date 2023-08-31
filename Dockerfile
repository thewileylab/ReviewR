FROM rocker/verse:4.3.1
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libpq-dev libsecret-1-dev libsodium-dev libssl-dev libv8-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.8.7")'
RUN Rscript -e 'remotes::install_version("rstudioapi",upgrade="never", version = "0.15.0")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.6.3")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.8.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.6")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.24")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.43")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.5")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.7")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.3.2.1")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.4")'
RUN Rscript -e 'remotes::install_version("here",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("gargle",upgrade="never", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version("usethis",upgrade="never", version = "2.2.2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.10")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version("gt",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("snakecase",upgrade="never", version = "0.11.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.7.6")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinydashboardPlus",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.3.1")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.4.5")'
RUN Rscript -e 'remotes::install_version("REDCapR",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("redcapAPI",upgrade="never", version = "2.7.4")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.28")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "2.3.3")'
RUN Rscript -e 'remotes::install_version("bigrquery",upgrade="never", version = "1.4.2")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 1410
CMD R -e "options('shiny.port'=1410,shiny.host='0.0.0.0');library(ReviewR);ReviewR::run_app()"
