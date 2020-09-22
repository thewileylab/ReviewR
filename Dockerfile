FROM rocker/r-ver:4.0.2
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libssh2-1-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "1.5")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "0.4.7")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.4")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.0.3")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.30")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.1")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.3")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("snakecase",upgrade="never", version = "0.11.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.3")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.7.9")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.15")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "1.4.4")'
RUN Rscript -e 'remotes::install_version("dashboardthemes",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_github("thewileylab/shinyPostgreSQL@7b25a0fdd7f71f33ed2009360208796ea155a501")'
RUN Rscript -e 'remotes::install_github("thewileylab/shinyREDCap@2c543d6b750a6ea4f14cef97c5c390e55889d873")'
RUN Rscript -e 'remotes::install_github("thewileylab/shinyBigQuery@acbefd1146ad0696d98cbc7ffd1ab36dc1b13fdd")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@851aef01cf591b50d8bb500e09b2256698e7f349")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 1410
CMD R -e "options('shiny.port'=1410,shiny.host='0.0.0.0');ReviewR::run_app()"
