
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReviewR: A light-weight, portable tool for reviewing individual patient records

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1488535.svg)](https://doi.org/10.5281/zenodo.1488535)
<!-- badges: end -->

ReviewR is a portable Shiny tool to help you explore patient data across
different data models. Within ReviewR, you can browse data stored in
either the OMOP or MIMIC-III data model.

In addition to viewing patient data, you may also connect to a REDCap
project to perform a chart review.

Currently, ReviewR will connect to Google BigQuery as a database
backend, with suppoort planned for:

  - PostgresSQL
  - SQL Server
  - others supported by the dbplyr backend
    (<https://dbplyr.tidyverse.org/>)

## Installation

To install the latest development release:

``` r
# install.packages('usethis')
usethis::create_from_github("thewileylab/ReviewR")
```

## Usage

``` r
shiny::runApp('app.R', port = 8100)
```

Once the app has loaded, please complete the ‘Setup’ tab (found in the
left navigation menu) to connect to your database and optionally connect
to a REDCap project.

### View Mode

Complete the database setup. For BigQuery connections, check the console
after hitting connect to authenticate with your project. Once
successfully connected, the ‘Patient Search’ tab will appear. Select the
patient you wish to view, and you will be taken to a chart abstraction
for that patient. Navigate through patients using the previous and next
buttons or select form the dropdown on the ‘Chart Review’ tab. At any
time, you may return to the ‘Patient Search’ tab to select a different
patient.

### Review Mode

Optionally, you may connect to a REDCap project to store manual review
information. On the setup tab, enter your institution’s REDCap URL and
an API token for a REDCap project. This project should contain a single
REDCap instrument for data collection. Once connected, please select the
REDCap field that will contain your patient information as well as the
field that will contain reviewer information. Enter your name to keep
track of who has completed the review.

Now, after selecting a patient from the ‘Patient Search’ tab, your
REDCap instrument will appear next to the patient information on the
‘Chart Review’ tab. Fill in desired information and click the ‘Upload
to REDCap’ button to store your information in the REDCap project.

## Disclaimer

This is a work in progress and thus there are no guarantees of
functionality or accuracy. Use at your own risk.
