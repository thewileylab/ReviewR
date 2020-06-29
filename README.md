
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReviewR <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1488534.svg)](https://doi.org/10.5281/zenodo.1488534)
<!-- badges: end -->

ReviewR is a portable Shiny tool to help you explore patient data across
different data models. Within ReviewR, you can browse data stored in
either the OMOP or MIMIC-III data model.

In addition to viewing patient data, you may also connect to a REDCap
project to perform a chart review.

Currently, ReviewR will connect to Google BigQuery as a database
back-end, with support planned for:

  - PostgreSQL
  - SQL Server
  - others supported by the dbplyr back-end
    (<https://dbplyr.tidyverse.org/>)

## Installation

To install the latest development release:

``` r
# install.packages('devtools')
devtools::install_github('thewileylab/ReviewR')
```

## Usage

To run the application from your local machine:

``` r
ReviewR::run_app()
```

Once the app has loaded, please complete the ‘Setup’ tab (found in the
left navigation menu) to connect to your patient database and optionally
connect to a REDCap project.

### View Mode

Complete the database setup to connect to EHR data stored in MIMIC-III
or OMOP format. For BigQuery connections, simply press “Sign in with
Google” and you will be redirected to authenticate with Google. Once
successfully connected to a patient database, the ‘Patient Search’ tab
will appear. Select the patient you wish to view, and you will be taken
to a pre-coordinated table containing EHR data for that patient.
Navigate through patients using the previous and next buttons or select
form the dropdown on the ‘Chart Review’ tab. At any time, you may return
to the ‘Patient Search’ tab to select a different patient. Patient
information globally within each tab or by a particular column.
Searching via regex is also supported.

### Review Mode

Optionally, you may connect to a REDCap project to store manual review
information. On the setup tab, enter your institution’s REDCap URL and
an API token for a REDCap project. This project may contain multiple
REDCap instruments for data collection which are selectable from the
Setup interface. Once connected, please select the REDCap field that
contains your patient information as well as the field that will contain
reviewer information. Enter your name to keep track of who has completed
the review.

Now, after selecting a patient from the ‘Patient Search’ tab, your
REDCap instrument will appear next to the patient identifier information
on the ‘Chart Review’ tab. Fill in desired information and click the
‘Upload to REDCap’ button to store your information in the REDCap
project.

## Disclaimer

This is a work in progress and thus there are no guarantees of
functionality or accuracy. Use at your own risk.

## Getting help

If you encounter bugs, errors, issues or other general unpleasantness,
please let us know on
[GitHub](https://github.com/thewileylab/ReviewR/issues).

-----

Please note that the ‘ReviewR’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
