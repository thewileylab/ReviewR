
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReviewR <a href='https://reviewr.thewileylab.org'><img src='man/figures/logo.svg' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ReviewR)](https://CRAN.R-project.org/package=ReviewR)
[![R-CMD-check](https://github.com/thewileylab/ReviewR/workflows/R-CMD-check/badge.svg)](https://github.com/thewileylab/ReviewR/actions)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1488534.svg)](https://doi.org/10.5281/zenodo.1488534)
<!-- badges: end -->

## Overview

ReviewR is a portable Shiny tool to help you explore patient-level
electronic health record data and perform chart review in a single
integrated framework. It is is distributed as an R package using the
[golem](https://thinkr-open.github.io/golem/) framework.

This tool supports browsing clinical data in many different formats
including multiple versions of the OMOP common data model as well as the
MIMIC-III data model. If you are using a different data format, ReviewR
can be easily customized to support your use case (see [Support a Custom
Data
Model](https://reviewr.thewileylab.org/articles/customize_support_new_datamodel.html)
vignette).

At present ReviewR supports data stored in Google BigQuery or Postgres,
although it can be easily customized to access any database supported by
[dbplyr](https://dbplyr.tidyverse.org/) (see [Support a New Relational
Database Management
System](https://reviewr.thewileylab.org/articles/customize_support_new_rdbms.html)
vignette).

To record chart review data, ReviewR supports connections to REDCap
(Research Electronic Data Capture).

Full documentation available at
[reviewr.thewileylab.org](https://reviewr.thewileylab.org).

## Installation

Install ReviewR from CRAN:

``` r
install.packages('ReviewR')
```

### Development

First ensure you have the library `devtools` installed. If you do not,
please install using:

``` r
install.packages('devtools')
```

Then install the latest development release of ReviewR using:

``` r
devtools::install_github('thewileylab/ReviewR')
```

## Usage

To run the application from your local computer simply run:

``` r
ReviewR::run_app()
```

If you would like to deploy ReviewR on a server, see the [Shiny Server
Deployment](https://reviewr.thewileylab.org/articles/deploy_server.html)
vignette. If you will be connecting to clinical data using Google
BigQuery please see [Google BigQuery
Deployment](https://reviewr.thewileylab.org/articles/deploy_bigquery.html)
vignette.

#### Explore Clinical Data

Once the app has loaded, please navigate to the ‘Setup’ tab (found in
the left navigation menu).

First, in the left panel, select which type of database you would like
to connect to (e.g., Google BigQuery). You may also choose to select the
Demo SQLite module to access synthetic clinical data in order to explore
how ReviewR works without connecting to your own database. For BigQuery
connections, simply press “Sign in with Google” and you will be
redirected to authenticate with Google and then return to the
application.

Once you have successfully connected to a patient database, navigate to
the ‘Patient Search’ tab, located in the left sidebar. On this tab you
can see basic demographic information about each patient record. Select
a particular patient ID you would like to view and the ‘Chart Review’
tab will open. The top left panel includes the same demographic
information found in the ‘Patient Search’ table, while the bottom panel
contains the clinical information available for that record with tabs
for different data types. You can filter any individual column by typing
in the text box beneath each column name, or you can search across all
columns using the search bar in the upper right corner of the panel.
Note that both regular text as well as regular expression based searches
are supported. If you would like to move to another patient you can use
the patient navigation panel in the upper right corner. You can navigate
to a specific patient using the dropdown selector or simply move to the
next or previous patient records using the buttons.

#### Perform Chart Review

Once the app has loaded, please navigate to the ‘Setup’ tab (found in
the left navigation menu). On the setup tab, enter your institution’s
REDCap URL and an API token for a REDCap project. This project may
contain multiple REDCap instruments for data collection which are
selectable from the Setup interface. Once connected, please select the
REDCap field that contains your patient information as well as the field
that will contain reviewer information. Enter your name to keep track of
who has completed the review.

First, in the left panel, select which type of database you would like
to connect to (e.g., Google BigQuery). You may also choose to select the
Demo SQLite module to access synthetic clinical data in order to explore
how ReviewR works without connecting to your own database. For BigQuery
connections, simply press “Sign in with Google” and you will be
redirected to authenticate with Google and then return to the
application.

Next, you will want to connect to a REDCap project to store the results
of your chart review. In the right panel of the ‘Setup’ tab, enter your
institution’s REDCap URL and the API token for your desired REDCap
project and then click ‘Connect to REDCap’. Next, configure your REDCap
instrument by selecting which variable in your collection instrument
will store the Patient ID value, ReviewR will automatically populate
this question with the chart you are viewing. If you also want to record
who is performing the chart review you can configure the question that
identifies the chart reviewer and enter the name so it can also be
auto-entered for each review session. If you do not want to record the
reviewer identifier just select ‘(Not Applicable)’. When you have
completed both selects click ‘Configure REDCap Instrument’.
Congratulations - you are ready to perform your chart review!

You can now navigate to the ‘Patient Search’ tab, located in the left
sidebar. On this tab you can see basic demographic information about
each patient record as well as the review status for the record (e.g.,
“Review Not Started”, “Complete”, etc.). If you have enabled support for
multiple reviewers, then the review status of all other reviewers is
provided in addition to the status of the configured reviewer. Select a
particular patient ID you would like to view and the ‘Chart Review’ tab
will open. The top left panel includes the same demographic and review
status information found in the ‘Patient Search’ table, while the bottom
left panel contains the clinical information available for that record
with tabs for different data types. You can filter any individual column
by typing in the text box beneath each column name, or you can search
across all columns using the search bar in the upper right corner of the
panel. Note that both regular text as well as regular expression based
searches are supported. Chart review data can be entered into the middle
right panel. This panel contains each of the questions in your REDCap
project. If your project has multiple instruments, use the drop down to
select the instrument you would like to use. You’ll notice that the
questions identifying the patient and reviewer identifier are pre-filled
in and not editable. Once you have finished with your entry for a
record, set the REDCap status in the lower right panel and click ‘Save
to REDCap’. *You must click ‘Save to REDCap’ or the data will not be
saved in the app or uploaded to REDCap!* If you would like to move to
another patient you can use the patient navigation panel in the upper
right corner. You can navigate to a specific patient using the dropdown
selector or simply move to the next or previous patient records using
the buttons.

## Disclaimer

Please note that while our tool is designed to be as secure as your
local computer or server environment, you should check with your
clinical data warehouse and/or IT departments to make sure that you are
authorized to use our tool with real patient data. We make no guarantee
of security or privacy.

## Getting help

If you encounter bugs, errors, issues or other general unpleasantness,
please let us know on
[GitHub](https://github.com/thewileylab/ReviewR/issues).

------------------------------------------------------------------------

## Code of Conduct

Please note that the ReviewR project is released with a [Contributor
Code of Conduct](https://reviewr.thewileylab.org/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
