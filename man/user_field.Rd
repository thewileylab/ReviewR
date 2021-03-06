% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_data_model_detection.R
\name{user_field}
\alias{user_field}
\title{user_field}
\usage{
user_field(table_map, desired_cdm_table, desired_cdm_field)
}
\arguments{
\item{table_map}{A \link[dplyr:reexports]{dplyr::tibble}, generated by \link{mod_data_model_detection} containing user database
tables and fields mapped to the determined CDM.}

\item{desired_cdm_table}{A string containing the table name in the desired CDM.}

\item{desired_cdm_field}{A string containing the field name in the desired CDM.}
}
\value{
A string containing the user database field pertaining to the standard data model field
}
\description{
user_field
}
\keyword{internal}
