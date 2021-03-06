% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_data_model_detection.R
\name{user_table}
\alias{user_table}
\title{user_table}
\usage{
user_table(table_map, db_con, desired_cdm_table)
}
\arguments{
\item{table_map}{A \link[dplyr:reexports]{dplyr::tibble}, generated by \link{mod_data_model_detection} containing user database
tables and fields mapped to the determined CDM.}

\item{db_con}{A \link[DBI:dbConnect]{DBI::dbConnect} object that is created through user interaction with the Setup Tab}

\item{desired_cdm_table}{A string containing the table name in the desired CDM.}
}
\value{
A SQL data source \link[dplyr:tbl]{dplyr::tbl}, ie. tbl(db_con, user_table), that connects to the user table
that corresponds to the standard data model table.
}
\description{
user_table
}
\keyword{internal}
