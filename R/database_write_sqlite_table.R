#' Write SQLite Table
#' 
#' A function to write an RDA file to a SQLite database.
#'
#' @param table_location Where the rda file resides on disk
#' @param table_name the desired of the table contained on disk
#' @param db_connection connection information for SQLite DB
#'
#' @keywords internal
#' @export
#' @importFrom DBI dbWriteTable
#'
write_sqlite_table <- function(table_location, table_name, db_connection){
  load(table_location)
  DBI::dbWriteTable(conn = db_connection, name = table_name, value = table, overwrite = T)
}
