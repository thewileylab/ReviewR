# ReviewR Internals

## reviewr_config
This is the central container that holds all configuration information used by ReviewR during runtime.  When fully initialized and connected to a database, you can expect the reviewr_config list to have the following fields:

| field | description|
|-------| -----------|
| data_model | Which data model to connect to (OMOP or MIMIC) |
| db_type | Which type of database system to connect to (postgres or bigquery) |
| host | postgres only - The database server to connect to (host name or IP address) |
| port | postgres only - The port on the database server to connect to |
| database | postgres only - The name of the database to connect to. |
| user | postgres only - Used during initialization, contains the username credentials for the database.  Cleared once connected. |
| password | postgres only - Used during initialization, contains the password for the database.  Cleared once connected. |
| project_id | bigquery only - The project identifier to connect to. |
| dataset | bigquery only - The dataset within the project to connect to. |
| connection | The underlying DBI connection object, once established. |
| table_map | The table map (containing reconciled table and column names) for the loaded data model. |