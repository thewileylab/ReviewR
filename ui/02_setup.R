#
# This file contains all elements that are needed to configure ReviewR and render the Setup Tab
#

# Source required setup modules----------
source('modules/db_setup_module.R')
db_setup_vars <- callModule(db_setup_logic, 'db_setup_ns')
db_connection_vars <- callModule(db_connect_logic, 'db_setup_ns', db_setup_vars$db_selection )
db_connection <- callModule(db_initialize, 'db_setup_ns', db_setup_vars$db_selection, db_connection_vars$bq_project)

# Define Setup Tab UI ----------
tagList(
  fluidRow(
    box(
      title = h2("ReviewR Setup", style = 'text-align: center;'),
      width = 12,
      status = 'primary',
      solidHeader = F
      )
    ),
fluidRow(
  column(
    width = 6,
    box(
      #Box Setup
      title = 'Connect to Patient Database',
      width = '100%',
      status = 'primary',
      solidHeader = F,
      #Box Contents
      db_setup_ui('db_setup_ns')
      )
    ),
  column(
    width = 6,
    box(
      #Box Setup
      title = 'Connect to REDCap Instrument',
      width = '100%',
      status = 'danger',
      solidHeader = F
      #Box Contents
      ),
    box(
      #Box Setup
      title = 'Configure REDCap Instrument',
      width = '100%',
      status = 'danger',
      solidHeader = F
      #Box Contents
      )
    )
  )
)
