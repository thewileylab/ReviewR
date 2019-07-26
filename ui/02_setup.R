#
# This file contains all elements that are needed to configure ReviewR and render the setup page
#

source('modules/db_setup_module.R')
db_type <- callModule(db_setup_logic, 'db_setup_ns')
callModule(db_connect_logic, 'db_connect_ns', db_type$db_selection)


tagList(fluidRow(box(
  status = 'primary',
  solidHeader = F,
  width = 12,
  h2("ReviewR Setup", style = 'text-align: center;')
)),
fluidRow(column(
  width = 6,
  box(
    title = 'Connect to Patient Database',
    width = '100%',
    status = 'primary',
    solidHeader = F,
    db_setup_ui('db_setup_ns'),
    db_connect_ui('db_connect_ns')
  )
),
column(
  width = 6,
  box(
    title = 'Connect to REDCap Instrument',
    width = '100%',
    status = 'danger',
    solidHeader = F
  ),
  box(
    title = 'Configure REDCap Instrument',
    width = '100%',
    status = 'danger',
    solidHeader = F
  )
)))
