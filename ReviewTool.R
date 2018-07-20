#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
## Requires the following packages:  install.packages(c("shiny", "tidyverse", "shinytemes", "DT", "bigrquery"))


# First time setup for all dependencies:
# install.packages(c("shiny", "shinyjs", "DT", "bigrquery", "tidyverse"))

# install.packages("RPostgreSQL")

library(shiny)
library(shinyjs)
library(shinydashboard)
#library(shinythemes)

# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(title = "MIMICIII Chart Review Tool"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Patient Search", tabName = "patient_search", icon = icon("dashboard")),
      menuItem("Chart Review", icon = icon("th"), tabName = "chart_review")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "patient_search",
              h2("Select a patient to view"),
              DT::dataTableOutput('all_patients_tbl')
      ),
      
      tabItem(tabName = "chart_review",
              fluidRow(
                column(12, h2("You are seeing the record of:"), 
                       h4(textOutput("subject_id_output")),
                       h4(textOutput("hadm_id_output")),
                       em("If you see 'Error: x>0 is not TRUE, that means that there are no data that match that request"),
                       br())
              ),
              
              tabsetPanel(
                tabPanel("ADMISSIONS", DT::dataTableOutput('admissions_tbl')),
                tabPanel("CALLOUT", DT::dataTableOutput('callout_tbl')),
                tabPanel("CHARTEVENTS",DT::dataTableOutput('chartevents_tbl')), ## MAKE SURE TO MAP TO D_ITEMTS
                tabPanel("CPTEVENTS", br(), em("Please note that due to licensing restrictions, neither MIMIC-III nor course instructors can provide labels for these CPT codes."), br(), DT::dataTableOutput('cptevents_tbl')),
                #tabPanel("DATETIMEEVENTS", DT:dataTableOutput('datetimeevents_tbl')), ## MAKE SURE TO MAP TO D_ITEMS
                tabPanel("DIAGNOSES_ICD", br(), em("Please note that this table has been joined with 'D_ICD_DIAGNOSES' to provide the long title value for your reference."), br(), DT::dataTableOutput('diagnoses_icd_tbl')),
                tabPanel("DRGCODES", DT::dataTableOutput('drgcodes_tbl')),
                tabPanel("ICUSTAYS", DT::dataTableOutput('icustays_tbl')),
                # tabPanel("INPUTEVENTS_CV", DT::dataTableOutput('inputevents_cv_tbl')),
                # tabPanel("INPUTEVENTS_MV", DT::dataTableOutput('inputevents_mv_tbl')),
                tabPanel("LABEVENTS", br(), em("Please note that this table has been joined with 'D_LABITEMS' to provide the lab label, fluid, and category for your reference."), br(), DT::dataTableOutput('labevents_tbl')),
                tabPanel("MICROBIOLOGYEVENTS", DT::dataTableOutput('microbiologyevents_tbl')),
                tabPanel("NOTEEVENTS", br(), em("Please note that all 'ISERROR' notes have been removed from this table."),br(), DT::dataTableOutput('noteevent_tbl')),
                #tabPanel("OUTPUTEVENTS", DT::dataTableOutput('outputevents_tbl')),
                tabPanel("PATIENTS", DT::dataTableOutput('patients_tbl')),
                tabPanel("PRESCRIPTIONS", DT::dataTableOutput('prescriptions_tbl')),
                tabPanel("PROCEDUREEVENTS_MV", br(), em("Please note that this table has been joined with 'D_ITEMS' to provide the Label, and DBSource for your reference."), br(), DT::dataTableOutput('procedureevents_mv_tbl')),
                tabPanel("PROCEDURES_ICD", br(), em("Please note that this table has been joined with 'D_ICD_PROCEDURES' to provide the long title value for your reference."), br(), DT::dataTableOutput('procedures_icd_tbl')),
                tabPanel("SERVICES", br(), em("Please note that service descriptions have been provided from the data dictionary for your reference."), br(), DT::dataTableOutput('services_tbl'), br(), tableOutput('services_labels_tbl')),
                tabPanel("TRANSFERS", DT::dataTableOutput('transfers_tbl'))
              )
      )
    )
  )
)

query_admissions <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, admittime, dischtime, deathtime, admission_type, admission_location, discharge_location, insurance, language, religion, marital_status, ethnicity, edregtime, edouttime, diagnosis, hospital_expire_flag, has_chartevents_data from ",
         table_config["admissions"],
         " where SUBJECT_ID = ", as.numeric(input$subject_id),
         ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

query_callout <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, submit_wardid, submit_careunit, curr_wardid, curr_careunit, callout_wardid, callout_service, request_tele, request_resp, request_cdiff, request_mrsa, request_vre, callout_status, callout_outcome, discharge_wardid, acknowledge_status, createtime, updatetime, acknowledgetime, outcometime, firstreservationtime, currentreservationtime from ",
                      table_config["callout"],
                      " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                      ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(ROW_ID)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(row_id)
  }
}

query_chartevents <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select a.row_id, a.ICUSTAY_ID, a.CHARTTIME, a.STORETIME, a.CGID, b.LABEL, a.VALUE, a.VALUENUM, a.VALUEUOM, a.WARNING, a.ERROR, a.RESULTSTATUS, a.STOPPED, b.DBSOURCE, b.CATEGORY, b.UNITNAME from ",
                       table_config["chartevents"],
                       " a inner join ",
                       table_config["d_items"],
                       " b on a.ITEMID=b.ITEMID where SUBJECT_ID = ", as.numeric(input$subject_id),
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      select(ROW_ID = a_ROW_ID, ICUSTAY_ID = a_ICUSTAY_ID, CHARTTIME = a_CHARTTIME, STORETIME = a_STORETIME, CGID = a_CGID, LABEL = b_LABEL, VALUE = a_VALUE, VALUENUM = a_VALUENUM, VALUEUOM = a_VALUEUOM, WARNING = a_WARNING, ERROR = a_ERROR, RESULTSTATUS = a_RESULTSTATUS, STOPPED = a_STOPPED, DBSOURCE = b_DBSOURCE, CATEGORY = b_CATEGORY, UNITNAME = b_UNITNAME) %>%
      mutate(LABEL = as.factor(LABEL), CATEGORY = as.factor(CATEGORY)) %>%
      arrange(CHARTTIME)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      select(row_id, icustay_id, charttime, storetime, cgid, label, value, valuenum, valueuom, warning, error, resultstatus, stopped, dbsource, category, unitname) %>%
      mutate(label = as.factor(label), category = as.factor(category)) %>%
      arrange(charttime)
  }
}

query_cptevents <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, costcenter, chartdate, cpt_cd, cpt_number, cpt_suffix, ticket_id_seq, sectionheader, subsectionheader, description from ",
                              table_config["cptevents"],
                              " where SUBJECT_ID = ", as.numeric(input$subject_id),
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(CHARTDATE)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(chartdate)
  }
}

query_diagnoses_icd <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select a.row_id, a.seq_num, a.icd9_code, b.long_title from ",
                       table_config["diagnoses_icd"],
                       " a inner join ",
                       table_config["d_icd_diagnoses"],
                       " b on a.ICD9_CODE = b.ICD9_CODE where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      select(ROW_ID = a_ROW_ID, SEQ_NUM = a_SEQ_NUM, ICD9_CODE = a_ICD9_CODE, LONG_TITLE = b_LONG_TITLE) %>% arrange(SEQ_NUM)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      arrange(seq_num)
  }
}

query_drgcodes <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, drg_type, drg_code, description, drg_severity, drg_mortality from ",
                       table_config["drgcodes"],
                       " where SUBJECT_ID = ", 
                       as.numeric(input$subject_id), 
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(row_id)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(row_id)
  }
}

query_icustays <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, icustay_id, dbsource, first_careunit, last_careunit, first_wardid, last_wardid, intime, outtime, los from ",
                       table_config["icustays"],
                       " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(ROW_ID)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(row_id)
  }
}

query_labevents <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select a.ROW_ID, a.CHARTTIME, b.LABEL, b.FLUID, b.CATEGORY, a.VALUE, a.VALUENUM, a.VALUEUOM, a.FLAG from ",
                       table_config["labevents"],
                       " a inner join ",
                       table_config["d_labitems"],
                       " b on a.ITEMID=b.ITEMID where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(LABEL = as.factor(LABEL), CATEGORY = as.factor(CATEGORY)) %>%
      arrange(CHARTTIME)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(label = as.factor(label), category = as.factor(category)) %>%
      arrange(charttime)
  }
}

query_microbiologyevents <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, chartdate, charttime, spec_itemid, spec_type_desc, org_itemid, org_name, isolate_num, ab_itemid, ab_name, dilution_text, dilution_comparison, dilution_value, interpretation from ",
                       table_config["microbiologyevents"],
                       " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      arrange(CHARTTIME)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      arrange(charttime)
  }
}

query_noteevent <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select chartdate, charttime, storetime, category, description, cgid, iserror, text from ",
                       table_config["noteevent"],
                       " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      filter(is.na(ISERROR)) %>% 
      select(-ISERROR) %>% 
      group_by(CHARTDATE, CHARTTIME, STORETIME, CATEGORY, CGID, TEXT) %>% 
      arrange(DESCRIPTION) %>% 
      summarise(DESCRIPTION = paste(DESCRIPTION,collapse = "; ")) %>% 
      ungroup() %>% 
      mutate(CATEGORY = as.factor(CATEGORY), DESCRIPTION = as.factor(DESCRIPTION)) %>% 
      arrange(CHARTDATE)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      filter(is.na(iserror)) %>% 
      select(-iserror) %>% 
      group_by(chartdate, charttime, storetime, category, cgid, text) %>% 
      arrange(description) %>% 
      summarise(description = paste(description,collapse = "; ")) %>% 
      ungroup() %>% 
      mutate(category = as.factor(category), description = as.factor(description)) %>% 
      arrange(chartdate)
  }
}

query_patients <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, gender, dob, dod, dod_hosp, dod_ssn, expire_flag from ",
                       table_config["patients"],
                       " where SUBJECT_ID = ", as.numeric(input$subject_id))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text)
  }
}

query_all_patients <- function(table_config, database_type, connection) {
  query_text <- paste0("select subject_id, gender, dob, dod, dod_hosp, dod_ssn, expire_flag from ",
                       table_config["patients"])
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(subject_id = paste0("<a class='row_subject_id' href='#'>", subject_id, "</a>"))
  }
}


query_prescriptions <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, icustay_id, startdate, enddate, drug_type, drug, drug_name_poe, drug_name_generic, formulary_drug_cd, gsn, ndc, prod_strength, dose_val_rx, dose_unit_rx, form_val_disp, form_unit_disp, route from ",
                       table_config["prescriptions"],
                       " where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(DRUG = as.factor(DRUG), FORM_UNIT_DISP = as.factor(FORM_UNIT_DISP), ROUTE = as.factor(ROUTE)) %>%
      arrange(STARTDATE)
  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(drug = as.factor(drug), form_unit_disp = as.factor(form_unit_disp), route = as.factor(route)) %>%
      arrange(startdate)
  }
}

query_procedureevents_mv <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select a.ROW_ID, a.ICUSTAY_ID, a.STARTTIME, a.ENDTIME, b.LABEL, b.DBSOURCE, a.VALUE, a.VALUEUOM, a.LOCATION, a.LOCATIONCATEGORY, a.STORETIME, a.CGID, a.ORDERID, a.LINKORDERID, a.ORDERCATEGORYNAME, a.SECONDARYORDERCATEGORYNAME, a.ORDERCATEGORYDESCRIPTION, a.ISOPENBAG, a.CONTINUEINNEXTDEPT, a.CANCELREASON, a.STATUSDESCRIPTION, a.COMMENTS_EDITEDBY, a.COMMENTS_CANCELEDBY, a.COMMENTS_DATE from ",
                       table_config["procedureevents"],
                       " a inner join ",
                       table_config["d_items"],
                       " b on a.ITEMID = b.ITEMID where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>%
      mutate(ORDERCATEGORYNAME = as.factor(ORDERCATEGORYNAME), SECONDARYORDERCATEGORYNAME = as.factor(SECONDARYORDERCATEGORYNAME), ORDERCATEGORYDESCRIPTION = as.factor(ORDERCATEGORYDESCRIPTION)) %>%
      arrange(STARTTIME)

  }
  else {
    dbGetQuery(connection, query_text) %>%
      mutate(ordercategoryname = as.factor(ordercategoryname), secondaryordercategoryname = as.factor(secondaryordercategoryname), ordercategorydescription = as.factor(ordercategorydescription)) %>%
      arrange(starttime)
  }
}

query_procedures_icd <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select a.ROW_ID, a.SEQ_NUM, a.ICD9_CODE, b.LONG_TITLE from ",
                       table_config["procedures_icd"],
                       " a inner join ",
                       table_config["d_icd_procedures"],
                       " b on a.ICD9_CODE = b.ICD9_CODE where SUBJECT_ID = ", as.numeric(input$subject_id), 
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(SEQ_NUM)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(seq_num)
  }
}

query_services <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, transfertime, prev_service, curr_service from ",
                       table_config["services"],
                       " where SUBJECT_ID = ",as.numeric(input$subject_id),
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(TRANSFERTIME)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(transfertime)
  }
}

query_transfers <- function(table_config, input, database_type, connection) {
  query_text <- paste0("select row_id, icustay_id, dbsource, eventtype, prev_careunit, curr_careunit, prev_wardid, curr_wardid, intime, outtime, los from ",
                       table_config["services"],
                       " where SUBJECT_ID = ",as.numeric(input$subject_id),
                       ifelse(is.na(input$hadm_id), paste0(" and HADM_ID = ", as.numeric(input$hadm_id)), ""))
  if (database_type == "bigquery") {
    query_exec(query_text, connection) %>% arrange(ROW_ID)
  }
  else {
    dbGetQuery(connection, query_text) %>% arrange(row_id)
  }
}

# Define server logic 
server <- function(input, output, session) {
  library(bigrquery)
  library(RPostgreSQL)
  
  library(readr)
  library(DT)
  library(tidyverse)
  library(magrittr)
  
  options("httr_oob_default" = TRUE)
  
  database_type <- "postgres"  # postgres | bigquery
  if (database_type == "bigquery") {
    connection <- read_lines(file = "./bigquery_projectid.txt")
  }
  else if (database_type == "postgres") {
    pg_connection_details_df <- read.csv(file="./conf/postgres_secrets.csv", stringsAsFactors = FALSE)
    pg_connection_details <- as.list(pg_connection_details_df[,"value"])
    names(pg_connection_details) <- pg_connection_details_df[,"key"]
    rm(pg_connection_details_df)
    
    drv <- dbDriver("PostgreSQL")
    connection <- dbConnect(drv, dbname = pg_connection_details["dbname"],
                     host = pg_connection_details["host"], port = as.integer(pg_connection_details["port"]),
                     user = pg_connection_details["user"], password = pg_connection_details["password"])
  }
  else {
    stop("Currently this application only supports Postgres and BigQuery databases")
  }
  
  # Load the table mapping configuration.  For a given database system, this will tell us
  # the correct SQL statements to use
  table_config_df <- read.csv(file=paste0("./conf/table_map_",database_type,".csv"), stringsAsFactors = FALSE)
  table_config <- as.list(table_config_df[,"database_table"])
  names(table_config) <- table_config_df[,"table_key"]
  rm(table_config_df)

  admissions_data <- reactive({query_admissions(table_config, input, database_type, connection)})
  callout_data <- reactive({query_callout(table_config, input, database_type, connection)})
  chartevents_data <- reactive({query_chartevents(table_config, input, database_type, connection)})
  cptevents_data <- reactive({query_cptevents(table_config, input, database_type, connection)}) 
  diagnoses_icd_data <- reactive({query_diagnoses_icd(table_config, input, database_type, connection)})
  drgcodes_data <- reactive({query_drgcodes(table_config, input, database_type, connection)})
  icustays_data <- reactive({query_icustays(table_config, input, database_type, connection)})
  labevents_data <- reactive({query_labevents(table_config, input, database_type, connection)})
  microbiologyevents_data <- reactive({query_microbiologyevents(table_config, input, database_type, connection)})
  noteevent_data <- reactive({query_noteevent(table_config, input, database_type, connection)})
  patients_data <- reactive({query_patients(table_config, input, database_type, connection)})
  prescriptions_data <- reactive({query_prescriptions(table_config, input, database_type, connection)})
  procedureevents_mv_data <- reactive({query_procedureevents_mv(table_config, input, database_type, connection)})
  procedures_icd_data <- reactive({query_procedures_icd(table_config, input, database_type, connection)})
  services_labels <- data.frame(SERVICE = c("CMED","CSURG","DENT","ENT","GU","GYN","MED","NB","NBB","NMED","NSURG","OBS","ORTHO","OMED", "PSURG","PSYCH","SURG","TRAUMA","TSURG","VSURG"), DESCRIPTION = c("Cardiac Medical - for non-surgical cardiac related admissions
", "Cardiac Surgery - for surgical cardiac admissions", "Dental - for dental/jaw related admissions", "Ear, nose, and throat - conditions primarily affecting these areas", "Genitourinary - reproductive organs/urinary system", "Gynecological - female reproductive systems and breasts", "Medical - general service for internal medicine", "Newborn - infants born at the hospital", "Newborn baby - infants born at the hospital", "Neurologic Medical - non-surgical, relating to the brain", "Neurologic Surgical - surgical, relating to the brain", "Obstetrics - conerned with childbirth and the care of women giving birth", "Orthopaedic - surgical, relating to the musculoskeletal system", "Orthopaedic medicine - non-surgical, relating to musculoskeletal system", "Plastic - restortation/reconstruction of the human body (including cosmetic or aesthetic)", "Psychiatric - mental disorders relating to mood, behaviour, cognition, or perceptions", "Surgical - general surgical service not classified elsewhere", "Trauma - injury or damage caused by physical harm from an external source", "Thoracic Surgical - surgery on the thorax, located between the neck and the abdomen", "Vascular Surgical - surgery relating to the circulatory system"))
  services_data <- reactive({query_services(table_config, input, database_type, connection)})
  transfers_data <- reactive({query_transfers(table_config, input, database_type, connection)})
  
  output$subject_id_output <- renderText({paste("Subject ID - ",input$subject_id)})
  output$hadm_id_output <- renderText({paste("HADM ID - ", input$hadm_id)})
  
  output$admissions_tbl <- DT::renderDataTable(admissions_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$callout_tbl <- DT::renderDataTable(callout_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$chartevents_tbl <- DT::renderDataTable(chartevents_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$cptevents_tbl <- DT::renderDataTable(cptevents_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F) 
  output$diagnoses_icd_tbl <- DT::renderDataTable(diagnoses_icd_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$drgcodes_tbl <- DT::renderDataTable(drgcodes_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$icustays_tbl <- DT::renderDataTable(icustays_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F) 
  output$labevents_tbl <- DT::renderDataTable(labevents_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$microbiologyevents_tbl <- DT::renderDataTable(microbiologyevents_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$noteevent_tbl <- DT::renderDataTable(noteevent_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$all_patients_tbl <- DT::renderDataTable(
    query_all_patients(table_config, database_type, connection),
    options = list(paging = TRUE, pageLength = 20, searchHighlight = TRUE),
    escape=FALSE, rownames=F,
    callback = JS(
      'table.on("click", "tr td a.row_subject_id", function() {
         Shiny.onInputChange("subject_id", $(this).text());
         $(".main-sidebar li a").click();
       });'))
  output$patients_tbl <- DT::renderDataTable(patients_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$prescriptions_tbl <- DT::renderDataTable(prescriptions_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$procedureevents_mv_tbl <- DT::renderDataTable(procedureevents_mv_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$procedures_icd_tbl <- DT::renderDataTable(procedures_icd_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$services_labels_tbl <- renderTable(services_labels)
  output$services_tbl <-  DT::renderDataTable(services_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$transfers_tbl <- DT::renderDataTable(transfers_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)

  session$onSessionEnded(function() {
    if (database_type == "postgres") {
      dbDisconnect(connection);
    }
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)

