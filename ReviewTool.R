#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
## Requires the following packages:  install.packages(c("shiny", "tidyverse", "shinytemes", "DT", "bigrquery"))



library(shiny)
#library(shinythemes)

# Define UI for application 
ui <- fluidPage(
   #theme = shinytheme("yeti"),
   fluidRow(
     column(9, h2("You are seeing the record of:"), 
            h4(textOutput("subject_id_output")),
            h4(textOutput("hadm_id_output")),
            em("If you see 'Error: x>0 is not TRUE, that means that there are no data that match that request"),
            br()),
       column(1, textInput("subject_id", "Subject_ID"), br()),
       column(1, textInput("hadm_id", "HADM_ID"), br()),
       column(1, br(), submitButton("Submit"))
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

# Define server logic 
server <- function(input, output) {
  library(bigrquery)
  library(readr)
  library(DT)
  library(tidyverse)
  library(magrittr)
  
  options("httr_oob_default" = TRUE)
  project_id <- read_lines(file = "./bigquery_projectid.txt")

  admissions_data <- reactive({query_exec(paste0("select * from [mimic3.ADMISSIONS] where SUBJECT_ID = ", 
                                              as.numeric(input$subject_id), 
                                              " and HADM_ID = ", 
                                              as.numeric(input$hadm_id)), 
                                       project_id) %>% select(-SUBJECT_ID, -HADM_ID)})
  
  callout_data <- reactive({query_exec(paste0("select * from [mimic3.CALLOUT] where SUBJECT_ID = ", 
                                              as.numeric(input$subject_id), 
                                              " and HADM_ID = ", 
                                              as.numeric(input$hadm_id)), 
                                       project_id) %>% select(-SUBJECT_ID, -HADM_ID) %>% arrange(ROW_ID)})
  
  chartevents_data <- reactive({query_exec(paste0("select * from [mimic3.CHARTEVENTS] a inner join [mimic3.D_ITEMS] b on a.ITEMID=b.ITEMID where SUBJECT_ID = ", 
                                                  as.numeric(input$subject_id), 
                                                  " and HADM_ID = ", 
                                                  as.numeric(input$hadm_id)), 
                                           project_id) %>% 
      select(ROW_ID = a_ROW_ID, ICUSTAY_ID = a_ICUSTAY_ID, CHARTTIME = a_CHARTTIME, STORETIME = a_STORETIME, CGID = a_CGID, LABEL = b_LABEL, VALUE = a_VALUE, VALUENUM = a_VALUENUM, VALUEUOM = a_VALUEUOM, WARNING = a_WARNING, ERROR = a_ERROR, RESULTSTATUS = a_RESULTSTATUS, STOPPED = a_STOPPED, DBSOURCE = b_DBSOURCE, CATEGORY = b_CATEGORY, UNITNAME = b_UNITNAME) %>% mutate(LABEL = as.factor(LABEL), CATEGORY = as.factor(CATEGORY)) %>% arrange(CHARTTIME)})
  
  cptevents_data <- reactive({query_exec(paste0("select * from [mimic3.CPTEVENTS] where SUBJECT_ID = ", 
                                                as.numeric(input$subject_id), 
                                                " and HADM_ID = ", 
                                                as.numeric(input$hadm_id)), 
                                         project_id) %>% select(-SUBJECT_ID, -HADM_ID) %>% arrange(CHARTDATE)}) 
  
  diagnoses_icd_data <- reactive({query_exec(paste0("select * from [mimic3.DIAGNOSES_ICD] a inner join [mimic3.D_ICD_DIAGNOSES] b on a.ICD9_CODE = b.ICD9_CODE where SUBJECT_ID = ", 
                                          as.numeric(input$subject_id), 
                                          " and HADM_ID = ", 
                                          as.numeric(input$hadm_id)), 
                                   project_id) %>% select(ROW_ID = a_ROW_ID, SEQ_NUM = a_SEQ_NUM, ICD9_CODE = a_ICD9_CODE, LONG_TITLE = b_LONG_TITLE) %>% arrange(SEQ_NUM)})
  
  drgcodes_data <- reactive({query_exec(paste0("select * from [mimic3.DRGCODES] where SUBJECT_ID = ", 
                                               as.numeric(input$subject_id), 
                                               " and HADM_ID = ", 
                                               as.numeric(input$hadm_id)), 
                                        project_id) %>% select(-SUBJECT_ID, -HADM_ID) %>% arrange(ROW_ID)}) 
  
  icustays_data <- reactive({query_exec(paste0("select * from [mimic3.ICUSTAYS] where SUBJECT_ID = ", 
                                               as.numeric(input$subject_id), 
                                               " and HADM_ID = ", 
                                               as.numeric(input$hadm_id)), 
                                        project_id) %>% select(-SUBJECT_ID, -HADM_ID) %>% arrange(ROW_ID)}) 
  
  labevents_data <- reactive({query_exec(paste0("select * from [mimic3.LABEVENTS] a inner join [mimic3.D_LABITEMS] b on a.ITEMID=b.ITEMID where SUBJECT_ID = ", 
                                                              as.numeric(input$subject_id), 
                                                              " and HADM_ID = ", 
                                                              as.numeric(input$hadm_id)), 
                                                       project_id) %>% 
      select(ROW_ID = a_ROW_ID, CHARTTIME = a_CHARTTIME, LABEL = b_LABEL, FLUID = b_FLUID, CATEGORY = b_CATEGORY, VALUE = a_VALUE, VALUENUM = a_VALUENUM, VALUEUOM = a_VALUEUOM, FLAG = a_FLAG) %>% mutate(LABEL = as.factor(LABEL), CATEGORY = as.factor(CATEGORY)) %>% arrange(CHARTTIME)})
  
  microbiologyevents_data <- reactive({query_exec(paste0("select * from [mimic3.MICROBIOLOGYEVENTS] where SUBJECT_ID = ", 
                                                         as.numeric(input$subject_id), 
                                                         " and HADM_ID = ", 
                                                         as.numeric(input$hadm_id)), 
                                                  project_id) %>% select(-SUBJECT_ID, -HADM_ID) %>% arrange(CHARTTIME)}) 
  

  noteevent_data <- reactive({query_exec(paste0("select * from [mimic3.NOTEEVENTS] where SUBJECT_ID = ", 
                                           as.numeric(input$subject_id), 
                                           " and HADM_ID = ", 
                                           as.numeric(input$hadm_id)), 
                                    project_id) %>% 
      filter(is.na(ISERROR)) %>% 
      select(-ISERROR, -SUBJECT_ID, -HADM_ID, -ROW_ID) %>% 
      group_by(CHARTDATE, CHARTTIME, STORETIME, CATEGORY, CGID, TEXT) %>% 
      arrange(DESCRIPTION) %>% 
      summarise(DESCRIPTION = paste(DESCRIPTION,collapse = "; ")) %>% 
      ungroup() %>% 
      mutate(CATEGORY = as.factor(CATEGORY), DESCRIPTION = as.factor(DESCRIPTION)) %>% 
      arrange(CHARTDATE)})
  
  patients_data <- reactive({query_exec(paste0("select * from [mimic3.PATIENTS] where SUBJECT_ID = ", 
                                               as.numeric(input$subject_id)), 
                                        project_id) %>% select(-SUBJECT_ID)})
  
  prescriptions_data <- reactive({query_exec(paste0("select * from [mimic3.PRESCRIPTIONS] where SUBJECT_ID = ", 
                                                    as.numeric(input$subject_id), 
                                                    " and HADM_ID = ", 
                                                    as.numeric(input$hadm_id)), 
                                             project_id) %>% select(-SUBJECT_ID, -HADM_ID) %>% mutate(DRUG = as.factor(DRUG), FORM_UNIT_DISP = as.factor(FORM_UNIT_DISP), ROUTE = as.factor(ROUTE)) %>% arrange(STARTDATE)}) 
  
  procedureevents_mv_data <- reactive({query_exec(paste0("select * from [mimic3.PROCEDUREEVENTS_MV] a inner join [mimic3.D_ITEMS] b on a.ITEMID = b.ITEMID where SUBJECT_ID = ", 
                                                         as.numeric(input$subject_id), 
                                                         " and HADM_ID = ", 
                                                         as.numeric(input$hadm_id)), 
                                                  project_id) %>% select(ROW_ID = a_ROW_ID, ICUSTAY_ID = a_ICUSTAY_ID, STARTTIME = a_STARTTIME, ENDTIME = a_ENDTIME, LABEL = b_LABEL, DBSOURCE = b_DBSOURCE, VALUE = a_VALUE, VALUEUOM = a_VALUEUOM, LOCATION = a_LOCATION, LOCATIONCATEGORY = a_LOCATIONCATEGORY, STORETIME = a_STORETIME, CGID = a_CGID, ORDERID = a_ORDERID, LINKORDERIK = a_LINKORDERID, ORDERCATEGORYNAME = a_ORDERCATEGORYNAME, SECONDARYORDERCATEGORYNAME = a_SECONDARYORDERCATEGORYNAME, ORDERCATEGORYDESCRIPTION = a_ORDERCATEGORYDESCRIPTION, ISOPENBAG = a_ISOPENBAG, CONTINUEINNEXTDEPT = a_CONTINUEINNEXTDEPT, CANCELREASON = a_CANCELREASON, STATUSDESCRIPTION = a_STATUSDESCRIPTION, COMMENTS_EDITEDBY = a_COMMENTS_EDITEDBY, COMMENTS_CANCELEDBY = a_COMMENTS_CANCELEDBY, COMMENTS_DATE = a_COMMENTS_DATE) %>% mutate(ORDERCATEGORYNAME = as.factor(ORDERCATEGORYNAME), SECONDARYORDERCATEGORYNAME = as.factor(SECONDARYORDERCATEGORYNAME), ORDERCATEGORYDESCRIPTION = as.factor(ORDERCATEGORYDESCRIPTION)) %>% arrange(STARTTIME)}) 

  procedures_icd_data <- reactive({query_exec(paste0("select * from [mimic3.PROCEDURES_ICD] a inner join [mimic3.D_ICD_PROCEDURES] b on a.ICD9_CODE = b.ICD9_CODE where SUBJECT_ID = ", 
                                                         as.numeric(input$subject_id), 
                                                         " and HADM_ID = ", 
                                                         as.numeric(input$hadm_id)), 
                                                  project_id) %>% select(ROW_ID = a_ROW_ID, SEQ_NUM = a_SEQ_NUM, ICD9_CODE = a_ICD9_CODE, LONG_TITLE = b_LONG_TITLE) %>% arrange(SEQ_NUM)})
  
  
  services_labels <- data.frame(SERVICE = c("CMED","CSURG","DENT","ENT","GU","GYN","MED","NB","NBB","NMED","NSURG","OBS","ORTHO","OMED", "PSURG","PSYCH","SURG","TRAUMA","TSURG","VSURG"), DESCRIPTION = c("Cardiac Medical - for non-surgical cardiac related admissions
", "Cardiac Surgery - for surgical cardiac admissions", "Dental - for dental/jaw related admissions", "Ear, nose, and throat - conditions primarily affecting these areas", "Genitourinary - reproductive organs/urinary system", "Gynecological - female reproductive systems and breasts", "Medical - general service for internal medicine", "Newborn - infants born at the hospital", "Newborn baby - infants born at the hospital", "Neurologic Medical - non-surgical, relating to the brain", "Neurologic Surgical - surgical, relating to the brain", "Obstetrics - conerned with childbirth and the care of women giving birth", "Orthopaedic - surgical, relating to the musculoskeletal system", "Orthopaedic medicine - non-surgical, relating to musculoskeletal system", "Plastic - restortation/reconstruction of the human body (including cosmetic or aesthetic)", "Psychiatric - mental disorders relating to mood, behaviour, cognition, or perceptions", "Surgical - general surgical service not classified elsewhere", "Trauma - injury or damage caused by physical harm from an external source", "Thoracic Surgical - surgery on the thorax, located between the neck and the abdomen", "Vascular Surgical - surgery relating to the circulatory system"))
  services_data <- reactive({query_exec(paste0("select * from [mimic3.SERVICES] where SUBJECT_ID = ", 
                                               as.numeric(input$subject_id), 
                                               " and HADM_ID = ", 
                                               as.numeric(input$hadm_id)), 
                                        project_id) %>% select(-SUBJECT_ID, -HADM_ID) %>% arrange(TRANSFERTIME)})
  
  transfers_data <- reactive({query_exec(paste0("select * from [mimic3.TRANSFERS] where SUBJECT_ID = ", 
                                           as.numeric(input$subject_id), 
                                           " and HADM_ID = ", 
                                           as.numeric(input$hadm_id)), 
                                    project_id) %>% select(-SUBJECT_ID, -HADM_ID) %>% arrange(ROW_ID)})

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
  output$patients_tbl <- DT::renderDataTable(patients_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$prescriptions_tbl <- DT::renderDataTable(prescriptions_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$procedureevents_mv_tbl <- DT::renderDataTable(procedureevents_mv_data(), filter = 'top', options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$procedures_icd_tbl <- DT::renderDataTable(procedures_icd_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$services_labels_tbl <- renderTable(services_labels)
  output$services_tbl <-  DT::renderDataTable(services_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
  output$transfers_tbl <- DT::renderDataTable(transfers_data(), options = list(paging = FALSE, searchHighlight = TRUE), rownames=F)
}

# Run the application 
shinyApp(ui = ui, server = server)

