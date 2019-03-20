#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Added a silly comment to test slack Git Integration
library(shiny)
library(shinydashboard)
library(redcapAPI)
library(tidyverse)
library(magrittr)
library(DT)
library(shinyjs)
source('lib/render_redcap.R')

## Survey complete choices
sc_values <- c(0,1,2)
names(sc_values) <- c('Incomplete', 'Unverified','Complete')

# Define UI for application that generates Shiny Widgets based on the Contents of a REDCap Instrument
ui <- dashboardPage(skin = 'red',
   
   # Application title
   dashboardHeader(title = "REDCap Shiny Widget Testing",titleWidth = 350),
   
   # Sidebar with some text input fields to get hooked into REDCap
   dashboardSidebar(width = 350,
     sidebarMenu(
       textInput(inputId = 'red_url',label = 'REDCap URL',value = 'https://redcap.ucdenver.edu/api/', width = 350),
       passwordInput(inputId = 'red_api',label = 'REDCap API Key',placeholder = 'Your API Key Here', width = 350),
       actionButton(inputId = 'red_connect',label = 'Connect!',icon = icon('user-astronaut')),
       selectInput(inputId = 'which_field', label = 'Select Identifier Field', choices = NULL, width = 350),
       selectInput(inputId = 'which_patient',label = 'Select Patient', choices = NULL, width = 350),
       splitLayout(cellWidths = c('47%','47%'),
       actionButton(inputId = 'prev',label = '<<Previous',width = '100%'),
       actionButton(inputId = 'next',label = 'Next>>',width = '100%'))
        )
   ),
   # Show a datatable of what is currently present in the instrument
   dashboardBody(
     fluidRow(
       column(width = 8,
        box(title = 'REDCap Records', collapsible = T, DT::dataTableOutput("redcap_records"), width = '100%', status = 'danger'),
        box(title = 'Instrument Info',collapsible = T,DT::dataTableOutput("meta_instrument"),width = '100%', status = 'danger')
        ),
       column(width = 4,
        box(useShinyjs(),
            id = "rc_instrument",
            title = 'REDCap Instrument',collapsed = T,width = '100%', status = 'danger',
            tags$style(HTML("
                            #redcap_instrument {
                            height:676px;
                            overflow-y:scroll
                            }
                            ")),
            uiOutput('redcap_instrument'),
            selectInput(inputId = 'survey_complete',label = 'Form Complete?',choices = sc_values),
            actionButton(inputId = 'save',label = 'Save Responses')
        ),
        box(title = 'REDCap Staging Area', collapsible = T, width = '100%', status = 'danger',
            textOutput("next_participant_id"),
            dataTableOutput("responses", width = '100%'),
            actionButton(inputId ='upload',label = 'Upload to REDCap')
        ) # Box
      ) # Second column
    ) # Fluid Row
   ) # Dashboard body
) # Dashboard Page

# Define server logic required ingest REDCap Instrument and render Shiny Widgets
server <- function(input, output, session) {
   # Watch for the button press
    observeEvent(input$red_connect,{
     # Create REDCap Connection
      red_con <<- redcapConnection(url = input$red_url,token = input$red_api)
     # Extract Instrument metadata
      instrument <- exportMetaData(red_con) %>% 
        filter(!field_type %in% c('slider','calc','descriptive'))
    # Download previous REDCap Entries
      redcap_records <-exportRecords(red_con) 
    # Populate identifier dropdown
      redcap_choices <- reactive({
        redcap_choices <- instrument %>% 
          select(field_name) %>% 
          slice(-1) #The first row will always be the auto incremented field (or at least we will tell people that.)
      })
      observe({
        updateSelectInput(session = session, inputId = "which_field", choices = redcap_choices()$field_name)
      })
      test <<- input$which_field
    # Populate patient dropdown
      patient_choices <- reactive({
        #redcap_choices %<>% 
        #add_row(!!(input$which_field) := "new patient",.before = 1)
        patient_choices <- redcap_records[[input$which_field]]
        patient_choices <- rbind("new", patient_choices)
        #patient_choices %<>% 
        #  add_row(!!input$which_field := "new")
        #redcap_records %>%
          #{if (input$which_field == '') . else select(input$which_field)}
      })
      observe({
        updateSelectInput(session = session, inputId = "which_patient",choices = patient_choices())
      })
     # Rendder metadata table
      output$meta_instrument <- renderDataTable(
       datatable(instrument,options = list(pageLength = 10,scrollX = TRUE, scrollY = TRUE))
        )
      # Render previous entries
      output$redcap_records <- renderDataTable(
        datatable(redcap_records,options = list(pageLength = 10,scrollX = TRUE, scrollY = TRUE))
        )
      # Determine the next patricipantID
      max_participant_id <<- max(as.numeric(redcap_records[,1]))  #First column will be the auto incrementing field
      
      # Next participantID
      output$next_participant_id <- renderText({
        paste0("ParticipantID: ",max_participant_id + 1)
      })
      # Determine the participantID field
      participantID <<- instrument[1,1]
      
      # Create widget map
        REDCap_field_type <- c("text","text","text","dropdown","truefalse","yesno","radio","checkbox","notes")
        REDCap_field_val <- c(NA,"date_mdy","integer",NA,NA,NA,NA,NA,NA)
        reviewr_function <- c("reviewr_text","reviewr_date","reviewr_integer","reviewr_dropdown","reviewr_truefalse","reviewr_yesno","reviewr_radio","reviewr_checkbox","reviewr_notes")
        widget_map <- tibble(REDCap_field_type,REDCap_field_val, reviewr_function)
      
      # Join REDCap Instrument with widget_map
      instrument %<>% 
        left_join(widget_map, by = c("field_type" = "REDCap_field_type", "text_validation_type_or_show_slider_number" = "REDCap_field_val")) %>% 
        mutate(reviewr_inputID = paste0(field_name,"_", reviewr_function))
      
      # Determine what variables are needed to store information       
      temp1 <- instrument %>%
        filter(is.na(reviewr_function) == F) %>% 
        select(reviewr_inputID) %>% 
        slice(-1)
      fields <<- temp1$reviewr_inputID

      # Render the REDCap Instrument
      output$redcap_instrument <- renderUI({
        lapply(2:nrow(instrument), function(i) {  #Start with the second element, the first will always be the auto incrementing field.
          render_redcap(instrument[i,])
          })
      })
   }) # Observe Event
  
  # Create a data frame called responses, which will collect responses to be sent to REDCap. Lightly process
  # Lots of help here: https://towardsdatascience.com/get-started-with-examples-of-reactivity-in-in-shiny-apps-db409079dd11
  
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
      responses <<- data #responses <<- rbind(responses, data)
    } else {
      responses <<- data
    }
  }
  
  loadData <- function() {
    if (exists("responses")) {
      checkbox_responses <- responses %>% 
        select(contains('checkbox')) %>% 
        mutate_if(is.list, unname) %>% 
        gather() %>% 
        mutate(value = ifelse(value == 'NULL', NA, value)) %>% 
        unnest() %>% 
        mutate(temp = 1) %>% 
        unite(col_name, key, value, sep = '___') %>% 
        spread(col_name, temp, fill = 0) %>% 
        rename_all(str_remove_all, pattern = regex(pattern = '_reviewr_checkbox')) %>% 
        select(-contains('___NA'))
      
      other_responses <- responses %>% 
        select(-contains("checkbox")) %>% 
        unnest() %>% 
        rename_all(str_remove_all, pattern = regex(pattern = '(_reviewr_).*'))
      
      participant_id <- tibble(!! participantID := max_participant_id + 1)
      
      all_responses <<- cbind(participant_id,other_responses,checkbox_responses)
      datatable(t(all_responses), options = list(pageLength = 25, scrollX = TRUE, scrollY = TRUE))
    }
  }

  # Collect all of the user entered data
  #formData is a reactive function
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData())
  })
  
  # Show the previous responses and update with current response when save is clicked
  output$responses <- DT::renderDataTable({
    input$save
    loadData()
  })
  
  #Watch for Upload button press, do some stuff if pressed
  observeEvent(input$upload, {
    # Is the survey complete
    is_complete <- tibble(survey_complete = input$survey_complete)
    red_complete <<- cbind(all_responses, is_complete)
    importRecords(rcon = red_con, data = red_complete)
  })
  
  #Reset inputs
  observeEvent(input$save, {
    shinyjs::reset("rc_instrument")
  })
  
} # Server

# Run the application, please 
shinyApp(ui = ui, server = server)

