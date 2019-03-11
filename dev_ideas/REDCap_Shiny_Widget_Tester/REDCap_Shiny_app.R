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
source('lib/render_redcap.R')

# Define UI for application that generates Shiny Widgets based on the Contents of a REDCap Instrument
ui <- dashboardPage(skin = 'red',
   
   # Application title
   dashboardHeader(title = "REDCap Shiny Widget Testing",titleWidth = 350),
   
   # Sidebar with some text input fields to get hooked into REDCap
   dashboardSidebar(width = 350,
     sidebarMenu(
       textInput(inputId = 'red_url',label = 'REDCap URL',value = 'https://redcap.ucdenver.edu/api/', width = 350),
       passwordInput(inputId = 'red_api',label = 'REDCap API Key',placeholder = 'Your API Key Here', width = 350,value = 'F3531A8BF589D54CEFDB5241BE5FCCC9'),
       actionButton(inputId = 'red_connect',label = 'Connect!',icon = icon('user-astronaut'))
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
        #tabBox(width = '100%', 
        #  title ='Dynamic Instrument Inputs', 
        #  id = 'instrument_1',
        #  tabPanel('Text Inputs', uiOutput("text_box"), width = '100%'),
        #  tabPanel('Note Inputs', uiOutput('note_box'), width = '100%'),
        #  tabPanel('Dropdown Inputs', uiOutput('dropdown_box'), width = '100%'),
        #  tabPanel('Yes/No Inputs', uiOutput('yesno_box'), width = '100%'),
        #  tabPanel('True/False Inputs', uiOutput('truefalse_box'), width = '100%'),
        #  tabPanel('Radio Inputs', uiOutput("radio_box"), width = '100%'),
        #  tabPanel('Checkbox Inputs',uiOutput("check_box"), width = '100%')
        #  ), # tabBox
        box(title = 'REDCap Instrument',collapsed = T,width = '100%',status = 'danger',
            uiOutput('redcap_instrument')
        ),
        box(title = 'Output Test', collapsible = T, width = '100%', status = 'danger',
            textOutput("next_participant_id")
            #textOutput("field_inputs")
        ) # Box
      
        ) # Second column
    ) # Fluid Row
   ) # Dashboard body
) # Dashboard Page

# Define server logic required ingest REDCap Instrument and render Shiny Widgets
server <- function(input, output) {
   # Watch for the button press
    observeEvent(input$red_connect,{
     # Create REDCap Connection
      red_con <- redcapConnection(url = input$red_url,token = input$red_api)
     # Extract Instrument metadata
      instrument <- exportMetaData(red_con) %>% 
        filter(!field_type %in% c('slider','calc','descriptive'))
    # Download previous REDCap Entries
      redcap_records <-exportRecords(red_con) 
     # Rendder metadata table
      output$meta_instrument <- renderDataTable(
       datatable(instrument,options = list(pageLength = 25,scrollX = TRUE, scrollY = TRUE))
        )
      # Render previous entries
      output$redcap_records <- renderDataTable(
        datatable(redcap_records,options = list(pageLength = 25,scrollX = TRUE, scrollY = TRUE))
        )
      # Determine the next patricipantID
      max_participant_id <- max(as.numeric(redcap_records$participant_id))  
      
      # Next participantID
      output$next_participant_id <- renderText({
        paste0("ParticipantID: ",max_participant_id + 1)
      })
      
      # Access dynamically created variables ##NEEDS WORK
      #output$field_inputs <- renderText({
      #  input[[instrument$field_name[1]]] ## schema needed for input variables.
      #})
      
      # Create widget map
      REDCap_field_type <- c("text","text","text","dropdown","truefalse","yesno","radio","checkbox","notes")
      REDCap_field_val <- c(NA,"date_mdy","integer",NA,NA,NA,NA,NA,NA)
      reviewr_function <- c("reviewr_text","reviewr_date","reviewr_integer","reviewr_dropdown","reviewr_truefalse","reviewr_yesno","reviewr_radio","reviewr_checkbox","reviewr_notes")
      widget_map <- tibble(REDCap_field_type,REDCap_field_val, reviewr_function)
      
      
      # Join REDCap Instrument with widget_map
      instrument %<>% 
        left_join(widget_map, by = c("field_type" = "REDCap_field_type", "text_validation_type_or_show_slider_number" = "REDCap_field_val"))
      
      # Render the REDCap Instrument
      output$redcap_instrument <- renderUI({
        lapply(1:nrow(instrument), function(i) {
          render_redcap(instrument[i,])
          })
      })
      
   }) # Observe Event
} # Server

# Run the application, please 
shinyApp(ui = ui, server = server)

