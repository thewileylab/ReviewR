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

# Define UI for application that generates Shiny Widgets based on the Contents of a REDCap Instrument
ui <- dashboardPage(skin = 'red',
   
   # Application title
   dashboardHeader(title = "REDCap Shiny Widget Testing",titleWidth = 350),
   
   # Sidebar with some text input fields to get hooked into REDCap
   dashboardSidebar(width = 350,
     sidebarMenu(
       textInput(inputId = 'red_url',label = 'REDCap URL',value = 'https://redcap.ucdenver.edu/api/', width = 350),
       passwordInput(inputId = 'red_api',label = 'REDCap API Key',placeholder = 'Your API Key Here', width = 350),
       actionButton(inputId = 'red_connect',label = 'Connect!',icon = icon('user-astronaut'))
        )
   ),
   # Show a datatable of what is currently present in the instrument
   dashboardBody(
     fluidRow(
       column(width = 8,
        box(title = 'Instrument Info',collapsible = T,DT::dataTableOutput("meta_instrument"),width = '100%', status = 'danger')
        ),
       column(width = 4,
        tabBox(width = '100%', 
          title ='Dynamic Instrument Inputs', 
          id = 'instrument_1',
          tabPanel('Text Inputs', uiOutput("text_box"), width = '100%'),
          tabPanel('Note Inputs', uiOutput('note_box'), width = '100%'),
          tabPanel('Yes/No Inputs', uiOutput('yesno_box'), width = '100%'),
          tabPanel('Radio Inputs', uiOutput("radio_box"), width = '100%'),
          tabPanel('Checkbox Inputs',uiOutput("check_box"), width = '100%')
          ), # tabBox
        box(title = 'Output Test', collapsible = T, width = '100%', status = 'danger'
            
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
     # Rendder metadata table
      output$meta_instrument <- renderDataTable(
       datatable(instrument,options = list(pageLength = 25,scrollX = TRUE, scrollY = TRUE))
        )
      
      # Text Instrument UI Rendering
      output$text_box <- renderUI({
        text_input <- instrument %>% filter(field_type  == 'text')
        numTextWidgets <- nrow(text_input)
        lapply(1:numTextWidgets, function(i) {
          textInput(label = paste0(text_input$field_label[i]),inputId = paste0(text_input$field_name[i]),placeholder = text_input$text_validation_type_or_show_slider_number[i])
        })
      })
      
      # Note Instrument UI Rendering
      output$note_box <- renderUI({
        note_input <- instrument %>% filter(field_type  == 'notes')
        numNoteWidgets <- nrow(note_input)
        lapply(1:numNoteWidgets, function(i) {
          textAreaInput(label = paste0(note_input$field_label[i]),inputId = paste0(note_input$field_name[i]),resize = 'vertical',rows = 5, width = '100%', placeholder = 'Enter your response here')
        })
      })
      
      # Yes No Instrument UI Rendering
      output$yesno_box <- renderUI({ 
        yesno_input <- instrument %>% filter(field_type == 'yesno')
        numYesNoWidgets <- nrow(yesno_input)
        lapply(1:numYesNoWidgets, function(i) {
          radioButtons(label = paste0(yesno_input$field_label[i]),inputId = paste0(yesno_input$field_name[i]), choiceNames = c('Yes','No'),choiceValues = c(1,0))
        })
      })
      
      # Radio Instrument UI Rendering
      output$radio_box <- renderUI({ 
        radio_input <- instrument %>% filter(field_type == 'radio') %>% 
          mutate(choices = map(.x = select_choices_or_calculations,.f = str_split, pattern = '\\|',simplify = F))
        numRadioWidgets <- nrow(radio_input)
        lapply(1:numRadioWidgets, function(i) {
          radioButtons(label = paste0(radio_input$field_label[i]),inputId = paste0(radio_input$field_name[i]), choices = radio_input$choices[i][[1]][[1]],choiceValues = radio_input$clean_values[i])
        })
      })
      
      # Checkbox Instrument UI Rendering
      output$check_box <- renderUI({ 
        checkbox_input <- instrument %>% filter(field_type == 'checkbox') %>% 
          mutate(choices = map(.x = select_choices_or_calculations,.f = str_split, pattern = '\\|',simplify = F))
        numCheckBoxWidgets <- nrow(checkbox_input)
        lapply(1:numCheckBoxWidgets, function(i) {
          checkboxGroupInput(label = paste0(checkbox_input$field_label[i]),inputId = paste0(checkbox_input$field_name[i]), choices = checkbox_input$choices[i][[1]][[1]])
        })
      })
   }) # Observe Event
} # Server

# Run the application, please 
shinyApp(ui = ui, server = server)

