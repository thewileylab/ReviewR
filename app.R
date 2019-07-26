#  __________            .__              __________ 
#  \______   \ _______  _|__| ______  _  _\______   \
#   |       _// __ \  \/ /  |/ __ \ \/ \/ /|       _/
#   |    |   \  ___/\   /|  \  ___/\     / |    |   \
#   |____|_  /\___  >\_/ |__|\___  >\/\_/  |____|_  /
#          \/     \/             \/               \/ 
#                                       by WileyLab
#
# Making manual record review fun since 2019!
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(magrittr)
library(pool)

options(shiny.port = 8100)
options(shiny.reactlog=TRUE)

# Define a reactive UI for ReviewR
ui <- dashboardPage(skin = 'red',
                    dashboardHeader(title = 'ReviewR v2.0'),
                    dashboardSidebar(
                        sidebarMenu(id = 'main_tabs',
                        sidebarMenuOutput('application_menu')
                        )
                        ),
                    dashboardBody(
                        uiOutput('main_ui')
                        )
                    )

# Define server logic required to make ReviewR magic happen. 
server <- function(input, output, session) {
    ## Define a dynamic application menu
    output$application_menu <- renderMenu({
        sidebarMenu(id = 'main_tabs',
            menuItem(tabName = 'welcome', text = 'Welcome',icon = icon('home')),
            menuItem(tabName = 'setup', text = 'Setup', icon = icon('cog')),
            menuItem(tabName = 'patient_search', text = 'Patient Serach', icon = icon('users'))
            )
        })
    
    ## Source Sidebar Menu Contents
    output$welcome_tab <- renderUI({source('ui/01_welcome.R', local = T)[1]})
    output$setup_tab <- renderUI({source('ui/02_setup.R', local = T)[1]})
    #output$patient_search_tab <- renderUI({source('ui/03_patient_search.R', local = T)[1]})
    
    ## Render the main UI
    output$main_ui <- renderUI({
        tabItems(
            tabItem(tabName = 'welcome', uiOutput('welcome_tab')),
            tabItem(tabName = 'setup', uiOutput('setup_tab'))
            #tabItem(tabName = 'patient_search', uiOutput('patient_search_tab'))
            )
        })

}

# Run the application 
shinyApp(ui = ui, server = server)
