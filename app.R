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

#
# authors:  Laura Wiley, Luke Rasmussen, David Mayer
#

# Initialize the application ----
source('lib/initialize_reviewr.R')
init_data <- initialize_reviewr()

# Define a reactive UI for ReviewR ----
ui <- dashboardPage(title = 'ReviewR',
                    skin = 'red',
                    dashboardHeader(title = tags$a(href='https://github.com/orgs/thewileylab', target='_blank',
                                                   tags$img(src='logo.png', width = '125px', height = '50px')
                                                   ),
                                    tags$li(class = 'dropdown', 
                                            actionButton(inputId = 'quit',label = 'Leave ReviewR',
                                                         icon = icon('suitcase-rolling')
                                                         )
                                            )
                                    ),
                    dashboardSidebar(
                        sidebarMenu(id = 'main_tabs',
                        sidebarMenuOutput('application_menu')
                        )
                        ),
                    dashboardBody(
                        useShinyjs(),
                        uiOutput('main_ui')
                        )
                    )

# Define server logic required to make ReviewR magic happen. ----
server <- function(input, output, session) {
    
    ## Close Application when button is clicked
    observeEvent(input$quit, {
        stopApp()
        })
    
    ## Define a dynamic application menu
    output$application_menu <- renderMenu({
        sidebarMenu(id = 'main_tabs',
            menuItem(tabName = 'welcome', text = 'Welcome',icon = icon('home')),
            menuItem(tabName = 'setup', text = 'Setup', icon = icon('cog')),
            menuItem(tabName = 'patient_search', text = 'Patient Search', icon = icon('users')),
            menuItem(tabName = 'chart_review', text ='Chart Review', icon = icon('table'))
            )
        })
    
    ## Source Sidebar Menu Contents
    source('ui/01_welcome.R', local = T)$value
    source('ui/02_setup.R', local = T)$value
    source('ui/03_patient_search.R', local = T)$value
    source('ui/04_chart_review.R', local = T)$value
    
    ## Render the main UI
    output$main_ui <- renderUI({
        tabItems(
            tabItem(tabName = 'welcome', uiOutput('welcome_tab')),
            tabItem(tabName = 'setup', uiOutput('setup_tab')),
            tabItem(tabName = 'patient_search', uiOutput('patient_search_tab')),
            tabItem(tabName = 'chart_review', uiOutput('chart_review_tab'))
            )
        })
    
}

# Run the application ----
shinyApp(ui = ui, server = server)
