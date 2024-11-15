library("shiny")
library("plotly")
library("shinydashboard")
library("readxl")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "black", 
    
    # App title 
    dashboardHeader(title = "State Legislature Trends"),
    
    # sidebar 
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("About", tabName = "about", icon = icon("info")), 
        menuItem(
          "Dashboard", 
          tabName = "dashboard", 
          icon = icon("dashboard") 
        ),
        textInput("search", "Search", placeholder = "Search..."), 
        selectInput("state", "Select State", choices = sort(state.name))
      )
    ),
    
    # body 
    dashboardBody(
      tabItems(
        
        # about tab content
        tabItem(tabName = "about",
                fluidRow(
                  box(title = "About This App", width = 12, status = "info",
                      solidHeader = TRUE,
                      "This dashboard provides insights into state legislatures, 
                        including committees per state and other trends.")
                )
        ),
        
        #dashboard tab content 
        tabItem(tabName = "dashboard",
          fluidRow(
            # heat map
            box(title = "US Heatmap: Number of Committees by State", width = 12, 
                status = "primary", plotlyOutput("us_map", height = "600px")
            )
          ),
          
          fluidRow(
            # other visual 
            box(title = "other vis 1", width = 6, status = "warning", 
                "vis 1 content"
            ),
            box(title = "other vis 2", width = 6, status = "warning",
                "vis 2 content"
            )
        )
  
      )
    )
      
  )
)