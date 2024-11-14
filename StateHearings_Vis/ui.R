library("shiny")
library("plotly")
library("shinydashboard")

# Define UI for application that draws a histogram
dashboardPage(
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
      
)