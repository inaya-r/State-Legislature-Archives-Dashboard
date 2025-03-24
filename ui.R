library("shiny")
library("shinythemes")
library("plotly")
library("shinydashboard")
library("DT")

ui <- dashboardPage(
  skin = "black",
  
  # Header
  dashboardHeader(title = "State Legislature Archive Trends"),
  
  # Sidebar
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Overall Trends", tabName = "overall_trends", icon = icon("chart-bar")),
      menuItem("Summary", tabName = "summary", icon = icon("info"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(tags$style(HTML("
      .box {
        border: none !important; 
        padding: 20px !important; 
        margin: 20px !important; /* Add margin between visuals */
        box-shadow: none !important;
      }
      .content {
        padding: 20px !important; 
      }
      .section-title {
        font-size: 20px;
        font-weight: bold;
        text-align: left;
        margin-bottom: 20px;
        margin-top: 40px; /* Space before the next title */
      }
      .spacer {
        margin-bottom: 30px !important; /* Adjust vertical spacing between sections */
      }
    "))),
    
    tabItems(
      # Overall Trends Tab
      tabItem(
        tabName = "overall_trends",
        
        # Regional Analysis Section
        h2("Regional and National Analysis", class = "section-title"),
        fluidRow(
          column(
            width = 6,
            div(
              class = "spacer",
              div(
                style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ddd;",
                uiOutput("national_summary")  # National Summary
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "spacer",
              div(style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ddd;",
                  plotlyOutput("stacked_bar_chart_regions", height = "400px")
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "spacer",
              div(
                style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ddd;",
                selectInput("region", "Select Region:",
                            choices = c("Northeast", "Midwest", "South", "West"),
                            selected = "Northeast"),
                plotlyOutput("regional_bar_chart", height = "400px")  # Regional Bar Chart
              )
            )
          )
        ),
        
        # State Comparisons Section
        h2("State Comparisons", class = "section-title"),
        fluidRow(
          column(
            width = 6,
            div(
              class = "spacer",
              selectInput("state_1", "Select First State:", choices = c("", state.name), selected = "NULL")
            )
          ),
          column(
            width = 6,
            div(
              class = "spacer",
              selectInput("state_2", "Select Second State:", choices = c("", state.name), selected = "NULL")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "spacer",
              id = "comparison_table",
              DTOutput("comparison_table")
            )
          )
        ),
        
        # Archives and Supplementary Resources Section
        h2("Archives by State and Supplementary Resources", class = "section-title"),
        fluidRow(
          column(
            width = 6,
            div(
              class = "spacer",
              div(
                style = "padding: 10px;",
                h3("Archives by State"),
                plotlyOutput("dataMap", height = "300px", width = "100%"),
                plotlyOutput("videoMap", height = "300px", width = "100%"),
                plotlyOutput("audioMap", height = "300px", width = "100%")
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "spacer",
              div(
                style = "padding: 10px;",
                h3("Supplementary Resources by State"),
                plotlyOutput("stacked_bar_chart", height = "900px", width = "100%")
              )
            )
          )
        )
      ),
      
      # Summary Tab
      tabItem(
        tabName = "summary",
        h2("Summary of State Archive Trends", class = "section-title"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ddd;",
              HTML('<h4>Key Highlights</h4>
            <p>This dashboard explores trends in state legislature archives, providing insights into the availability of data, video, and audio archives, as well as supplementary resources.</p>
            <ul>
              <li><strong>Average years of data archives:</strong> 22.5 years</li>
              <li><strong>Average years of video archives:</strong> 9 years</li>
              <li><strong>Average years of audio archives:</strong> 5 years</li>
              <li><strong>Most Data Archives:</strong> Kansas</li>
              <li><strong>Least Data Archives:</strong> Maryland</li>
              <li><strong>Most Committees:</strong> Mississippi</li>
              <li><strong>Least Committees:</strong> Maryland</li>
            </ul>
            <p>Explore detailed patterns through the interactive visualizations in the "Overall Trends" section.</p>')
            )
          )
        ),
        h2("Project Overview", class = "section-title"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ddd;",
              HTML('<h4>Created By:</h4>
              <p><strong>Inaya Rizvi</strong></p>
              <h4>Advisor</h4>
              <p><strong>Joseph Ferrare</strong></p>
              <h4>Purpose</h4>
              <p>Through this project, I collected data on legislative meetings from each of the 50 states and analyze it to gain insights into the level of information released on legislative matters by each state. This analysis will form the foundation for the preliminary stages of building a comprehensive database. The overarching goal was to understand how much data is accessible and to provide a visual overview of audio and video data availability.</p>
              <h4>Variables of Focus</h4>
              <ul>
                <li><strong>Number of years of video and/or audio:</strong> A core data point providing insights into accessibility.</li>
                <li><strong>Number of years of data:</strong> To gauge the comprehensiveness of data archives.</li>
              </ul>
              <p></p>')
            )
          )
        ),
        h2("Variable Explanations", class = "section-title"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ddd;",
              HTML('<h4>&#39;data&#39;</h4>
              <p>The general &#39;data&#39; variable encompasses all forms of data (including the other variables) to give a general idea of how far back legislative session meeting resources go.
              <h4>&#39;minutes&#39;, &#39;documents&#39; and &#39;agenda&#39</h4>
              <p>The values for the &#39;minutes&#39;, &#39;documents&#39; and &#39;agenda&#39; are measured as Yes, No, or Unclear, depending on if they were found on the official state legislature website or not. For easier analysis, the values were changed to Yes = 1, No = 0, and Unclear = -1.
              <h4>Data Collection</h4>
              <p>This dataset was outlined by Joseph Ferrare and created by Inaya Rizvi by researching state legislature websites. It contains data on what what information each state legislature releases regarding their session meetings.
              <p></p>')
            )
          )
        ),
        h2("Full Dataset", class = "section-title"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "padding: 10px; background-color: #ffffff; border: 1px solid #ddd;",
              DTOutput("full_data_table")  # Table to display the full dataset
            )
          )
        )
      )
      
    )
  )
)


