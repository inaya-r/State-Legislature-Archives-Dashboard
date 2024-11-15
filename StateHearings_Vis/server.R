library("shiny")
library("dplyr")
library("ggplot2")
library("plotly")
library("lubridate")
library("reshape2")
library("readxl")
library("tidyverse")

server <- function (input, output, session) {
  
  # loading and preprocessing data
  committee_data <- read_excel("~/Desktop/StateHearings/StateHearings/StateHearings_Vis/StateHearings.xlsx", sheet = "Committees")

  state_committee_counts <- committee_data %>%
    separate_rows(standing_committee_names, sep = ";") %>%
    mutate(standing_committee_names = str_trim(standing_committee_names)) %>%
    group_by(state) %>%
    summarise(Committee_Count = n()) %>%
    mutate(state_code = state.abb[match(state, state.name)]) # map state names to abbreviations
  
  # rendering US heatmap
  output$us_map <- renderPlotly({ 
  
    # creating chloropleth map
    fig <- plot_ly(
      state_committee_counts,
      type = "choropleth",
      locations = ~state_code,
      z = ~Committee_Count, # variable we're coloring by
      locationmode = "USA-states", # using US states
      colorscale = list(
        c(0, "lightblue"),
        c(1, "darkblue")
      ),
      colorbar = list(
        title = "Committee Count",
        orientation = "h",  # horizontal color bar
        x = 0.5, # centered horizontally
        xanchor = "center",
        y = -0.2 # below map
      )
    ) %>%
      layout (
        title = "Number of Committees by State",
        geo = list(
          scope = "usa", # restricting to US
          projection = list(type = "albers usa"),
          domain = list(x = c(0, 1), y = c(0, 1)) # using full width and height of box
        )
      )
    
    fig
    # test_fig <- plot_ly(
    #   type = "choropleth",
    #   locations = c("AL", "AK", "AZ"),
    #   z = c(10, 20, 30),
    #   locationmode = "USA-states",
    #   colorscale = "Blues"
    # ) %>%
    #   layout(
    #     geo = list(scope = "usa"),
    #     colorbar = list(
    #       title = "Test",
    #       orientation = "h",
    #       x = 0.5,
    #       xanchor = "center",
    #       y = -0.2
    #     )
    #   )
    # test_fig
  })
  
}