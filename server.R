library("shiny")
library("ggplot2")
library("plotly")
library("reshape2")
library("readxl")
library("tidyverse")
library("usmap")
library("tidyr")
library("shinydashboard")
library("DT")



server <- function(input, output, session) {
  
  # Load and preprocess data
  full_data <- read_excel("data/StateHearings.xlsx", 
                          sheet = "State Legislative - Numerical")
  
  # removing unnecessary column 
  full_data <- full_data %>% select(-"...15")


  # Colors
  dark_purple <- "#704A4B"
  light_purple <- "#FFABAB"
  dark_green <- "#656B59"
  light_green <- "#B9C4A5"
  light_orange <- "#F8DBA2"
  dark_orange <- "#A86834"
  light_blue <- "#A8D7FF"
  dark_blue <- "#303D66"
  
  # Defining regions
  regions <- list(
    
    Northeast = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
                  "Rhode Island", "Vermont", "New York", "New Jersey", 
                  "Pennsylvania"),
    Midwest = c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", 
                "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", 
                "South Dakota", "Wisconsin"),
    South = c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", 
              "Kentucky", "Louisiana", "Maryland", "Mississippi", 
              "North Carolina", "Oklahoma", "South Carolina", "Tennessee", 
              "Texas", "Virginia", "West Virginia"),
    West = c("Alaska", "Arizona", "California", "Colorado", "Hawaii", 
             "Idaho", "Montana", "Nevada", "New Mexico", "Oregon", "Utah", 
             "Washington", "Wyoming")
  )
 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  
  # US Heat Map
  output$us_map <- renderPlotly({
    # Process the committee data
    committee_data <- full_data %>%
      separate_rows(standing_committee_names, sep = ";") %>%
      group_by(state) %>%
      summarise(Committee_Count = n()) %>%
      mutate(
        state_code = state.abb[match(state, state.name)],
        hover_text = paste(
          "State:", state,
          "<br>Committee Count:", Committee_Count
        ),  # Define custom hover text
        is_selected = ifelse(state %in% c(input$state_1, input$state_2), 
                             "Selected", "Not Selected")
      )
    
    # Define colors for highlighting selected states
    committee_data <- committee_data %>%
      mutate(
        # Highlight selected states
        color = ifelse(is_selected == "Selected", "yellow", NA)  
      )
    
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
    
    # Create the choropleth map
    fig <- plot_ly(
      committee_data,
      type = "choropleth",
      locations = ~state_code,
      z = ~Committee_Count,
      locationmode = "USA-states",
      colorscale = list(c(0, light_blue), c(1, dark_blue)),
      colorbar = list(
        title = "Committee Count",
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2
      ),
      text = ~hover_text,  # Use custom hover text
      hoverinfo = "text"   # Display only the text in hover
    )
    
    # Add markers for selected states
    fig <- fig %>%
      add_trace(
        type = "scattergeo",
        mode = "markers",
        lat = ~state.center$y[match(state_code, state.abb)],
        lon = ~state.center$x[match(state_code, state.abb)],
        text = ~hover_text,
        hoverinfo = "text",
        marker = list(
          color = "yellow",
          size = 10,
          opacity = 0.8
        ),
        data = committee_data %>% filter(is_selected == "Selected")
      )
    
    # Layout adjustments
      fig <- fig %>%
        layout(
          title = list(text = "Number of Committees by State", y = 0.95),
          margin = list(t = 80, b = 40, l = 50, r = 50),
          geo = list(
            scope = "usa",
            projection = list(type = "albers usa"),
            showgrid = FALSE,
            showframe = FALSE
          )
        )
    
    
    fig
  })
  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  
  # Heatmap of Archive Years
  output$archive_years_heatmap <- renderPlotly({
    heatmap_data <- full_data %>%
      select(state, years_audio, years_video, years_data) %>%
      pivot_longer(cols = c(years_audio, years_video, years_data), 
                   names_to = "archive_type", values_to = "years") %>%
      mutate(archive_type = recode(archive_type, years_audio = "Audio", 
                                   years_video = "Video", years_data = "Data"))
    
    ggplotly(
      ggplot(heatmap_data, aes(x = archive_type, y = state, fill = years)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "lightblue", high = "darkblue", 
                            na.value = "grey50") +
        labs(title = "Years of Audio, Video, and Data Archives by State", 
             x = "Archive Type", y = "State") +
        theme_minimal()
    )
  })
  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  
  
  output$stacked_bar_chart <- renderPlotly({
    
    # Define sorting order based on available resources
    # most available at top, least available at bottom
    state_order <- full_data %>%
      mutate(
        has_minutes = ifelse(minutes == 1, 1, 0),
        has_agenda = ifelse(agenda == 1, 1, 0),
        has_documents = ifelse(documents == 1, 1, 0),
        unclear_minutes = ifelse(minutes == -1, 1, 0),
        unclear_agenda = ifelse(agenda == -1, 1, 0),
        unclear_documents = ifelse(documents == -1, 1, 0),
        total_yes = has_minutes + has_agenda + has_documents, # Count Yes count
        total_no = (minutes == 0) + (agenda == 0) + (documents == 0), # Count No
        total_unclear = unclear_minutes + unclear_agenda + unclear_documents  # Count Unclear
      ) %>%
      group_by(state) %>%
      summarise(
        total_yes = sum(total_yes),
        total_no = sum(total_no),
        total_unclear = sum(total_unclear)
      ) %>%
      # Prioritize Yes, then No, then Unclear
      arrange(desc(total_yes), total_no, total_unclear) %>% 
      pull(state)
    
    # Step 2: Prepare data for plotting
    archive_summary <- full_data %>%
      pivot_longer(cols = c(minutes, agenda, documents),
                   names_to = "archive_type",
                   values_to = "value") %>%
      mutate(
        value = factor(value, levels = c(1, 0, -1), 
                       labels = c("No", "Yes", "Unclear")),
        state = factor(state, levels = state_order),  # Apply new sorting order
        highlight = ifelse(state %in% c(input$state_1, input$state_2), 
                           "Selected", " ")  # Highlight selected states
      )
    
    # Define custom colors
    custom_colors <- c("Yes" = "#B9C4A5",     # Green for Yes
                       "No" = "#D95755",      # Red for No
                       "Unclear" = "#FFEEE1") # Orange for Unclear
    
    # Define border colors for highlighting
    border_colors <- c("Selected" = "black", " " = "transparent")  
    
    gg <- ggplot(archive_summary, aes(x = state, fill = value, 
                                      color = highlight)) +
      geom_bar(position = "fill", width = 0.8, size = 0.8) +  
      facet_wrap(~archive_type, ncol = 3) +
      labs(title = "State Archive Availability by Type",
           x = "State", y = "Proportion") +
      coord_flip() +
      scale_fill_manual(values = custom_colors) +  
      scale_color_manual(values = border_colors) +  
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 8),   
        axis.text.x = element_blank(),        
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12, face = "bold"), 
        legend.position = "bottom",                
        legend.title = element_blank(),             
        panel.spacing = unit(0.1, "lines"),       # Add spacing between columns
        plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.y = element_text(margin = margin(r = 5)),  
        axis.title.x = element_blank()
      )
    
    ggplotly(gg)  # Convert ggplot to plotly
  })
  
  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

  # Choropleth Maps (Data, Video, Audio)
    
    # Overall Data Map 
    output$dataMap <- renderPlotly({
    
        
        # Prepare data for data archives
        data_map <- full_data %>%
          mutate(
            state_code = state.abb[match(state, state.name)], # abbreviate states
            lat = state.center$y[match(state_code, state.abb)],  # Latitude
            lon = state.center$x[match(state_code, state.abb)],   # Longitude
            highlight = ifelse(state %in% c(input$state_1, input$state_2), 1, 0),  
            hover_text = paste(
              "State:", state, "<br>",
              "Years of Data Archives:", years_data, "<br>")
          )
        
        # Define a custom color scale for data archives
        custom_colorscale <- list(
          list(0, light_purple), 
          list(1, dark_purple))
        
        
        # Create the choropleth map for Data Archives
        choropleth_fig <- plot_ly(
          data_map,
          type = "choropleth",
          locations = ~state_code,
          z = ~years_data,  # Number of years of data archives
          locationmode = "USA-states",
          colorscale = custom_colorscale,  
          text = ~hover_text,
          hoverinfo = "text",
          colorbar = list(
            title = "Years of Data Archives",
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2
          ),
          showscale = TRUE
        )
        
        # Create the scattergeo trace for highlighted states
        scatter_fig <- plot_ly(
          data = data_map %>% filter(highlight == 1),  
          type = "scattergeo",
          mode = "markers",  
          lat = ~lat,  
          lon = ~lon,
          marker = list(color = "red", size = 5, opacity = 0.6),  
          text = ~hover_text,
          hoverinfo = "text",
          name = "Selected State",
          hoverlabel = list(
            bgcolor = "white",    
            font = list(color = "black"),  
            bordercolor = "gray"  
          )
        )
        
        # Combine both plots using subplot()
          fig <- subplot(choropleth_fig, scatter_fig) %>%
            layout(
              title = list(text = "Data Archives by State", y = 0.95),
              margin = list(t = 80, b = 40, l = 50, r = 50),
              geo = list(
                scope = "usa",
                projection = list(type = "albers usa"),
                showgrid = FALSE,
                showframe = FALSE,
                domain = list(x = c(0, 1), y = c(0, 1))
              )
            )
        
        fig

    })

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
    
    # Map Displaying Years of Video Records per State
    output$videoMap <- renderPlotly({
   
        
        # Prepare data for video archives
        video_map <- full_data %>%
          mutate(
            state_code = state.abb[match(state, state.name)], 
            lat = state.center$y[match(state_code, state.abb)],  
            lon = state.center$x[match(state_code, state.abb)],   
            highlight = ifelse(state %in% c(input$state_1, input$state_2), 1, 0),  
            hover_text = paste(
              "State:", state, "<br>",
              "Years of Video Archives:", years_data, "<br>")
          )
        
        # Define a custom color scale for video archives
        custom_colorscale <- list(
          list(0, light_green), 
          list(1, dark_green)
        )
        
        
        # Create the choropleth map for Video Archives
        choropleth_fig <- plot_ly(
          video_map,
          type = "choropleth",
          locations = ~state_code,
          z = ~years_video,  # Number of years of video archives
          locationmode = "USA-states",
          colorscale = custom_colorscale,  
          text = ~hover_text,
          hoverinfo = "text",
          colorbar = list(
            title = "Years of Video Archives",
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2
          ),
          showscale = TRUE
        )
        
        # Create the scattergeo trace for highlighted states
        scatter_fig <- plot_ly(
          data = video_map %>% filter(highlight == 1),  
          type = "scattergeo",
          mode = "markers",  
          lat = ~lat,  
          lon = ~lon,
          marker = list(color = "red", size = 5, opacity = 0.6),  
          text = ~hover_text,
          hoverinfo = "text",
          name = "Selected State",
          hoverlabel = list(
            bgcolor = "white",    
            font = list(color = "black"),  
            bordercolor = "gray"  
          )
        )
        
        # Combine both plots using subplot()
          fig <- subplot(choropleth_fig, scatter_fig) %>%
            layout(
              title = list(text = "Video Archives by State", y = 0.95),
              margin = list(t = 80, b = 40, l = 50, r = 50),
              geo = list(
                scope = "usa",
                projection = list(type = "albers usa"),
                showgrid = FALSE,
                showframe = FALSE,
                domain = list(x = c(0, 1), y = c(0, 1))
              )
            )
        
        fig
  
    })

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

    # Map Displaying Years of Audio Records per State
    output$audioMap <- renderPlotly({

        
        # Prepare data for audio archives
        audio_map <- full_data %>%
          mutate(
            state_code = state.abb[match(state, state.name)],  
            lat = state.center$y[match(state_code, state.abb)],  
            lon = state.center$x[match(state_code, state.abb)],   
            highlight = ifelse(state %in% c(input$state_1, input$state_2), 1, 0),  
            hover_text = paste(
              "State:", state, "<br>",
              "Years of Audio Archives:", years_data, "<br>")
          )
        
        # Define a custom color scale for audio archives
        custom_colorscale <- list(
          list(0, light_orange), 
          list(1, dark_orange)
        )
        
        # Create the choropleth map (no scattergeo here)
        choropleth_fig <- plot_ly(
          audio_map,
          type = "choropleth",
          locations = ~state_code,
          z = ~years_audio,
          locationmode = "USA-states",
          colorscale = custom_colorscale,  
          text = ~hover_text,
          hoverinfo = "text",
          colorbar = list(
            title = "Years of Audio Archives",
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2
          ),
          showscale = TRUE
        )
        
        # Create scattergeo separately
        scatter_fig <- plot_ly(
          data = audio_map %>% filter(highlight == 1),
          type = "scattergeo",
          mode = "markers",  
          lat = ~lat,  
          lon = ~lon,
          marker = list(color = "red", size = 5, opacity = 0.6),  
          text = ~hover_text,
          hoverinfo = "text",
          name = "Selected State",
          hoverlabel = list(
            bgcolor = "white",    
            font = list(color = "black"),  
            bordercolor = "gray"  
          )
        )
        
        # Combine the two plots using subplot
          fig <- subplot(choropleth_fig, scatter_fig) %>%
            layout(
              title = list(text = "Audio Archives by State", y = 0.95),
              margin = list(t = 80, b = 40, l = 50, r = 50),
              geo = list(
                scope = "usa",
                projection = list(type = "albers usa"),
                showgrid = FALSE,
                showframe = FALSE,
                domain = list(x = c(0, 1), y = c(0, 1))
              )
            )
        
        fig

    })

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

  # Prepping data for comparison table, and creating reactive objects for user
  # input
  
    state_1_data <- reactive({
      full_data %>%
        filter(state == input$state_1) %>%
        mutate(state = input$state_1)
    })
    
    state_2_data <- reactive({
      full_data %>%
        filter(state == input$state_2) %>%
        mutate(state = input$state_2)
    })
  
    comparison_data <- reactive({
      # Check if states are selected
      if (is.null(input$state_1) || is.null(input$state_2) || 
          input$state_1 == "" || input$state_2 == "") {
        # Return an empty data frame with column placeholders
        return(data.frame(
          state = character(),
          years_data = character(),
          years_video = character(),
          years_audio = character(),
          agenda = character(),
          documents = character(),
          minutes = character(),
          committees = character()
        ))
      }
      
      state1 <- state_1_data()
      state2 <- state_2_data()
      
      # Process data for both states
      processed_data <- bind_rows(state1, state2) %>%
        mutate(
          agenda = ifelse(agenda == 1, "Yes", 
                          ifelse(agenda == -1, "Unclear", "No")),
          documents = ifelse(documents == 1, "Yes", 
                             ifelse(documents == -1, "Unclear", "No")),
          minutes = ifelse(minutes == 1, "Yes", 
                           ifelse(minutes == -1, "Unclear", "No")),
          # Count committees
          committees = str_count(standing_committee_names, ";") + 1 
        ) %>%
        select(state, years_data, years_video, years_audio, agenda, documents, 
               minutes, committees) %>%
        mutate(across(everything(), as.character)) #Convert all columns to char
      
      # Return processed data
      processed_data
    })
    
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
    
    # Table to Compare Data from 2 states (chosen by user)
    output$comparison_table <- renderDT({
      # Get comparison data
      data <- comparison_data()

      if (nrow(data) == 0) {
        # Placeholder table when no states are selected
        placeholder_data <- data.frame(
          State_Comparisons = "Please select two states to compare."
        )

        datatable(
          placeholder_data,
          options = list(
            dom = 't',  # Remove search, pagination, etc.

            # Center align all columns
            columnDefs = list(list(className = 'dt-center', targets = "_all"))
          ),
          rownames = FALSE
        ) %>%
          formatStyle(
            columns = "State_Comparisons",
            backgroundColor = "#ffffff",  # White background
            border = "0.3px solid #000000",  # Black border
            fontStyle = "italic",
            fontWeight = "bold",
            color = "gray"
          )
      } else {
        # Pivot data so states are rows and variables are columns
        pivoted_data <- data %>%
          pivot_longer(
            cols = -state,  # All columns except the 'state' column
            names_to = "Metric",
            values_to = "Value"
          ) %>%
          pivot_wider(names_from = "Metric", values_from = "Value")

        # Render the comparison table
        datatable(
          pivoted_data,
          options = list(
            dom = 't',  # Remove search, pagination, etc.

            # Center align all columns
            columnDefs = list(list(className = 'dt-center', targets = "_all"))
          ),
          rownames = FALSE
        ) %>%
          formatStyle(
            columns = names(pivoted_data),
            backgroundColor = "#ffffff",  # White background
            border = "0.3px solid #000000"  # Black border
          )
      }
    })
    
    
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
    
    # Processing data for state highlight table
    
    # Reactive filtering for top/bottom states
    top_states_data <- reactive({
      # Add a calculated "committees" column to the data
      processed_data <- full_data %>%
        # Calculate committee count
        mutate(committees = str_count(standing_committee_names, ";") + 1)  
      
      # Determine if showing "Most" or "Least"
      ascending <- ifelse(input$top_bottom == "Most", FALSE, TRUE)
      
      # Arrange and select the top/bottom 5 states for the chosen variable
      processed_data %>%
        arrange(if (ascending) !!sym(input$variable) else desc(!!sym(input$variable))) %>%
        select(state, .data[[input$variable]]) %>%
        head(5)
    })
    
    # Render the dynamic table
    output$top_states_table <- renderDT({
      req(input$variable)  # Ensure that the variable is selected
      
      # Process data to include the chosen variable
      sorted_data <- full_data %>%
        mutate(committees = str_count(standing_committee_names, ";") + 1) %>%  
        arrange(if (input$top_bottom == "Most") desc(.data[[input$variable]]) else .data[[input$variable]]) %>%
        select(state, .data[[input$variable]]) %>%
        head(5)  # Keep top 5 rows
      
      # Rename columns dynamically
      # Replace "Value" with the selected variable name
      colnames(sorted_data) <- c("State", input$variable)  
      
      # Render the table
      datatable(
        sorted_data,
        options = list(
          dom = 't',  # Simplify the table (no search or pagination)
          columnDefs = list(list(className = 'dt-center', targets = "_all"))  
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = names(sorted_data),
          backgroundColor = "#ffffff",  # White background
          border = "0.3px solid #000000"  # Black border
        )
    })
    
    # Reactive values for national summary
    national_summary <- reactive({
      summary_data <- full_data %>%
        mutate(committees = str_count(standing_committee_names, ";") + 1)
      
      list(
        avg_data = round(mean(summary_data$years_data, na.rm = TRUE), 1),
        avg_video = round(mean(summary_data$years_video, na.rm = TRUE), 1),
        avg_audio = round(mean(summary_data$years_audio, na.rm = TRUE), 1),
        total_committees = sum(summary_data$committees, na.rm = TRUE),
        most_data_state = 
          summary_data$state[which.max(summary_data$years_data)],
        least_data_state = 
          summary_data$state[which.min(summary_data$years_data)],
        most_committees_state = 
          summary_data$state[which.max(summary_data$committees)],
        least_committees_state = 
          summary_data$state[which.min(summary_data$committees)]
      )
    })
    
    # Render national summary as HTML
    output$national_summary <- renderUI({
      summary <- national_summary()
      HTML(paste0(
        "<h4>National Summary</h4>",
        "<p><b>Average Years of Data Archives:</b> ", summary$avg_data, "</p>",
        "<p><b>Average Years of Video Archives:</b> ", summary$avg_video, 
        "</p>",
        "<p><b>Average Years of Audio Archives:</b> ", summary$avg_audio, 
        "</p>",
        "<p><b>Total Committees Nationwide:</b> ", summary$total_committees, 
        "</p>",
        "<p><b>State with Most Data Archives:</b> ", summary$most_data_state, 
        "</p>",
        "<p><b>State with Least Data Archives:</b> ", summary$least_data_state, 
        "</p>",
        "<p><b>State with Most Committees:</b> ", summary$most_committees_state, 
        "</p>",
        "<p><b>State with Least Committees:</b> ", 
        summary$least_committees_state, "</p>"
      ))
    })
    
    
    # Defining regions
    region_data <- reactive({
      
      req(input$region)  # Ensure input$region is selected
      
      full_data %>%
        filter(state %in% unlist(regions[[input$region]])) %>%
        mutate(
          # Calculate committees
          committees = str_count(standing_committee_names, ";") + 1,  
          # Map full names to abbreviations
          state_abbreviation = state.abb[match(state, state.name)]    
        )
    })
    
    

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
    
    # Regional summary chart
    output$regional_bar_chart <- renderPlotly({
      region_data() %>%
        group_by(state_abbreviation) %>%
        summarise(avg_archives = mean(years_data, na.rm = TRUE)) %>%
        plot_ly(
          x = ~state_abbreviation, y = ~avg_archives, type = "bar",
          marker = list(color = light_green),
          name = "Average Years of Archives"
        ) %>%
        layout(
          title = list(
            text = "Regional Analysis of Data Archives",
            y = 0.9,
            x = 0.5,
            xanchor = "center",
            yanchor = "top"
          ),
          margin = list(t = 80, b = 40, l = 50, r = 50), # create padding for title
          xaxis = list(title = "State"),
          yaxis = list(title = "Average Years of Archives")
        )
    })
    
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
    
    # defining colors for regional stacked bar chart
    custom_colors <- c(
      "avg_years_data" = "#C78584",    # Light green for data
      "avg_years_video" = "#868F77",  # Light orange for video
      "avg_years_audio" = light_orange   # Light coral for audio
    )
    
    # Stacked Bar Chart showing the types of data records for each region
    output$stacked_bar_chart_regions <- renderPlotly({
      
      # filtering and aggregating data by region
      full_data %>%
        mutate(
          region = case_when(
            state %in% unlist(regions["Northeast"]) ~ "Northeast",
            state %in% unlist(regions["Midwest"]) ~ "Midwest",
            state %in% unlist(regions["South"]) ~ "South",
            state %in% unlist(regions["West"]) ~ "West",
            TRUE ~ "Unknown"
          )
        )  %>%
        ungroup() %>%
        group_by(region) %>%
        summarise(
          avg_years_data = round(mean(years_data, na.rm = TRUE), 2),  
          avg_years_video = round(mean(years_video, na.rm = TRUE), 2),  
          avg_years_audio = round(mean(years_audio, na.rm = TRUE), 2)  
        ) %>%
        filter(region != "Unknown") %>%
        pivot_longer(cols = starts_with("avg_years"), names_to = "Archive_Type", 
                     values_to = "Average_Years") %>%
        mutate(
          Archive_Type = case_when(
            Archive_Type == "avg_years_data" ~ "Data",
            Archive_Type == "avg_years_video" ~ "Video",
            Archive_Type == "avg_years_audio" ~ "Audio",
            TRUE ~ Archive_Type
          ),
          # Replace NA with 0
          Average_Years = ifelse(is.na(Average_Years), 0, Average_Years)  
        ) %>%
        plot_ly(
          x = ~region,
          y = ~Average_Years,
          color = ~Archive_Type,
          # Ensure color scale matches Archive_Type
          colors = c("Data" = "#C78584", "Video" = "#868F77", 
                     "Audio" = "#F8DBA2"),  
          type = "bar",
          hovertext = ~paste("Archive Type:", Archive_Type, 
                        "<br>Avg. Years:", Average_Years),
          hoverinfo = "text"
        ) %>%
        layout(
          barmode = "stack",
          title = "Average Years of Archives by Region",
          xaxis = list(title = "Region"),
          yaxis = list(title = "Average Years"),
          legend = list(title = list(text = "Archive Type")),
          margin = list(t = 80, b = 40, l = 50, r = 50)
        )
    })
 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------   
    
    # Full Data table for Summary tab
    output$full_data_table <- renderDT({
      datatable(
        full_data,
        options = list(
          pageLength = 10,  # Display 10 rows per page
          scrollX = TRUE    # Enable horizontal scrolling
        ),
        rownames = FALSE
      )
    })

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------    
   
    # Time Series to compare Data, Video, and Audio Archives 
    # (Not used in final dashboard)
    output$time_series_chart <- renderPlotly({
      # Assuming you have a `year` column
      time_data <- full_data %>%
        group_by(years_data) %>%
        summarise(
          avg_data = mean(years_data, na.rm = TRUE),
          avg_video = mean(years_video, na.rm = TRUE),
          avg_audio = mean(years_audio, na.rm = TRUE)
        )
      
      plot_ly(time_data, x = ~year) %>%
        add_lines(y = ~avg_data, name = "Data Archives", 
                  line = list(color = dark_purple)) %>%
        add_lines(y = ~avg_video, name = "Video Archives", 
                  line = list(color = dark_green)) %>%
        add_lines(y = ~avg_audio, name = "Audio Archives", 
                  line = list(color = dark_orange)) %>%
        layout(
          title = "Trends in Archive Availability Over Time",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Average Years of Archives"),
          margin = list(t = 80, b = 40, l = 50, r = 50)
        )
    })
    
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
}    
    
    
    
  
    

