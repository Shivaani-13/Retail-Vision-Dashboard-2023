install.packages("shinythemes")
install.packages("scales")
install.packages("gganimate")
install.packages("tigris")
install.packages("networkD3")
install.packages("shinyjs")
install.packages("ggridges")
library(shinyjs)  # show/hide UI elements dynamically
library(ggridges) # used for ridgeplots 
library(networkD3) # used for sankey diagram
library(tigris) # U.S. state boundary files
library(tidyverse) 
library(forecast)  
library(gganimate) # used for annimation
library(lubridate) # date manipulation
library(shinythemes) # dashboard themes
library(scales) # axis formatting
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet) # interactive maps
library(sf) # handles spatial bdatas
library(maps) # map data
library(gifski) # renders animations as GIF files

###...........................................................................###

shopping <- read.csv("shopping_behavior_with_timestamp.csv", stringsAsFactors = FALSE)

shopping$Purchase_Date <- as.Date(shopping$Purchase_Date, format = "%m/%d/%Y")

shopping$Season <- factor(trimws(shopping$Season), levels = c("Spring", "Summer", "Fall", "Winter"))

# Hawaii, Puerto Rico and DC are removed to keep the map clean
states_sf_base <- tigris::states(cb = TRUE, resolution = "20m", progress_bar = FALSE) %>%
  dplyr::filter(!NAME %in% c("Hawaii", "Puerto Rico", "District of Columbia")) %>%
  tigris::shift_geometry() %>%
  sf::st_transform(4326) %>%
  dplyr::rename(State_Name = NAME)

# Assigning unique colour to each U.S. state for the map
state_colors <- list(
  "Alabama" = "#2ecc71", "Alaska" = "#76d7c4", "Arizona" = "#85c1e9", "Arkansas" = "#f39c12", 
  "California" = "#af7ac5", "Colorado" = "#27ae60", "Connecticut" = "#5dade2", "Delaware" = "#a93226", 
  "Florida" = "#f4d03f", "Georgia" = "#ec7063", "Idaho" = "#f7dc6f",
  "Illinois" = "#f9e79f", "Indiana" = "#d7bde2", "Iowa" = "#7fb3d5", "Kansas" = "#f5cba7", 
  "Kentucky" = "#48c9b0", "Louisiana" = "#a569bd", "Maine" = "#f0b27a", "Maryland" = "#cb4335", 
  "Massachusetts" = "#f1948a", "Michigan" = "#28b463", "Minnesota" = "#f5b041", "Mississippi" = "#f7dc6f",
  "Missouri" = "#85c1e9", "Montana" = "#aed6f1", "Nebraska" = "#e74c3c", "Nevada" = "#fad7a0", 
  "New Hampshire" = "#5dade2", "New Jersey" = "#17202a", "New Mexico" = "#f1c40f", "New York" = "#e74c3c", 
  "North Carolina" = "#3498db", "North Dakota" = "#6c3483", "Ohio" = "#f39c12", "Oklahoma" = "#a04000",
  "Oregon" = "#a97142", "Pennsylvania" = "#d4ac0d", "Rhode Island" = "#7d3c98", "South Carolina" = "#5dade2", 
  "South Dakota" = "#85c1e9", "Tennessee" = "#eb984e", "Texas" = "#9a9f3a", "Utah" = "#e67e22", 
  "Vermont" = "#58d68d", "Virginia" = "#d68910", "Washington" = "#27ae60", "West Virginia" = "#f4d03f",
  "Wisconsin" = "#a569bd", "Wyoming" = "#d2b4de"
)
color_lookup <- data.frame(Location = names(state_colors), State_Color = unlist(state_colors), stringsAsFactors = FALSE)

# These are the Colours used to mark the top 5 ranked states
rank_marker_colors <- c("red", "orange", "green", "blue", "purple")

###..........................................................................###

# --- UI ---
ui <- navbarPage(
  useShinyjs(),
  title = div("Shopping analysis"),
  id = "main_nav",
  
  # Custom CSS styling applied across all tabs each tab has its own colour theme
  header = tags$head(
    tags$style(HTML("
    /* GLOBAL NAVBAR */
    .navbar-default { background-color: #2E4D2E !important; border: none; }
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color: white !important; font-weight: bold; }
    
    /* TAB 1: MAP TAB STYLING (GREEN THEME) */
    #map_content { background-color: #F1F8E9 !important; min-height: 100vh; padding: 20px; }
    #map_content .sub-title { color: #689F38; font-size: 16px; margin-bottom: 25px; font-weight: 800; }
    #map_content .well { background-color: #DCEDC8 !important; border: 1px solid #AED581 !important; border-radius: 15px !important; }
    
    /* TAB 2: INSIGHTS TAB STYLING (ORCHID THEME) */
    #insights_content { background-color: #F7F3FF !important; min-height: 100vh; padding: 20px; }
    #insights_content .sub-title { color: #6A1B9A; font-size: 16px; margin-bottom: 25px; font-weight: 800; }
    #insights_content .well { background-color: #EEE3FF !important; border: 1px solid #D1C4E9 !important; border-radius: 15px !important; }
    
    /* COMMON ELEMENTS */
    .main-title { color: #2E4D2E; font-weight: 800; font-size: 38px; margin-top: 10px; }
    .plot-card { background: white; padding: 25px; border-radius: 15px; box-shadow: 0 4px 20px rgba(0,0,0,0.06); margin-bottom: 20px; border: 1px solid #C5E1A5; }
    .interpretation-box { background: #F3E5F5; padding: 15px; border-left: 5px solid #7B1FA2; margin-top: 10px; border-radius: 4px; color: #4A148C; }
    
    /* TIME SERIES TAB STYLING (BEIGE THEME) */
    #timeseries_content {background-color: #F5F0E6 !important;min-height: 100vh;padding: 20px;}
    #timeseries_content .sub-title {font-weight: 800;color: #8B6F3D;font-size: 16px;}
    
    #proxy_content {background-color: #FFF0F0 !important;min-height: 100vh;padding: 20px;}
    #proxy_content .sub-title {font-weight: 800;color: #E85C5C;font-size: 16px;}
    
    #journey_content {background-color: #EAF3FF !important;  /* light blue page */min-height: 100vh;padding: 20px;}
    #journey_content .sub-title {font-weight: 800;font-size: 16px;color: #1F3A5F;  /* dark blue text */}
    #journey_content .plot-card {max-width: 1200px; /* pick a width that fits your layout */margin: 0 auto; /* center it */width: 100%;overflow-x: hidden;/* prevent spill */}
      
    
    /* TAB 6: STYLE PULSE STYLING (TEAL THEME) */
    #style_pulse_content {background-color: #E0F2F1 !important;min-height: 100vh;padding: 20px;}
    #style_pulse_content .sub-title {font-weight: 800;color: #00796B;font-size: 16px;margin-bottom: 25px;}

    /* Fix plot container rounding / clipping */
    .plot-card {border-radius: 0 !important;overflow: hidden !important;background: white;}
    "))
  ),
  
  # Home tab - landing page with a Launch button that takes users to the Revenue Map
  tabPanel("Home",
           
           div(style = "height: calc(100vh - 50px);width: 100%;margin: 0;padding: 0;background: linear-gradient(135deg,#d4bdf0,#b8d7f5,#c2ede2,#f4e3a3,#f3b7ce);
           display: flex;align-items: center;justify-content: center;",
               
               div(style = "background: rgba(255, 255, 255, 0.18);backdrop-filter: blur(14px);-webkit-backdrop-filter: blur(14px);border: 1px solid rgba(255,255,255,0.3);box-shadow: 0 25px 60px rgba(0,0,0,0.12);
        padding: 60px;border-radius: 25px; text-align: center;max-width: 900px;",
                   
                   h1(
                     HTML("Retail Vision Dashboard <br>
              <span style='font-size:36px;'>2023</span>"),
                     style = "font-size: 48px;font-weight: 800;margin-bottom: 25px;background: linear-gradient(90deg, #2c3e50, #34495e, #3a6073);
                -webkit-background-clip: text;-webkit-text-fill-color: transparent;text-align: center;"),
                   
                   actionButton("enter_dashboard", "Launch",
                                style = "background-color: rgba(255, 255, 255, 0.35);color: #2f3e46;border: 1px solid rgba(255,255,255,0.6);
                            font-weight: 600;font-size: 18px;padding: 15px 30px;border-radius: 12px;box-shadow: 0 8px 20px rgba(0,0,0,0.12);"
                   )
               )
           )
  ),
  
  # Revenue Map tab - interactive choropleth map showing retail revenue by state
  tabPanel("Revenue Map",
           div(id = "map_content",
               fluidRow(
                 column(12, div(class = "main-title", "🍀The Geo-Revenue Lens"),
                        div(class = "sub-title", "Analyzing Retail Growth Using Geographic Insights"))
               ),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              h4("Dashboard Controls", style="font-weight: bold; color: #33691E;"),
                              selectInput("CatMap", "Category", choices = unique(shopping$Category)),
                              radioButtons("viewType",
                                           "Revenue View:",
                                           choices = c("Seasonal", "Annual"),
                                           selected = "Seasonal",
                                           inline = TRUE),
                              sliderInput("SeasonSlider", "Seasonal Revenue Cycle:", min = 1, max = 4, value = 1, step = 1,
                                          animate = animationOptions(interval = 2200, loop = TRUE), ticks = FALSE),
                              helpText("1: Spring | 2: Summer | 3: Fall | 4: Winter", style="font-size: 11px; color: black;"),
                              hr(),
                              h5("Top Performing States", style="font-weight: bold; color: black;"),
                              tableOutput("TopStatesTable")
                 ),
                 mainPanel(width = 9,
                           div(class = "plot-card", leafletOutput("Map2", height = "600px"))
                 )
               )
           )
  ),
  
  # Customer Insights tab - ridgeline plot showing spending by age and gender
  tabPanel("Customer Insights",
           div(id = "insights_content",
               fluidRow(
                 column(12, 
                        div(class = "main-title", "👩‍🤝‍👨Consumer Demographics"),
                        div(class = "sub-title", "Spending Behavior by Age and Gender")
                 )
               ),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              h4("Demographic Filters", style="font-weight: bold; color: #4A148C;"),
                              selectInput("CatDemo", "Category", choices = unique(shopping$Category)),
                              selectInput("GenderFilter", "Gender", 
                                          choices = c("Both", "Male", "Female"), 
                                          selected = "Both"),
                              sliderInput("AgeDemo", "Age Range", 
                                          min = 18, max = 72, value = c(18, 72)),
                              hr()
                 ),
                 mainPanel(width = 9,
                           div(class = "plot-card", 
                               h4("Spending Distribution", 
                                  style="font-weight: bold;"),
                               plotOutput("PlotRidge", height = "500px"),
                               uiOutput("RidgeText")
                           )
                 )
               )
           )
  ),
  
  # Business Outlook tab - time series plot with 2024 revenue forecast
  tabPanel("Business Outlook",
           div(id = "timeseries_content",
               fluidPage(
                 fluidRow(
                   fluidRow(
                     column(12,
                            div(class = "main-title", "📈Revenue Over Time"),
                            div(class = "sub-title", "Revenue Trends with 2024 Forecast")
                     )
                   ),
                   fluidRow(
                     column(12,
                            div(class = "plot-card",
                                imageOutput("TimeSeriesAnim", height = "600px")
                            )
                     )
                   )
                 )
               )
           )
  ),
  
  # Obesity Proxy tab - lollipop chart ranking states by L/XL apparel purchases
  tabPanel("Obesity Proxy",
           div(id = "proxy_content",
               fluidRow(
                 column(12,
                        div(class = "main-title", "📍Obesity Proxy by State"),
                        div(class = "sub-title",
                            "Estimated Obesity Levels Based on Apparel Size Distribution")
                 )
               ),
               sidebarLayout(
                 sidebarPanel(
                   h4("Filters", style="font-weight:bold;"),
                   
                   radioButtons("proxyGender",
                                "Gender:",
                                choices = c("Both","Male","Female"),
                                selected = "Both",
                                inline = TRUE),
                   
                   sliderInput("proxyAge",
                               "Age Range:",
                               min = 18, max = 72,
                               value = c(18,72))
                 ),
                 
                 mainPanel(
                   div(class = "plot-card",
                       plotOutput("LollipopPlot", height = "700px")
                   )
                 )
               )
           )
  ),
  
  # Customer Journey tab - Sankey diagram showing the 5 stage customer pathway
  tabPanel("Customer Journey",
           div(id = "journey_content",
               fluidPage(
                 fluidRow(
                   column(12,
                          div(class = "main-title", "👣 Customer Pathway Analysis"),
                          div(class = "sub-title", "5-Stage Flow: From Gender to Payment Method")
                   )
                 ),
                 fluidRow(
                   column(12,
                          div(class = "plot-card",
                              sankeyNetworkOutput("JourneySankey", height = "600px", width = "100%"),
                              
                              div(
                                style = "margin-top:15px; font-size:14px; color:#555;",
                                "Note: Flow thickness represents the number of customers transitioning between stages. 
                Hover over flows to view exact counts."
                              )
                          )
                   )
                 )
               )
           )
  ),
  
  # Seasonal Trends tab - bubble chart showing product performance across seasons
  tabPanel("Seasonal Trends",
           div(id = "style_pulse_content",
               fluidPage(
                 fluidRow(
                   column(12, 
                          div(class = "main-title","🎨Seasonal Style Pulse"),
                          div(class = "sub-title", "Tracking Color Popularity & Top Items through the 2023 Retail Year")
                   )
                 ),
                 fluidRow(
                   column(12,
                          div(class = "plot-card",
                              imageOutput("StylePulseAnim", height = "600px")
                          )
                   )
                 )
               )
           )
  )
)

###..........................................................................###

# --- SERVER ---
server <- function(input, output, session) {
  
  # When the Launch button is clicked, it navigates to the Revenue Map tab
  observeEvent(input$enter_dashboard, {
    updateNavbarPage(session, "main_nav", selected = "Revenue Map")
  })
  
  # Hides the season slider when Annual view is selected in the Revenue Map
  observe({
    if (input$viewType == "Annual") {
      hide("SeasonSlider")
    } else {
      show("SeasonSlider")
    }
  })
  
  # Hide season slider when Annual selected
  observe({
    if (input$viewType == "Annual") {
      hide("SeasonSlider")
    } else {
      show("SeasonSlider")
    }
  })
  
  # this is used to filter the shopping data based on category and season selection
  geo_filtered <- reactive({
    
    if (input$viewType == "Seasonal") {
      
      season_name <- switch(as.character(input$SeasonSlider),
                            "1"="Spring",
                            "2"="Summer",
                            "3"="Fall",
                            "4"="Winter")
      
      shopping %>%
        filter(Category == input$CatMap,
               Season == season_name)
      
    } else {
      
      shopping %>%
        filter(Category == input$CatMap)
    }
  })
  
  # Building an interactive choropleth map using Leaflet
  output$Map2 <- renderLeaflet({
    
    # Calculating total revenue per state and assign rankings
    rev_data <- geo_filtered() %>%
      group_by(Location) %>%
      summarise(total = sum(Purchase.Amount..USD., na.rm = TRUE),
                .groups = "drop") %>%
      arrange(desc(total)) %>%
      mutate(Rank = row_number())
    
    # Merge revenue data with state boundary geometry
    map_df <- states_sf_base %>%
      left_join(rev_data, by = c("State_Name" = "Location")) %>%
      left_join(color_lookup, by = c("State_Name" = "Location"))
    
    # incase of any missing revenue values replacing it with 0
    map_df$total[is.na(map_df$total)] <- 0
    
    # Getting the centre coordinates of each state for label placement
    centers <- st_coordinates(st_centroid(st_geometry(map_df)))
    map_df$lng <- centers[,1]
    map_df$lat <- centers[,2]
    
    leaflet(map_df) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      
      #Filling each states with its assigned colours and show revenue on hover
      addPolygons(
        fillColor = ~State_Color,
        weight = 1.2,
        color = "white",
        fillOpacity = 0.75,
        label = ~paste0(State_Name, ": $",format(total, big.mark=","))
      ) %>%
      
      addLabelOnlyMarkers(
        lng = ~lng,
        lat = ~lat,
        label = ~State_Name,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = 'center',
          textOnly = TRUE,
          style = list(
            "font-weight" = "bold",
            "font-size" = "9px",
            "color" = "#2C3E50"
          )
        )
      ) %>%
      
      # Adding star markers for the top 5 revenue states
      addAwesomeMarkers(
        data = map_df %>% filter(Rank <= 5),
        lng = ~lng,
        lat = ~lat,
        popup = ~paste0("<b>", State_Name, "</b><br>",
                        "Revenue: $", format(total, big.mark=","),
                        "<br>Rank: ", Rank),
        icon = ~awesomeIcons(
          icon = 'ios-star',
          markerColor = rank_marker_colors[Rank],
          library = 'ion'
        )
      ) %>%
      
      # Adding a ranking legend in the bottom right corner
      addControl(
        html = "<div style='background: white; padding: 12px; border-radius: 10px;
              box-shadow: 0 0 15px rgba(0,0,0,0.2); line-height: 1.8;'>
              <b style='color:#33691E; font-size: 14px; display:block; margin-bottom:5px;'>
              Revenue Rank</b>
              <span><b style='color:red'>★</b> 1st Place</span><br>
              <span><b style='color:orange'>★</b> 2nd Place</span><br>
              <span><b style='color:green'>★</b> 3rd Place</span><br>
              <span><b style='color:blue'>★</b> 4th Place</span><br>
              <span><b style='color:purple'>★</b> 5th Place</span>
              </div>",
        position = "bottomright"
      )
  })
  
  # Shows the top 5 states by revenue in the summary table
  output$TopStatesTable <- renderTable({
    geo_filtered() %>%
      group_by(Location) %>%
      summarise(Total_Revenue = sum(Purchase.Amount..USD., na.rm = TRUE),
                .groups = "drop") %>%
      arrange(desc(Total_Revenue)) %>%
      head(5)
  })
  
  
  output$TopStatesTable <- renderTable({
    geo_filtered() %>% group_by(Location) %>% summarise(Revenue = sum(Purchase.Amount..USD.)) %>%
      arrange(desc(Revenue)) %>% head(5) %>% mutate(Revenue = paste0("$", format(Revenue, big.mark=",")))
  })
  
  
  demo_data <- reactive({
    
    df <- shopping %>%
      filter(
        Category == input$CatDemo,
        Age >= input$AgeDemo[1],
        Age <= input$AgeDemo[2]
      )
    
    if (input$GenderFilter != "Both") {
      df <- df %>% filter(Gender == input$GenderFilter)
    }
    
    df
  })
  
  # Draw the ridgeline density plot for gender spending comparison
  output$PlotRidge <- renderPlot({
    
    df <- demo_data()
    
    req(nrow(df) > 0)
    
    ggplot(df, 
           aes(x = Purchase.Amount..USD., 
               y = Gender, 
               fill = Gender)) +
      
      geom_density_ridges(
        alpha = 0.8,
        scale = 1.2,
        rel_min_height = 0.01,
        color = "white"
      ) +
      
      scale_fill_manual(values = c(
        "Female" = "#FFC0CB",
        "Male"   = "#AED6F1"
      )) +
      
      labs(
        x = "Purchase Amount (USD)",
        y = "Gender"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Calculate and display spending peak values below the ridgeline plot
  output$RidgeText <- renderUI({
    
    df <- demo_data()
    
    if (nrow(df) < 5) return(NULL)
    
    # Find the peak spending point for each gender using density estimation
    m_data <- df$Purchase.Amount..USD.[df$Gender == "Male"]
    f_data <- df$Purchase.Amount..USD.[df$Gender == "Female"]
    
    m_peak <- if (length(m_data) > 2)
      round(density(m_data)$x[which.max(density(m_data)$y)], 0)
    else "N/A"
    
    f_peak <- if (length(f_data) > 2)
      round(density(f_data)$x[which.max(density(f_data)$y)], 0)
    else "N/A"
    
    avg_val <- round(mean(df$Purchase.Amount..USD.), 2)
    
    div(class="interpretation-box",
        HTML(paste0(
          "The height of each ridge shows where spending is most concentrated. ",
          "Overall average spend is <b>$", avg_val, "</b>. ",
          "<br><br>",
          "<b>Spending Peaks:</b><br>",
          "Male peak: <b>$", m_peak, "</b><br>",
          "Female peak: <b>$", f_peak, "</b><br><br>",
          "Higher ridges indicate stronger customer clustering at that price level."
        ))
    )
  })
  
  # Building an animated time series plot with 2024 forecast
  output$TimeSeriesAnim <- renderImage({
    
    set.seed(123)
    
    # Aggregating monthly revenue by season
    ts_data <- shopping %>%
      mutate(
        Month = floor_date(Purchase.Date, "month"),
        MonthNum = month(Purchase.Date),
        Season = case_when(
          MonthNum %in% c(3,4,5)  ~ "Spring",
          MonthNum %in% c(6,7,8)  ~ "Summer",
          MonthNum %in% c(9,10,11) ~ "Fall",
          MonthNum %in% c(12,1,2) ~ "Winter"
        )
      ) %>%
      group_by(Month, Season) %>%
      summarise(TotalRevenue = sum(Purchase.Amount..USD.), .groups = "drop") %>%
      arrange(Month)
    
    ts_data$Season <- factor(ts_data$Season, levels = c("Spring", "Summer", "Fall", "Winter"))
    ts_data$Type <- "Actual"
    
    # Generating a 12 month forecast for each season and Added few noise and a slight upward trend to make it look more realistic
    forecasts <- ts_data %>%
      group_by(Season) %>%
      group_modify(~{
        hist <- .x
        
        pattern <- hist$TotalRevenue
        base <- rep(pattern, length.out = 12)
        
        noise <- rnorm(12, mean = 0, sd = sd(base) * 0.05)
        
        trend <- seq(0, mean(base) * 0.03, length.out = 12)
        
        raw_forecast <- base + noise + trend
        
        # Smoothing the forecast to remove sharp jumps
        smooth_forecast <- stats::filter(raw_forecast, rep(1/3, 3), sides = 2)
        smooth_forecast <- as.numeric(smooth_forecast)
        smooth_forecast[is.na(smooth_forecast)] <- raw_forecast[is.na(smooth_forecast)]
        
        tibble(
          Month = seq(from = max(hist$Month) + months(1), by = "1 month", length.out = 12),
          Season = unique(hist$Season),
          TotalRevenue = smooth_forecast,
          Type = "Forecast"
        )
      })
    
    # by combining the actual and forecasted data then animate the line chart
    plot_data <- bind_rows(ts_data, forecasts)
    
    # 3) Plot
    p <- ggplot(plot_data, aes(x = Month, y = TotalRevenue, color = Season, group = Season)) +
      geom_line(data = subset(plot_data, Type == "Actual"), linewidth = 1.2) +
      geom_line(data = subset(plot_data, Type == "Forecast"), linewidth = 1.2, linetype = "dashed") +
      geom_point(data = subset(plot_data, Type == "Actual"), size = 2) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_color_manual(
        values = c(
          "Spring" = "#E74C3C",
          "Summer" = "#27AE60",
          "Fall"   = "#2980B9",
          "Winter" = "#8E44AD"
        )
      ) +
      labs(
        #title = "Revenue Trends with 2024 Forecast",
        x = "Date",
        y = "Total Revenue (USD)",
        color = "Season"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold"),
        axis.text.y  = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text  = element_text(face = "bold"),
        legend.background = element_rect(fill = "#F5F0E6", color = "#8B6F3D", linewidth = 0.8),
        legend.box.background = element_rect(fill = "#F5F0E6", color = "#8B6F3D", linewidth = 0.8)
      )+
      transition_reveal(Month)
    
    
    # Save and return the animation as a GIF file
    outfile <- tempfile(fileext = ".gif")
    
    animate(
      p,
      fps = 10,
      width = 1300,
      height = 550,
      renderer = gifski_renderer(outfile)
    )
    
    list(
      src = outfile,
      contentType = "image/gif"
    )
    
  }, deleteFile = TRUE)
  
  # Filtering clothing data(L/XL) for obesity proxy based on age and gender
  obesity_data <- reactive({
    
    df <- shopping %>%
      filter(Category == "Clothing") %>%
      mutate(IsLarge = ifelse(Size %in% c("L","XL"), 1, 0)) %>%
      filter(
        Age >= input$proxyAge[1],
        Age <= input$proxyAge[2]
      )
    
    if (input$proxyGender != "Both") {
      df <- df %>% filter(Gender == input$proxyGender)
    }
    
    # Calculatig the percentage of L/XL purchases per state
    df %>%
      group_by(Location) %>%
      summarise(
        ObesityProxy = ifelse(n() > 0, mean(IsLarge) * 100, NA),
        .groups = "drop"
      ) %>%
      drop_na(ObesityProxy) %>%
      arrange(ObesityProxy)
  })
  
  # Displaying through the lollipop chart, ranking states by L/XL purchase percentage
  output$LollipopPlot <- renderPlot({
    
    data <- obesity_data()
    req(nrow(data) > 0)
    
    # Highlight top 5 in red, bottom 5 in blue, rest in grey(Low to High)
    data <- data %>%
      arrange(ObesityProxy) %>%
      mutate(
        Location = factor(Location, levels = Location),
        Rank = row_number(),
        Highlight = case_when(
          Rank <= 5 ~ "Bottom 5",
          Rank > n() - 5 ~ "Top 5",
          TRUE ~ "Middle"
        ),
        Highlight = factor(Highlight,
                           levels = c("Top 5", "Middle", "Bottom 5"))
      )
    
    # dashed-line National Average
    national_avg <- mean(data$ObesityProxy, na.rm = TRUE)
    
    ggplot(data,
           aes(x = Location,
               y = ObesityProxy,
               color = Highlight)) +
      
      geom_segment(aes(xend = Location,
                       y = 0,
                       yend = ObesityProxy),
                   linewidth = 1.2,
                   color = "grey85") +
      
      geom_point(size = 4) +
      
      geom_hline(yintercept = national_avg,
                 linetype = "dashed",
                 color = "black",
                 linewidth = 1) +
      
      annotate("text",
               x = 1,
               y = national_avg + 2,
               label = paste0("National Avg: ",
                              round(national_avg,1), "%"),
               hjust = 0,
               fontface = "bold",
               size = 4) +
      
      scale_color_manual(
        values = c(
          "Top 5" = "#B30000",
          "Middle" = "#9E9E9E",
          "Bottom 5" = "#2C7BB6"
        ),
        name = "Relative Ranking"
      ) +
      
      coord_flip() +
      
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.05)),
        labels = scales::percent_format(scale = 1)
      ) +
      
      labs(
        title = "Obesity Proxy Ranking by State",
        subtitle = "Top 5 and Bottom 5 highlighted | Dashed line = National Average",
        x = "",
        y = "L/XL Percentage"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 18)
      )
  })
  
  #Building a Sankey diagram to show the 5 stage customer journey
  output$JourneySankey <- renderSankeyNetwork({
    
    sankey_data <- shopping %>%
      mutate(
        Discount = ifelse(Discount.Applied == TRUE | Discount.Applied == "Yes",
                          "Discount", "No Discount"),
        Subscription = ifelse(Subscription.Status == TRUE | Subscription.Status == "Yes",
                              "Subscribed", "Not Subscribed")
      )
    
    # Create links between each stage of the customer journey
    links1 <- sankey_data %>%
      count(Gender, Frequency.of.Purchases, name = "value") %>%
      rename(source = Gender, target = Frequency.of.Purchases)
    
    links2 <- sankey_data %>%
      count(Discount, Subscription, name = "value") %>%
      rename(source = Discount, target = Subscription)
    
    links3 <- sankey_data %>%
      count(Frequency.of.Purchases, Discount, name = "value") %>%
      rename(source = Frequency.of.Purchases, target = Discount)
    
    links4 <- sankey_data %>%
      count(Subscription, Payment.Method, name = "value") %>%
      rename(source = Subscription, target = Payment.Method)
    
    links <- bind_rows(links1, links2, links3, links4)
    
    # Converting node names to numeric IDs as required for sankey
    nodes <- data.frame(
      name = unique(c(links$source, links$target))
    )
    
    links$source_id <- match(links$source, nodes$name) - 1
    links$target_id <- match(links$target, nodes$name) - 1
    
    sankey <- sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "source_id",
      Target = "target_id",
      Value  = "value",
      NodeID = "name",
      fontSize = 12,
      nodeWidth = 25,
      nodePadding = 25
    )
    
    #small JavaScript snippet to style the Sankey flow links it reduces opacity for a cleaner look
    htmlwidgets::onRender(
      sankey,
      '
    function(el) {
      d3.select(el).selectAll(".link")
        .style("stroke-opacity", 0.2)
        .style("stroke", "#C8C8C8");
    }
    '
    )
    
  })
  
  # Building animated bubble chart for seasonal product performance
  output$StylePulseAnim <- renderImage({
    
    color_map <- c(
      "Gray" = "gray40",
      "Maroon" = "maroon",
      "Turquoise" = "turquoise3",
      "White" = "gray90",
      "Charcoal" = "darkslategray",
      "Silver" = "gray70",
      "Teal" = "#008080",
      "Yellow" = "goldenrod",
      "Green" = "forestgreen",
      "Pink" = "hotpink",
      "Purple" = "purple",
      "Cyan" = "cyan3",
      "Orange" = "orange2",
      "Blue" = "blue3",
      "Brown" = "brown",
      "Magenta" = "magenta3",
      "Olive" = "olivedrab",
      "Beige" = "tan3",
      "Black" = "black",
      "Lavender" = "mediumpurple1",
      "Violet" = "darkorchid3",
      "Indigo" = "#4B0082",
      "Gold" = "goldenrod2",
      "Peach" = "peachpuff3",
      "Red" = "red3"
    )
    
    #Aggregating average age, spend and sales count by season and colour
    pulse_data <- shopping %>%
      group_by(Season, Color) %>%
      summarise(
        Avg_Age = mean(Age, na.rm = TRUE),
        Avg_Spend = mean(Purchase.Amount..USD., na.rm = TRUE),
        Sales_Count = n(),
        Top_Item = names(which.max(table(Item.Purchased))),
        .groups = "drop"
      ) %>%
      group_by(Season) %>%
      mutate(
        Season_Revenue = sum(Sales_Count * Avg_Spend),
        Season_Label = paste0(
          "Total Season Revenue: $",
          format(round(Season_Revenue), big.mark = ",")
        )
      ) %>%
      ungroup()
    
    pulse_data$Season <- factor(pulse_data$Season, levels = c("Spring", "Summer", "Fall", "Winter"))
    
    
    # the bubble colour matches the actual item colour purchased by customers
    p <- ggplot(pulse_data, aes(x = Avg_Age, y = Avg_Spend, size = Sales_Count, color = Color)) +
      
      # Giant "2023" watermark in the CENTER of the view
      annotate(
        "text",
        x = mean(c(35, 55)),
        y = mean(c(50, 70)),
        label = "2023",
        size = 60,
        alpha = 0.07,
        fontface = "bold",
        color = "black"
      ) +
      
      geom_point(alpha = 0.7, stroke = 1) +
      
      geom_text(
        aes(label = Top_Item),
        size = 3.5,
        color = "black",
        fontface = "bold",
        vjust = -1.2,
        check_overlap = TRUE
      ) +
      
      geom_text(
        data = dplyr::distinct(pulse_data, Season, Season_Label),
        aes(
          x = 55,
          y = 70,
          label = Season_Label
        ),
        hjust = 1.05, vjust = 1.5,
        inherit.aes = FALSE,
        size = 5.5,
        fontface = "bold",
        color = "#00796B"
      ) +
      
      scale_color_manual(values = color_map) +
      scale_size_continuous(range = c(8, 30)) +
      
      # Axis ticks
      scale_x_continuous(breaks = seq(35, 55, by = 5)) +
      scale_y_continuous(breaks = seq(50, 70, by = 5)) +
      
      coord_cartesian(xlim = c(35, 55), ylim = c(50, 70), clip = "on") +
      
      labs(
        title = '2023 Trend Pulse: {closest_state}',
        subtitle = "Bubble size represents sales volume",
        x = "Average Age of Buyer",
        y = "Average Purchase Amount (USD)",
        size = "Sales Volume"
      ) +
      
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 22, color = "#00796B"),
        plot.subtitle = element_text(size = 12, color = "#555555", margin = margin(b = 20)),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      
      # Animation
      transition_states(Season, transition_length = 2, state_length = 4) +
      
      
      ease_aes('cubic-in-out')
    
    #Save and return the animation as a GIF
    outfile <- tempfile(fileext = ".gif")
    animate(p, fps = 10, width = 1100, height = 600, renderer = gifski_renderer(outfile))
    
    list(src = outfile, contentType = "image/gif")
    
  }, deleteFile = TRUE)
}  # end server

shinyApp(ui, server)




