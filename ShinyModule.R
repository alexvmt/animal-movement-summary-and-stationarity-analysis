####################
# setup
####################

# load packages
library(shiny)
library(leaflet)
library(leaflet.extras)
library(move)
library(sp)
library(pals)
library(mapview)
library(geosphere)
library(plotly)
library(DT)
library(RColorBrewer)
library(dplyr)
library(shinybusy)



####################
# user interface
####################

shinyModuleUserInterface <- function(id, label) {
  
  # create function to insert linebreaks
  linebreaks <- function(n){HTML(strrep(br(), n))}
  
  # create list with choices for date range dropdown
  choices_date_range <- list("last day" = 1,
                             "last week" = 7,
                             "last 30 days" = 30,
                             "last 60 days" = 60,
                             "last 90 days" = 90,
                             "last 180 days" = 180,
                             "last year" = 365)
  
  ns <- NS(id)
  tagList(
    titlePanel("Animal Movement Summary and Stationarity Analysis"),
    tags$style(type = "text/css", ".col-sm-9 {padding: 15px;}"), # prevent graphs from overlapping
    fluidRow(
      column(2, selectInput(ns("dropdown_individual"), "Individual:", choices = c("all"))),
      column(2, selectInput(ns("dropdown_date_range"), "Date range:", choices = choices_date_range, selected = c("last 180 days" = 180))),
      column(2, numericInput(ns("max_diameter"), "Max. diameter (m):", 100, min = 1, max = 100000)),
      column(2, numericInput(ns("min_duration"), "Min. duration (h):", 24, min = 1, max = 240)),
      column(2, linebreaks(1), downloadButton(ns("download_table"), "Download table")),
      column(2, linebreaks(1), actionButton(ns("about_button"), "Show app info"))
      ),
    fluidRow(
      column(12, dataTableOutput(ns("movement_summary")))
      ),
    fluidRow(
      column(7, checkboxInput(ns("checkbox_full_map"), "Limit map to 10 tracks", TRUE), leafletOutput(ns("map"))),
      column(5, linebreaks(2), plotlyOutput(ns("time_series")))
      )
    )
  }



####################
# server
####################

shinyModule <- function(input, output, session, data) {
  
  # show app info when about button is clicked
  observeEvent(input$about_button, {
    showModal(modalDialog(
      title = "About this app",
      HTML(
        "This app helps find stationarity in animal movement mainly through a visual analysis of animal tracking data.<br><br>
        
        It consists of three components:
        <li>statistical movement summary table</li>
        <li>map with animal tracks (and last coordinates)</li>
        <li>time series plot to help visually spot anomalous movement patterns</li><br>
        
        <b>Filters:</b>
        <li>The date range filter applies to all individuals in a given dataset and all three components in the app.
        Filtering and again aggregating data if the date range filter is changed may take a moment.</li>
        <li>Selecting an individual will not affect the movement summary table.
        Only the map and time series plot will be filtered according to the selected individual.</li><br>
        
        <b>Potential workflow:</b><br>
        A potential workflow could start by spotting a single animal of interest in either the table or the map.
        Then the data can be filtered for this specific animal and different date ranges can also be analyzed.
        A date range always refers to the last n days of each animal's tracking data time series.<br><br>
        
        <b>Notes:</b>
        <li>Aggregation happens at a daily resolution.</li>
        <li>Distances are calculated in meters (using Haversine great circle distance).</li>
        <li>The maximum date range to be analyzed are the last 365 days per individual.</li>
        <li>Please note that the app performs best with rather small datasets, containing not too many individuals.
        This is mainly because loading data and calculating distances between coordinates is computationaly intense when there are frequent location measurements (e. g. every 5 minutes).</li>
        <li>Plotting many locations for many individuals on the map also slows the app down.</li>
        <li>If the check box to limit the number of tracks on the map is checked, tracks are shown only for the first 10 individuals (selected from the tag ids in ascending order).</li>"
      )
    ))
  })
  
  # make input data reactive so that it can be returned later if unmodified
  current <- reactiveVal(data)
  
  # generate inputs for dropdowns
  observe({
    
    # show modal during data loading
    showModal(modalDialog("Loading data...", footer = NULL))
    
    # wait until the data is loaded
    if (is.null(data)) return()
    data_df <- as.data.frame(data)
    keys <- c(sort(as.character(data_df$tag.local.identifier)), "all")
    values <- c(sort(as.character(data_df$tag.local.identifier)), "all")
    key_value_list <- setNames(values, keys)
    updateSelectInput(session, "dropdown_individual", choices = key_value_list, selected = c("all" = "all"))
    
    # remove modal after data loading
    removeModal()
    
  })
  
  # ensure that max diameter is within limits
  observe({
    
    if (input$max_diameter > 100000 || input$max_diameter < 1) {
      
      showModal(
        modalDialog(
          title = strong("Warning!", style = "font-size:24px; color: red;"),
          p("Input value for max. diameter exceeds limits (min: 1; max: 100000). Reset to default.", style = "font-size:16px"),
          footer = modalButton("Close"))
        )
      
      updateNumericInput(session, "max_diameter", value = 100)
      
    }
    
  })
  
  # ensure that min duration is within limits
  observe({
    
    if (input$min_duration > 240 || input$min_duration < 1) {
      
      showModal(
        modalDialog(
          title = strong("Warning!", style = "font-size:24px; color: red;"),
          p("Input value for min. duration exceeds limits (min: 1; max: 240). Reset to default.", style = "font-size:16px"),
          footer = modalButton("Close"))
      )
      
      updateNumericInput(session, "min_duration", value = 24)
      
    }
    
  })
  
  
  
  ##### process loaded data
  rctv_data_processed <- reactive({
    
    # show modal during data processing
    show_modal_spinner(text = "Processing data and calculating distances. This may take a moment. Please wait.")
    
    # transform move object to dataframe
    data_df <- as.data.frame(data)
    
    # reset row names
    row.names(data_df) <- NULL
    
    # cast tag.local.identifier to character
    data_df$tag.local.identifier <- as.character(data_df$tag.local.identifier)
    
    # make sure right coordinates are used
    if ("coords.x1" %in% names(data_df)) {
      data_df$location.long <- data_df$coords.x1
    }
    
    if ("coords.x2" %in% names(data_df)) {
      data_df$location.lat <- data_df$coords.x2
    }
    
    # get individuals
    individuals <- unique(data_df$tag.local.identifier)
    
    # create year and date columns
    data_df$date <- as.Date(format(data_df$timestamps, format = "%Y-%m-%d"))
    data_df$year <- as.integer(format(data_df$timestamps, format = "%Y"))
    
    # create empty dataframe to store processed individual data
    data_processed <- data.frame(matrix(ncol = 5, nrow = 0))
    processed_data_columns <- c("tag.local.identifier",
                                "timestamps",
                                "location.long",
                                "location.lat",
                                "date")
    colnames(data_processed) <- processed_data_columns
    
    # set max number of last days to process
    last_n_days <- 365
    
    # process last n days of observations per individual
    for (individual in individuals) {
      
      # filter data based on individual
      individual_data <- data_df[data_df$tag.local.identifier == individual, ]
      
      # subset data to relevant columns
      individual_data <- individual_data[ , processed_data_columns]
      
      # drop rows with missing values
      individual_data <- na.omit(individual_data)
      
      # drop duplicated rows
      individual_data <- individual_data[!duplicated(individual_data[c("tag.local.identifier", "timestamps")]), ]
      
      # extract max and min date
      max_date <- max(individual_data$date)
      min_date <- max_date - last_n_days
      
      # filter data based on date range
      individual_data <- individual_data[(individual_data$date > min_date) & (individual_data$date <= max_date), ]
      
      # append processed data to existing dataframe
      data_processed <- rbind(data_processed, individual_data)
      
    }
    
    # order data
    data_processed <- data_processed[order(data_processed$tag.local.identifier, data_processed$timestamps), ]
    
    # create lag columns
    data_processed$tag.local.identifier.lag <- c(NA, head(data_processed$tag.local.identifier, -1))
    data_processed$location.long.lag <- c(NA, head(data_processed$location.long, -1))
    data_processed$location.lat.lag <- c(NA, head(data_processed$location.lat, -1))
    
    data_processed$tag.local.identifier.lag <- ifelse(data_processed$tag.local.identifier == data_processed$tag.local.identifier.lag,
                                                      data_processed$tag.local.identifier.lag,
                                                      NA)
    
    data_processed$location.long.lag <- ifelse(data_processed$tag.local.identifier == data_processed$tag.local.identifier.lag,
                                               data_processed$location.long.lag,
                                               NA)
    
    data_processed$location.lat.lag <- ifelse(data_processed$tag.local.identifier == data_processed$tag.local.identifier.lag,
                                              data_processed$location.lat.lag,
                                              NA)
    
    # create function to calculate distance between coordinates
    calculate_distance_in_meters_between_coordinates <- function(lon_a, lat_a, lon_b, lat_b) {
      if(anyNA(c(lon_a, lat_a, lon_b, lat_b))) return(NA)
      distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distVincentyEllipsoid)
    }
    
    # calculate distance between two successive coordinates
    data_processed$distance_meters_successive <- mapply(lon_a = data_processed$location.long,
                                                        lat_a = data_processed$location.lat,
                                                        lon_b = data_processed$location.long.lag,
                                                        lat_b = data_processed$location.lat.lag,
                                                        FUN = calculate_distance_in_meters_between_coordinates)
    
    # drop rows with missing distances
    data_processed <- data_processed %>% 
      filter(!is.na(distance_meters_successive))
    
    # get max date per individual
    max_dates <- data_processed %>% 
      group_by(tag.local.identifier) %>% 
      summarise(max_date = max(date))
    
    # get last timestamp and coordinates per individual
    last_timestamps_coordinates <- data_processed %>% 
      group_by(tag.local.identifier) %>% 
      arrange(timestamps) %>% 
      filter(row_number() == n()) %>% 
      rename(timestamps.last = timestamps,
             location.long.last = location.long,
             location.lat.last = location.lat) %>% 
      select(tag.local.identifier,
             timestamps.last,
             location.long.last,
             location.lat.last)
    
    # join data processed and last timestamps and coordinates
    data_processed <- data_processed %>% 
      left_join(last_timestamps_coordinates, by = "tag.local.identifier")
    
    # calculate difference between given and last timestamp
    data_processed$difference_hours_last <- as.numeric(difftime(data_processed$timestamps.last, data_processed$timestamps, units ="hours"))
    
    # calculate distance between given and last coordinates
    # only for time period within maximum of minimum duration parameter to avoid calculating unnecessary distances
    max_min_duration_parameter <- 240
    data_processed_max_time_period <- data_processed %>% 
      filter(difference_hours_last <= max_min_duration_parameter)
    
    data_processed_max_time_period$distance_meters_last <- mapply(lon_a = data_processed_max_time_period$location.long,
                                                                  lat_a = data_processed_max_time_period$location.lat,
                                                                  lon_b = data_processed_max_time_period$location.long.last,
                                                                  lat_b = data_processed_max_time_period$location.lat.last,
                                                                  FUN = calculate_distance_in_meters_between_coordinates)
    
    data_processed <- data_processed %>% 
      filter(difference_hours_last > max_min_duration_parameter) %>% 
      mutate(distance_meters_last = NA) %>% 
      rbind(data_processed_max_time_period)
    
    # drop columns that are not needed anymore
    columns_to_drop <- c("tag.local.identifier.lag",
                         "location.long.lag",
                         "location.lat.lag",
                         "timestamps.last",
                         "location.long.last",
                         "location.lat.last")
    data_processed <- subset(data_processed, select = !(names(data_processed) %in% columns_to_drop))
    
    # remove objects that are not needed anymore
    rm(data_df,
       individual_data,
       last_timestamps_coordinates,
       data_processed_max_time_period)
    
    # remove modal after data processing and notify user
    remove_modal_spinner()
    notify_success("Processing data and calculating distances complete.")
    
    list(data_processed = data_processed, max_dates = max_dates)
    
  })
  
  
  
  ##### check individuals for stationarity
  rctv_stationary_individuals <- reactive({
    
    # load reactive data
    data_processed <- rctv_data_processed()$data_processed
    
    # get max diameter
    max_diameter <- as.numeric(input$max_diameter)
    
    # get min duration
    min_duration <- as.numeric(input$min_duration)
    
    # get non-stationary individuals if there are any
    non_stationary_individuals <- data_processed %>% 
      filter((difference_hours_last <= min_duration) & (distance_meters_last > max_diameter)) %>% 
      distinct(tag.local.identifier)
    
    # get stationary individuals if there are any
    stationary_individuals <- data_processed %>% 
      distinct(tag.local.identifier) %>% 
      anti_join(non_stationary_individuals, by = "tag.local.identifier") %>% 
      mutate(stationary = "yes")
    
    stationary_individuals
    
  })
  
  
  
  ##### filter processed data
  rctv_data_processed_filtered <- reactive({
    
    # show modal during data filtering
    show_modal_spinner(text = "Filtering data according to selected date range. This may take a moment. Please wait.")
    
    # load reactive data
    data_processed <- rctv_data_processed()$data_processed
    max_dates <- rctv_data_processed()$max_dates
    
    # get last n days
    last_n_days <- as.numeric(input$dropdown_date_range)
    
    # filter data according to selected date range
    data_processed_filtered <- data_processed %>% 
      left_join(max_dates, by = "tag.local.identifier") %>% 
      filter(date >= max_date - last_n_days)
    
    # get individuals
    individuals <- unique(data_processed_filtered$tag.local.identifier)
    
    # remove modal after data filtering and notify user
    remove_modal_spinner()
    notify_success("Data filtering according to selected date range complete.")
    
    list(data_processed_filtered = data_processed_filtered, individuals = individuals)
    
  })
  
  
  
  ##### aggregate filtered processed data
  rctv_data_aggregated <- reactive({
    
    # load reactive data
    data_processed_filtered <- rctv_data_processed_filtered()$data_processed_filtered
    
    # aggregate distances by date and individual
    data_aggregated <- data_processed_filtered %>% 
      group_by(date, tag.local.identifier) %>% 
      summarise(daily_distance_meters = sum(distance_meters_successive, na.rm = TRUE),
                measures_per_date = n())
    
    # get individuals
    individuals <- unique(data_aggregated$tag.local.identifier)
    
    list(data_aggregated = data_aggregated, individuals = individuals)
    
  })
  
  
  
  ##### time series
  rctv_time_series <- reactive({
    
    # load reactive data
    data_aggregated <- rctv_data_aggregated()$data_aggregated
    
    # select individual to plot data for
    if (input$dropdown_individual == "all") {
      individual <- data_aggregated$tag.local.identifier[1]
    } else {
      individual <- input$dropdown_individual
    }
    
    # get data to plot
    data_to_plot <- data_aggregated[data_aggregated$tag.local.identifier == individual, ]
    start_date <- min(data_to_plot$date)
    end_date <- max(data_to_plot$date)

    # generate sequence of dates and fill missing dates with zero
    date_seq <- data.frame(date = seq(start_date, end_date, by = "day"))
    data_to_plot <- date_seq %>% 
      left_join(data_to_plot)
    data_to_plot[is.na(data_to_plot)] <- 0

    # set date scale
    if (dim(data_to_plot)[1] > 30) {
      scale <- "1 week"
    } else {
      scale <- "1 day"
    }
    
    # plot time series for selected individual
    p <- plot_ly(as.data.frame(data_to_plot),
                 x = ~date,
                 y = ~daily_distance_meters / 1000,
                 type = "scatter",
                 mode = "lines",
                 name = individual) %>% 
      layout(showlegend = TRUE,
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y = 1),
             title = "Do the last distances moved look anomalous to you?",
             yaxis = list(title = "Daily distance (km)"),
             xaxis = list(title = "Date"))
    
    p
    
  })
  
  output$time_series <- renderPlotly({ rctv_time_series() })
  
  
  
  ##### map
  rctv_map <- reactive({
    
    # load reactive data
    data_processed_filtered <- rctv_data_processed_filtered()$data_processed_filtered
    individuals <- rctv_data_processed_filtered()$individuals
    
    # set map colors and parameters
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]
    col_vector <- tail(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), -4)
    line_opacity <- 0.8
    line_weight <- 2
    circle_opacity <- 0.5
    circle_fill_opacity <- 0.3
    legend_opacity <- 0.6
    
    # store individual colors
    individual_colors <- col_vector[1:length(individuals)]

    # filter for individual
    if (input$dropdown_individual == "all") {
      # do nothing and proceed
    } else {
      data_processed_filtered <- data_processed_filtered[data_processed_filtered$tag.local.identifier == input$dropdown_individual, ]
    }
    
    # get remaining individual(s)
    remaining_individuals <- unique(data_processed_filtered$tag.local.identifier)
    length_remaining_individuals <- length(remaining_individuals)
    
    # limit number of shown tracks on map if needed
    track_limit <- length_remaining_individuals
    fixed_track_limit <- 10
    if (input$checkbox_full_map) {
      track_limit <- fixed_track_limit
    }

    # create map with scale, tiles and controls
    map <- leaflet() %>% 
      addTiles() %>% 
      addScaleBar(position = "topleft") %>% 
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% 
      addLayersControl(position = "topleft", baseGroups = c("StreetMap", "Aerial"),
                       overlayGroups = c("Lines", "Points"),
                       options = layersControlOptions(collapsed = FALSE))
    
    # populate map
    if (length(remaining_individuals) > 1) {

      for (i in seq(along = head(remaining_individuals, n = track_limit))) {
        
        # add lines and points
        map <- map %>% 
          addPolylines(data = data_processed_filtered[data_processed_filtered$tag.local.identifier == remaining_individuals[i], ],
                       lat = ~location.lat,
                       lng = ~location.long,
                       color = individual_colors[i],
                       opacity = line_opacity,
                       weight = line_weight,
                       group = "Lines") %>% 
          addCircles(data = data_processed_filtered[data_processed_filtered$tag.local.identifier == remaining_individuals[i], ],
                     lat = ~location.lat,
                     lng = ~location.long,
                     color = individual_colors[i],
                     opacity = circle_opacity,
                     fillOpacity = circle_fill_opacity,
                     label = ~timestamps,
                     group = "Points")
      
      }
      
      # don't show legend if map is showing more than 10 tracks
      if (track_limit <= fixed_track_limit) {

        map <- map %>% 
          addLegend(position = "topright",
                    colors = individual_colors,
                    opacity = legend_opacity,
                    labels = remaining_individuals)

      }
      
    } else {
      
      # get index of selected individual
      selected_index <- which(individuals == remaining_individuals)
      
      # get first and last lon, lat and time
      first_lon <- head(data_processed_filtered, 1)$location.long
      first_lat <- head(data_processed_filtered, 1)$location.lat
      first_time <- head(data_processed_filtered, 1)$timestamps
      
      last_lon <- tail(data_processed_filtered, 1)$location.long
      last_lat <- tail(data_processed_filtered, 1)$location.lat
      last_time <- tail(data_processed_filtered, 1)$timestamps
      
      # create start and end icons
      start_icon <- awesomeIcons(icon = "map-pin",
                                 library = "fa",
                                 markerColor = "white")
      end_icon <- awesomeIcons(icon = "map-pin",
                               library = "fa",
                               markerColor = "red")
      
      # add lines, points and markers
      map <- map %>% 
        addPolylines(data = data_processed_filtered,
                     lat = ~location.lat,
                     lng = ~location.long,
                     color = individual_colors[selected_index],
                     opacity = line_opacity,
                     weight = line_weight,
                     group = "Lines") %>% 
        addCircleMarkers(data = data_processed_filtered,
                         lat = ~location.lat,
                         lng = ~location.long,
                         color = individual_colors[selected_index],
                         opacity = circle_opacity,
                         fillOpacity = circle_fill_opacity,
                         label = ~timestamps,
                         clusterOptions = markerClusterOptions(),
                         group = "Points") %>% 
        addAwesomeMarkers(lng = first_lon,
                          lat = first_lat,
                          icon = start_icon,
                          label = paste0("First location at: ", first_time)) %>% 
        addAwesomeMarkers(lng = last_lon,
                          lat = last_lat,
                          icon = end_icon,
                          label = paste0("Last location at: ", last_time))
      
      # add legend
      map <- map %>% 
        addLegend(position = "topright",
                  colors = individual_colors[selected_index],
                  opacity = legend_opacity,
                  labels = remaining_individuals)

    }

    map
    
  })
 
  output$map <- renderLeaflet({ rctv_map() })
  
  
  
  ##### movement summary
  rctv_movement_summary <- reactive({
    
    # load reactive data
    data_aggregated <- rctv_data_aggregated()$data_aggregated
    individuals <- rctv_data_aggregated()$individuals
    stationary_individuals <- rctv_stationary_individuals()
    
    # create empty dataframe to store movement summary
    movement_summary_columns <- c("individual",
                                  "start date",
                                  "end date",
                                  "#days w measures",
                                  "#days w/o measures",
                                  "total distance (km)",
                                  "median distance (km)")
    movement_summary <- data.frame(matrix(ncol = length(movement_summary_columns), nrow = 0))
    colnames(movement_summary) <- movement_summary_columns
    
    # compute movement summary for last n days per individual
    for (individual in individuals) {
      
      # filter data based on individual
      individual_data_aggregated <- data_aggregated[data_aggregated$tag.local.identifier == individual, ]
      
      # get start and end date
      start_date <- min(individual_data_aggregated$date)
      end_date <- max(individual_data_aggregated$date)
      
      # get number of days with and without measures
      days_with_measures <- dim(individual_data_aggregated)[1]
      days_without_measures <- as.numeric(input$dropdown_date_range) - days_with_measures
      days_without_measures <- ifelse(days_without_measures < 0, 0, days_without_measures)
      
      # get total and median distance
      total_distance <- sum(individual_data_aggregated$daily_distance_meters)
      median_distance <- median(individual_data_aggregated$daily_distance_meters)

      # store values
      individual_movement_summary <- c(individual,
                                       as.character(start_date),
                                       as.character(end_date),
                                       days_with_measures,
                                       days_without_measures,
                                       round(total_distance / 1000, 2),
                                       round(median_distance / 1000, 2))
      
      # append individual movement summary to existing dataframe
      movement_summary[nrow(movement_summary) + 1, ] <- individual_movement_summary
      
    }
    
    # calculate average number of measures per day and variance
    measures_aggregated <- data_aggregated %>% 
      group_by(tag.local.identifier) %>% 
      summarise(avg_measures = round(mean(measures_per_date), 1),
                var_measures = round(var(measures_per_date), 1)) %>% 
      rename(individual = tag.local.identifier)

    # join avg and var measures
    movement_summary <- movement_summary %>% 
      left_join(measures_aggregated, by = "individual")
    colnames(movement_summary) <- c(head(colnames(movement_summary), -2),
                                    "avg. measures",
                                    "var. measures")

    # convert relevant columns to numeric
    movement_summary[ , 4:9] <- apply(movement_summary[ , 4:9], 2, as.numeric)
    
    # join stationary individuals if there are any
    if (nrow(stationary_individuals) > 0) {
      
      movement_summary <- movement_summary %>% 
        left_join(stationary_individuals, by = join_by("individual" == "tag.local.identifier")) %>% 
        mutate(stationary = ifelse(is.na(stationary), "no", stationary))
      
    } else {
      
      movement_summary <- cbind(movement_summary, stationary = "no")
      
    }
    
    movement_summary
    
  })
  
  output$movement_summary <- renderDataTable({ datatable(rctv_movement_summary()) })
  
  
  
  ##### download table
  output$download_table <- downloadHandler(
    
    filename = function(){paste0("movement_summary_last_", as.character(input$dropdown_date_range), "_days.csv")},
    content = function(fname){write.csv(rctv_movement_summary(), file = fname, row.names = FALSE)}
    
    )
  
  
  
  # return unmodified input data
  return(reactive({ current() }))

 }
