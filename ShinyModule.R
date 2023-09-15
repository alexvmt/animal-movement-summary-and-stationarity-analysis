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
  ns <- NS(id)
  tagList(
    titlePanel("Animal Movement Summary and Stationarity Analysis"),
    tags$style(type = "text/css", ".col-sm-9 {padding: 15px;}"), # prevent graphs from overlapping
    fluidRow(
      column(2,
             selectInput(ns("dropdown_individual"),
                         "Individual:",
                         choices = c("all")),
             selectInput(ns("dropdown_date_range"),
                         "Date range:",
                         choices = list("last day" = 1,
                                        "last week" = 7,
                                        "last 30 days" = 30,
                                        "last 60 days" = 60,
                                        "last 90 days" = 90,
                                        "last 180 days" = 180,
                                        "last year" = 365,
                                        "all time" = 99999),
                         selected = c("last 180 days" = 180)),
	     checkboxInput(ns("checkbox_full_map"), "Limit map to 10 tracks", TRUE),
             actionButton(ns("about_button"), "Show app info")),
      column(10, dataTableOutput(ns("movement_summary")))
    ),
    fluidRow(
      column(2),
      column(6, leafletOutput(ns("map"))),
      column(4, plotlyOutput(ns("time_series")))
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
        The data processing loop and distance calculations are executed again if the date range filter is changed, which may take a moment.</li>
        <li>Selecting an individual will not affect the movement summary table.
        Only the map and time series plot will be filtered.</li>
        <li>Data is aggregated by day and distances are calculated in meters (using Haversine great circle distance).</li><br>
        
        <b>Potential workflow:</b><br>
        A potential workflow could start by spotting a single animal of interest in either the movement summary table or the map.
        Then the data can be filtered for this specific animal and also different date ranges can be analyzed.
        A date range always refers to the last n days of each given animal's tracking data time series.<br><br>
        
        <b>Notes:</b>
        <li>Please note that the app performs best with rather small datasets, containing not too many individuals.
        This is mainly because calculating distances between coordinates is computationaly intense when there are many measurements (e. g. every 5 minutes).</li>
        <li>If the check box to limit the number of tracks on the map is checked, tracks are shown only for the first 10 individuals (selected from the tag ids in ascending order).</li>"
      )
    ))
  })
  
  # make loaded data reactive
  rctv_data <- reactive({ data })
  
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
  
  
  
  ##### process loaded data
  rctv_processed_data <- reactive({
    
    # show modal during data processing
    show_modal_spinner(text = "Processing data. Please wait.")
    
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
    processed_data <- data.frame(matrix(ncol = 5, nrow = 0))
    processed_data_columns <- c("tag.local.identifier", "timestamps", "location.long", "location.lat", "date")
    colnames(processed_data) <- processed_data_columns
    
    # select time window
    last_n_days <- as.numeric(input$dropdown_date_range)
    
    # process last n days of observations per individual
    for(individual in individuals) {
      
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
      processed_data <- rbind(processed_data, individual_data)
      
    }
    
    # order data
    processed_data <- processed_data[order(processed_data$tag.local.identifier, processed_data$timestamps), ]
    
    # create lag columns
    processed_data$tag.local.identifier.lag <- c(NA, head(processed_data$tag.local.identifier, -1))
    processed_data$location.long.lag <- c(NA, head(processed_data$location.long, -1))
    processed_data$location.lat.lag <- c(NA, head(processed_data$location.lat, -1))
    
    processed_data$tag.local.identifier.lag <- ifelse(processed_data$tag.local.identifier == processed_data$tag.local.identifier.lag,
                                                      processed_data$tag.local.identifier.lag,
                                                      NA)
    
    processed_data$location.long.lag <- ifelse(processed_data$tag.local.identifier == processed_data$tag.local.identifier.lag,
                                               processed_data$location.long.lag,
                                               NA)
    
    processed_data$location.lat.lag <- ifelse(processed_data$tag.local.identifier == processed_data$tag.local.identifier.lag,
                                              processed_data$location.lat.lag,
                                              NA)
    
    # calculate distance between two location measurements
    calculate_distance_in_meters_between_coordinates <- function(lon_a, lat_a, lon_b, lat_b) {
      if(anyNA(c(lon_a, lat_a, lon_b, lat_b))) return(NA)
      distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)
    }
    
    processed_data$distance_meters <- mapply(lon_a = processed_data$location.long,
                                             lat_a = processed_data$location.lat,
                                             lon_b = processed_data$location.long.lag,
                                             lat_b = processed_data$location.lat.lag,
                                             FUN = calculate_distance_in_meters_between_coordinates)
    
    # remove modal after data processing and notify user
    remove_modal_spinner()
    notify_success("Data processing complete.")
    
    rctv_processed_data <- processed_data
    
    rctv_processed_data
    
  })
  
  
  
  ##### aggregate processed data
  rctv_data_aggregated <- reactive({
    
    # aggregate distances by time interval and individual
   data_aggregated <- rctv_processed_data() %>% 
	   		filter(!is.na(distance_meters)) %>% 
	   		group_by(date, tag.local.identifier) %>% 
	   		summarise(daily_distance_meters = sum(distance_meters, na.rm = TRUE),
	   		          measures_per_date = n())

    data_aggregated
    
  })
  
  
  
  ##### time series
  output$time_series <- renderPlotly({
    
    # load reactive data
    data_aggregated <- rctv_data_aggregated()
    
    # select individual to plot data for
    if(input$dropdown_individual == "all") {
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
    p <- plot_ly(as.data.frame(data_to_plot), x = ~date, y = ~daily_distance_meters, type = "scatter", mode = "lines", name = individual) %>% 
      layout(showlegend = TRUE, legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1), title = "Do the last distances moved look anomalous to you?")
    
    p
    
  })
  
  
  
  ##### map
  map <- reactive({
    
    # load reactive data
    processed_data <- rctv_processed_data()
    
    # set map colors
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]
    col_vector <- tail(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), -4)
    
    # store individual names and colors
    individual_names_original <- sort(unique(processed_data$tag.local.identifier))
    individual_colors <- col_vector[1:length(individual_names_original)]

    # filter for individual
    if(input$dropdown_individual == "all") {
      # do nothing and proceed
    } else {
      processed_data <- processed_data[processed_data$tag.local.identifier == input$dropdown_individual, ]
    }
    
    # filter for date range
    processed_data_filtered <- NULL
    for(this_tag in unique(processed_data$tag.local.identifier)) {
      individual_processed_data <- processed_data[processed_data$tag.local.identifier == this_tag, ]
      temp_dates <- as.Date(individual_processed_data$timestamps)
      max_temp_dates <- max(temp_dates)
      individual_processed_data <- individual_processed_data[temp_dates > (max_temp_dates - as.numeric(input$dropdown_date_range)), ]
      processed_data_filtered <- rbind(processed_data_filtered, individual_processed_data)
    }
    
    # get remaining individuals
    individual_names <- sort(unique(processed_data_filtered$tag.local.identifier))
    if (length(individual_names_original) == 1) {
      selected_id <- 1
    } else {
      selected_id <- which(individual_names_original %in% input$dropdown_individual)
    }
    
    this_line_opacity <- 0.8
    this_line_weight  <- 2

    # limit if needed the number of shown tracks on the map
    # remove the legend in case of more than 10 tracks
    track_limit <- length(individual_names)
    fixed_track_limit <- 10
    if(input$checkbox_full_map) {
      track_limit <- fixed_track_limit
    }

    # create map with lines for each individual
    map <- leaflet() %>% 
      addTiles()
    
    # check if only one element is in the selected set
    if(length(individual_names) > 1) {

      for (i in seq(along = head(individual_names, n = track_limit))) {

        map <- map %>% 
          addPolylines(data = processed_data_filtered[processed_data_filtered$tag.local.identifier == individual_names[i], ], lat = ~location.lat, lng = ~location.long, color = individual_colors[i], opacity = this_line_opacity, group = individual_names[i], weight = this_line_weight) %>% 
          addCircles(data = processed_data_filtered[processed_data_filtered$tag.local.identifier == individual_names[i], ], lat = ~location.lat, lng = ~location.long, color = individual_colors[i], opacity = 0.5, fillOpacity = 0.3, group = individual_names[i])
      
      }
      
    } else {
      
      last_lon <- tail(processed_data_filtered, 1)$location.long
      last_lat <- tail(processed_data_filtered, 1)$location.lat
      last_time <- tail(processed_data_filtered, 1)$timestamps

      map <- map %>% 
        addPolylines(data = processed_data_filtered, lat = ~location.lat, lng = ~location.long, color = individual_colors[selected_id], opacity = this_line_opacity, group = individual_names_original[selected_id], weight = this_line_weight) %>% 
        addCircleMarkers(data = processed_data_filtered, lat = ~location.lat, lng = ~location.long, color = individual_colors[selected_id], opacity = 0.5, fillOpacity = 0.3, label = ~timestamps, clusterOptions = markerClusterOptions()) %>% 
        addMarkers(lng = last_lon,
                   lat = last_lat,
                   label = paste0("Last location at: ", last_time))

    }
    
    if(input$dropdown_individual == "all") {
      selected_id <- 1:length(individual_colors)
    }

    # don't show the legend if the map is showing more than 10 tracks
    if(track_limit <= fixed_track_limit) {
      
      map  <- map %>% 
        addLegend(position = "topright", colors = individual_colors[selected_id], opacity = 0.6, labels = individual_names_original[selected_id])
    
    }

    map
    
  })
 
  output$map <- renderLeaflet({ map() })
  
  
  
  ##### movement summary
  rctv_movement_summary <- reactive({
    
    # load reactive data
    data_aggregated <- rctv_data_aggregated()
    
    # get individuals
    individuals <- unique(data_aggregated$tag.local.identifier)

    # calculate average measures per day and variance
    measures_aggregated <- data_aggregated %>% 
				    group_by(tag.local.identifier) %>% 
				    summarise(avg_measures = round(mean(measures_per_date), 1),
				              var_measures = round(var(measures_per_date), 1))
    
    # create empty dataframe to store movement summary
    movement_summary_columns <- c("individual", "start date", "end date", "#days w measures", "#days w/o measures", "today below avg.", "total distance (m)", "avg. distance (m)")
    movement_summary <- data.frame(matrix(ncol = length(movement_summary_columns), nrow = 0))
    colnames(movement_summary) <- movement_summary_columns
    
    # compute movement summary for last n days per individual
    for(individual in individuals) {
      
      # filter data based on individual
      individual_data_aggregated <- data_aggregated[data_aggregated$tag.local.identifier == individual, ]
      
      # calculate missing days (account for all set to 99999)
      missing_days <- as.numeric(input$dropdown_date_range) - dim(individual_data_aggregated)[1]
      missing_days <- ifelse(missing_days < 0, 0, missing_days)
      
      # calculate today below average
      avg_distance <- mean(individual_data_aggregated$daily_distance_meters)
      sd_distance <- sd(individual_data_aggregated$daily_distance_meters)
      min_date <- min(individual_data_aggregated$date)
      max_date <- max(individual_data_aggregated$date)
      meters_today <- individual_data_aggregated[individual_data_aggregated$date == max_date, "daily_distance_meters"] 
      below_today <- ifelse(meters_today < avg_distance - (1.5 * sd_distance), "yes", "no")

      # store values
      individual_movement_summary <- c(individual,
                                       as.character(min_date),
                                       as.character(max_date),
                                       dim(individual_data_aggregated)[1],
                                       missing_days,
                                       below_today,
                                       round(sum(individual_data_aggregated$daily_distance_meters), 0),
                                       round(avg_distance, 0))
      
      # append individual movement summary to existing dataframe
      movement_summary[nrow(movement_summary) + 1, ] <- individual_movement_summary
      
    }

    # join avg and var movement
    movement_summary <- movement_summary %>% 
      left_join(measures_aggregated, by = join_by(individual == tag.local.identifier))
    colnames(movement_summary) <- c(head(colnames(movement_summary), -2), "avg. measures", "var. measures")

    # convert relevant columns to numeric
    movement_summary[, 4:5] <- apply(movement_summary[, 4:5], 2, as.numeric)
    movement_summary[, 7:10] <- apply(movement_summary[, 7:10], 2, as.numeric)

    movement_summary
    
  })
  
  output$movement_summary <- renderDataTable({ datatable(rctv_movement_summary()) })
  
  return(rctv_data)

 }
