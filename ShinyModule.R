####################
# setup
####################

# load packages
library(leaflet)
library(leaflet.extras)
library(move)
library(sp)
library(pals)
library(mapview)
library(geosphere)
library(ggplot2)
library(magrittr)
library(htmlwidgets)
library(htmltools)
library(plotly)
library(DT)
library(RColorBrewer)


####################
# user interface
####################

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  tagList(
    titlePanel("Animal Movement Summary and Stationarity Analysis"),
    tags$style(type = "text/css", ".col-sm-9 {padding: 15px;}"), # prevent graphs from overlapping
    fluidRow(
      column(3,
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
                         selected = c("last year" = 365))),
      column(9, DT::dataTableOutput(ns("movement_summary")))
    ),
    fluidRow(
      column(3,
             helpText("This app summarizes animal movement data and helps to find stationary tags.
                      First, a general overview of all data should help to spot individuals which are of potential interest.
                      The movement summary table should help to filter for those.")),
      column(5, leafletOutput(ns("map"))),
      column(4, plotlyOutput(ns("time_series")))
    )
  )
}



####################
# server
####################

shinyModule <- function(input, output, session, data) {
  
  # make loaded data reactive
  rctv_data <- reactive({ data })
  
  # generate inputs for dropdowns
  observe({
    # wait until the data is loaded
    if (is.null(data)) return()
    data_df <- as.data.frame(data)
    keys <- c(as.character(data_df$tag.local.identifier), "all")
    values <- c(as.character(data_df$tag.local.identifier), "all")
    key_value_list <- setNames(values, keys)
    updateSelectInput(session, "dropdown_individual", choices = key_value_list, selected = c("all" = "all")) 
  })
  
  
  
  ##### process loaded data
  rctv_processed_data <- reactive({
    
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
      individual_data <- individual_data[(individual_data$date >= min_date) & (individual_data$date <= max_date), ]
      
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
    
    rctv_processed_data <- processed_data
    
    rctv_processed_data
    
  })
  
  
  
  ##### aggregate processed data
  rctv_data_aggregated <- reactive({
    
    # aggregate distances by time interval and individual
    data_aggregated <- aggregate(distance_meters ~ date + tag.local.identifier, data = rctv_processed_data(), FUN = sum)
    
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
    
    # set date scale
    if (dim(data_to_plot)[1] > 30) {
      scale <- "1 week"
    } else {
      scale <- "1 day"
    }
    
    # plot time series for selected individual
    p <- plot_ly(data_to_plot, x = ~date, y = ~distance_meters, type = "scatter", mode = "lines", name = individual) %>% 
      layout(showlegend = TRUE, legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1))
    
    p
    
  })
  
  
  
  ##### map
  map <- reactive({
    
    # load reactive data
    processed_data <- rctv_processed_data()
    
    # store individual names and colors
    individual_names_original <- unique(processed_data$tag.local.identifier)
    individual_colors <- brewer.pal(length(individual_names_original), "Dark2")
    #   individual_colors <- rainbow(length(individual_names_original))
    
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
    individual_names <- unique(processed_data_filtered$tag.local.identifier)
    if (length(individual_names_original) == 1) {
      selected_id <- 1
    } else {
      selected_id <- which(individual_names_original %in% input$dropdown_individual)
    }
    
    # create map with lines for each individual
    map <- leaflet() %>% 
      addTiles() 
    
    # check if only one element is in the selected set
    if(length(individual_names) > 1) {
      
      for (i in seq(along = individual_names)) {
        
        map <- map %>% 
          addPolylines(data = processed_data_filtered[processed_data_filtered$tag.local.identifier == individual_names[i], ], lat = ~location.lat, lng = ~location.long, color = individual_colors[i], opacity = 0.6,  group = individual_names[i], weight = 2) %>% 
          addCircles(data = processed_data_filtered[processed_data_filtered$tag.local.identifier == individual_names[i], ], lat = ~location.lat, lng = ~location.long, color = individual_colors[i], opacity = 0.5, fillOpacity = 0.3, group = individual_names[i])
        
      }
      
    } else {
      
      last_lon <- tail(processed_data_filtered, 1)$location.long
      last_lat <- tail(processed_data_filtered, 1)$location.lat
     
      # last marker color 
      marker_color <- "red"
      
      map <- map %>% 
        addPolylines(data = processed_data_filtered, lat = ~location.lat, lng = ~location.long, color = individual_colors[selected_id], opacity = 0.6,  group = individual_names_original[selected_id], weight = 2) %>% 
        addCircles(data = processed_data_filtered, lat = ~location.lat, lng = ~location.long, color = individual_colors[selected_id], opacity = 0.5, fillOpacity = 0.3, group = individual_names_original[selected_id]) %>% 
        addCircleMarkers(lng = last_lon,
                         lat = last_lat,
                         label = paste0("lon: ", last_lon, "; lat: ", last_lat),
                         color = marker_color)
      
    }
    
    if(input$dropdown_individual == "all") {
      selected_id <- 1:length(individual_colors)
    }

    map  <- map %>% 
      addLegend(position = "topright", colors = individual_colors[selected_id], opacity = 0.6, labels = individual_names_original[selected_id])
    
    map
    
  })
  
  output$map <- renderLeaflet({ map() })
  
  
  
  ##### movement summary
  rctv_movement_summary <- reactive({
    
    # load reactive data
    data_aggregated <- rctv_data_aggregated()
    
    # get individuals
    individuals <- unique(data_aggregated$tag.local.identifier)
    
    # create empty dataframe to store movement summary
    movement_summary_columns <- c("individual", "#observations", "#days w/o observations", "today below avg.", "total distance (m)", "avg. distance (m)")
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
      avg_distance <- mean(individual_data_aggregated$distance_meters)
      sd_distance <- sd(individual_data_aggregated$distance_meters)
      max_date <- max(individual_data_aggregated$date)
      meters_today <- individual_data_aggregated[individual_data_aggregated$date == max_date, "distance_meters"] 
      below_today <- ifelse(meters_today < avg_distance - (1.5 * sd_distance), "yes", "no")

      # store values
      individual_movement_summary <- c(individual,
                                       dim(individual_data_aggregated)[1],
                                       missing_days,
                                       below_today,
                                       round(sum(individual_data_aggregated$distance_meters), 0),
                                       round(avg_distance, 0)
      )
      
      # append individual movement summary to existing dataframe
      movement_summary[nrow(movement_summary) + 1, ] <- individual_movement_summary
      
    }
    
    movement_summary
    
  })
  
  output$movement_summary <- DT::renderDataTable({ DT::datatable(rctv_movement_summary()) })
  
  return(rctv_data)
  
}
