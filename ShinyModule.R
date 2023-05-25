####################
# Setup
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
#library(rgeos)   # needed only if we also calculate the bounding box



####################
# User interface
####################

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  tagList(
    titlePanel("Animal Movement Summary and Stationarity Analysis"),
    tags$style(type = "text/css", ".col-sm-9 {padding: 15px;}"), # prevent graphs from overlapping
    fluidRow(
      column(3,
             selectInput(ns("dropdown_indi"),
                         "Individium:",
                         choices = c("Choice 1", "Choice 2", "Choice 3")),
             selectInput(ns("dropdown_date"),
                         "Date range:",
                         choices = list("last day" = 1,
                                        "last week" = 7,
                                        "last 30 days" = 30,
					"last 90 days" = 90,
                                        "last 6 months" = 180,
                                        "last year" = 365,
                                        "all time" = 99999),
                         selected = c("last year" = 365))
             # TODO: decide if we add this; default should be daily
                        # choices = c("daily", "hourly"), selected = "daily"),
             # TODO: remove outliers; unprobable distances > 100km/h per day; add checkbox
             ),
      column(9,
             DT::dataTableOutput(ns("table"))
             )
      ),
    fluidRow(
      column(3,
             # empty for now
             helpText("This app helps to find stationarity and to explore animal tracking data. Three components: a statistics table, a map with the animal tracks and a time series plot help to analyze the given data set. The filters apply to all data sets but the indivial selection will not reduce the summary table to one individial and also only one indivial will be shown within the time series plot. Data is aggregated on a daily granularity and distances are calculated in meters. A potential workflow could start by spotting a single animal of interest on either the summary table or on the map and then the data can be filtered for this specific animal and also different date ranges analyzed. A date range always refers to the last n days of each given animal tracking series.")
             ),
      column(5,
             leafletOutput(ns("mymap"))
             ),
      column(4,
             plotlyOutput(ns("ts_plot"))
             )
      )
    )
}



####################
# Server
####################

shinyModule <- function(input, output, session, data) {

  #### interactive object to read in .RData file  ####
  mvObj <- reactive({ data })
  
  observe({
    # wait until the data is loaded
    if (is.null(data)) return()
    # TODO: find a better/more stable way than gsub for removing the X
    # Update the dropdown with the unique values from the 'id' column of the loaded data
    data_df <- as.data.frame(data)
    keys <- c(data_df$tag.local.identifier, "all")
    values <- c(data_df$tag.local.identifier, "all")
    key_value_list <- setNames(values, keys)
    updateSelectInput(session, "dropdown_indi", choices = key_value_list, selected = c("all" = "all")) 
  })
  
  # data depends on user input
  rctv_processed_data <- reactive({
    
    # transform move object to dataframe
    data_df <- as.data.frame(data)
    
    # cast tag.local.identifier to character
    data_df$tag.local.identifier <- as.character(data_df$tag.local.identifier)
    
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
    last_n_days <- as.numeric(input$dropdown_date)
    
    # process last n days of observations per individual
    for(individual in individuals) {
      
      # filter data based on individual
      individual_data <- data_df[data_df$tag.local.identifier == individual, ]
      
      # subset data to relevant columns
      individual_data <- individual_data[ , processed_data_columns]
      
      # TODO: maybe interpolate with 0s and mark in an additional row that this data was missing;
      #       could be show in a different color in the visualization then
      
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
    
    }) # end of reactive data processing
  
  rctv_agg_data <- reactive({
    
    # aggregate distances by time interval and individual
    data_aggregated <- aggregate(distance_meters ~ date + tag.local.identifier, data = rctv_processed_data(), FUN = sum)
    data_aggregated
    
  })
  
  
  
  ##### Time series
  
  # render plotly plot
  output$ts_plot <- renderPlotly({
    
    # load reactive data
    data_agg_id_date <- rctv_agg_data()
    
    if(input$dropdown_indi == "all") {
      # TODO: define a function to plot all and not just the first given one
      id <- data_agg_id_date$tag.local.identifier[1]
    } else {
      id <- input$dropdown_indi
    }
    
    # plot time series for selected individual
    data_to_plot <- data_agg_id_date[data_agg_id_date$tag.local.identifier == id, ]
    start_date <- min(data_to_plot$date)
    end_date <- max(data_to_plot$date)
    
    if (dim(data_to_plot)[1] > 30) {
      scale <- "1 week"
    } else {
      scale <- "1 day"
    }
    
    p <- plot_ly(data_to_plot, x = ~date, y = ~distance_meters, type = "scatter", mode = "lines", name = id) %>% 
      # add_trace(y = ~Series2, name = 'Series 2', mode = 'lines') %>% 
      # add_trace(y = ~Series3, name = 'Series 3', mode = 'lines') %>% 
      # layout(title = paste0("Distance per individual ", id, " between ", start_date, " and ", end_date), showlegend = TRUE)
      layout(showlegend = TRUE, legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1))
    
    p
    
  })
  
  
  
  ##### Map
  
  # TODO: consider using https://movevis.org/ to create an interactive map
  
  #### make map as reactive object to be able to save it ####
  mapFinal <- reactive({
    
    mv <- mvObj()
    
    # store individual colors and names
    #cols <- rainbow(length(namesIndiv(mv)))
    #data_spl <- move::split(mv)
    #indi_names_org <- namesIndiv(mv)
    
    # convert to data frame to filter
    mvdf <- as.data.frame(mv)
   
    # store individual colors and names
    indi_names_org <- unique(mvdf$tag.local.identifier)
    #cols <- viridis(length(indi_names_org))
    cols <- rainbow(length(indi_names_org))

    # filter for date range and individuals
    if(input$dropdown_indi == "all"){
      # do nothing
    } else {
      mvdf <- mvdf[mvdf$tag.local.identifier == input$dropdown_indi, ]
    }

     # filter for date range
    mvdf_filtered <- NULL
    for(this_tag in unique(mvdf$tag.local.identifier)){
        sub_mvdf <- mvdf[mvdf$tag.local.identifier==this_tag,]
        tmpdates <- as.Date(sub_mvdf$timestamps)
        maxtmpdates <- max(tmpdates)
        sub_mvdf <- sub_mvdf[tmpdates > (maxtmpdates - as.numeric(input$dropdown_date)),]
        mvdf_filtered <- rbind(mvdf_filtered, sub_mvdf)
    }
    mvdf <- mvdf_filtered
    
    # get remaining individuals
    indi_names <- unique(mvdf$tag.local.identifier)
    selected_id <- which(indi_names_org %in% input$dropdown_indi)
    
    # create map with lines for each animal, check if only one element is in the selected set
    map <- leaflet() %>% 
      addTiles() 
    
    if(length(indi_names) > 1) {
      for (i in seq(along = indi_names)) {
        map <- map %>% 
          addPolylines(data = mvdf[mvdf$tag.local.identifier==indi_names[i],], lat = ~location.lat, lng = ~location.long, color = cols[i], opacity = 0.6,  group = indi_names[i], weight = 2) %>% 
          addCircles(data = mvdf[mvdf$tag.local.identifier==indi_names[i],], lat = ~location.lat, lng = ~location.long, fillOpacity = 0.3, opacity = 0.5, color = cols[i], group = indi_names[i])
      }
    } else {
      ## Calculate opacities based on time
      #mvdf$date <- as.Date(mvdf$timestamps)	    
      #max_time <- max(mvdf$date)
      #min_time <- min(mvdf$date)
      #mvdf$this_opacity <- (as.numeric(mvdf$date - min_time) / as.numeric(max_time - min_time))^2

      map <- map %>% 
        addPolylines(data = mvdf, lat = ~location.lat, lng = ~location.long, color = cols[selected_id], opacity = 0.6,  group = indi_names_org[selected_id], weight = 2) %>% 
        addCircles(data = mvdf, lat = ~location.lat, lng = ~location.long, fillOpacity = 0.3, opacity = 0.5, color = cols[selected_id], group = indi_names_org[selected_id])
    }

    if(input$dropdown_indi == "all"){
      selected_id <- 1:length(cols)
    }

    map  <- map %>% 
      addLegend(position = "topright", colors = cols[selected_id], labels = indi_names_org[selected_id], opacity = 0.6)
    
    map
    
  })
  
  ### render map to be able to see it ####
  output$mymap <- renderLeaflet({ mapFinal() })
  
  
  
  ##### Movement summary
  
  rctv_summary <- reactive({
    
    data_agg_id_date <- rctv_agg_data()
    
    # get individuals
    individuals <- unique(data_agg_id_date$tag.local.identifier)
    
    # TODO: add column for n days with almost no movement/2x below stdev
    # TODO: missing data today / in time series?
    # TODO: less movement than expected => below_today, to be discussed
    
    # create empty dataframe to store summary data
    colnames_summary <- c("individual", "#observations", "#days w/o observations", "today below avg.", "total distance", "avg. distance")
    summary <- data.frame(matrix(ncol = length(colnames_summary), nrow = 0))
    colnames(summary) <- colnames_summary
    
    # compute summary statistics for last n days per individual
    for(individual in individuals) {
      
      # filter data based on individual
      individual_agg_data <- data_agg_id_date[data_agg_id_date$tag.local.identifier == individual, ]
      
      # calculate missing days / account for all set to 99999
      missing_days <- as.numeric(input$dropdown_date) - dim(individual_agg_data)[1]
      missing_days <- ifelse(missing_days < 0, 0, missing_days)
      
      # calculate below average of today
      avg_distance <- mean(individual_agg_data$distance_meters)
      sd_distance <- sd(individual_agg_data$distance_meters)
      max_date <- max(individual_agg_data$date)
      meters_today <- individual_agg_data[individual_agg_data$date == max_date, "distance_meters"] 
      below_today <- ifelse(meters_today < avg_distance - (1.5 * sd_distance), "yes", "no")
     
      ## optional: calculate bounding box for lat and long
      #coordinates <- cbind(lon, lat)
      #spatial_points <- SpatialPoints(coordinates, proj4string = CRS("+proj=longlat"))

      ## Convert to UTM (Universal Transverse Mercator) to preserve areas
      #spatial_points_utm <- spTransform(spatial_points, CRS("+proj=utm"))

      ## Calculate bounding box
      #bbox <- bbox(spatial_points_utm)

      ## Calculate area in square meters
      #area <- gArea(as(rgeos::gBuffer(SpatialPoints(cbind((bbox[1,2] + bbox[1,1])/2, (bbox[2,2] + bbox[2,1])/2)), width=abs(bbox[1,2] - bbox[1,1])/2), "SpatialPolygons"))

      # store values
      individual_summary_data <- c(unique(individual_agg_data$tag.local.identifier),
                                   dim(individual_agg_data)[1],
                                   missing_days,
                                   below_today,
                                   round(sum(individual_agg_data$distance_meters), 0),
                                   round(avg_distance, 0)
                                   )
      
      # append summary data to existing dataframe
      summary[nrow(summary) + 1, ] <- individual_summary_data
      
    }
    
    summary
    
  })
  
  # plot a table
  output$table <- DT::renderDataTable({ DT::datatable(rctv_summary()) })
  
  return(mvObj)
  
}
