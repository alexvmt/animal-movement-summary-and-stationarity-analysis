library(leaflet)
library(leaflet.extras)
library(move)
library(sp)
library(pals)
library(mapview)

library(readr)
library(geosphere)
library(ggplot2)
library(magrittr)
library(htmlwidgets)
library(htmltools)
library(plotly)



shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  tagList(
    titlePanel("Inspect Stationarity"),
    tags$style(type = 'text/css', ".col-sm-9 {padding: 15px;}"),  #prevent graphs from overlapping
    fluidRow(
	column(3,
             selectInput("dropdown", "Individium:",
		  # TODO: make reactive, depending on given data set
                  choices = c("Choice 1", "Choice 2", "Choice 3")),
	     actionButton("toggle", "Toggle")
	),
	column(9,
#		plotOutput(ns("timeseries_plot"))
                #plotlyOutput(ns("ts_plot"))
		DT::dataTableOutput(ns("table"))
	)
    ),
    fluidRow(
        column(3,
	       # empty for now
	       helpText("This app helps to find stationarity. First a general overview of all data should help to spot individials, which are of potential interest. The statistic table should help to filter for those.")
        ),
        column(5,
                leafletOutput(ns("mymap"))
        ),
	column(4,
                #DT::dataTableOutput(ns("table"))
	       plotlyOutput(ns("ts_plot"))
	)
    )
    #fluidRow(
    #	downloadButton(ns('savePlot'), 'Save Plot')
    #)
  )
}

shinyModule <- function(input, output, session, data) {
  #### interactive object to read in .RData file  ####
  mvObj <- reactive({ data })

 # get dimensions and unique ids
 data_tmp <- as.data.frame(data)


# TODO: check if necessary to filter / which id column is the right one?
# rename columns
id_col <- "individual.id"    #"tag-local-identifier"
timestamp_col <- "timestamp"
lon_col <- "location.long"
lat_col <- "location.lat"
names(data_tmp)[names(data_tmp) == id_col] <- "id"
names(data_tmp)[names(data_tmp) == timestamp_col] <- "timestamp"
names(data_tmp)[names(data_tmp) == lon_col] <- "lon"
names(data_tmp)[names(data_tmp) == lat_col] <- "lat"


# get individuals
 individuals <- unique(data_tmp$id)


 # create year and date columns
 data_tmp$date <- as.Date(format(data_tmp$timestamp, format = "%Y-%m-%d"))
 data_tmp$year <- as.integer(format(data_tmp$timestamp, format = "%Y"))

 # create empty dataframe to store processed individual data
 processed_data <- data.frame(matrix(ncol = length(colnames(data_tmp)[-length(colnames(data_tmp))]), nrow = 0))
 colnames(processed_data) <- colnames(data_tmp)[-length(colnames(data_tmp))]


# set last n days
last_n_days <- 365

for(individual in individuals) {

  # filter data based on individual
  individual_data <- data_tmp[data_tmp$id == individual, ]

  # TODO: commented for now since object contains all columns and therefore more NAs
  #       maybe interpolate with 0s and mark in an additional row that this data was missing
  #       could be show in a different color in the visualization then
  # drop rows with missing values
  #individual_data <- na.omit(individual_data)

  # drop duplicated rows
  individual_data <- individual_data[!duplicated(individual_data[c("id", "timestamp")]), ]

  # extract max and min date
  max_date <- max(individual_data$date)
  min_date <- max_date - last_n_days

  # filter data based on date range
  individual_data <- individual_data[(individual_data$date >= min_date) & (individual_data$date <= max_date), ]

  # append processed data to existing dataframe
  processed_data <- rbind(processed_data, individual_data)

}


# order data
processed_data <- processed_data[order(processed_data$id, processed_data$timestamp), ]

# create lag columns
processed_data$id_lag <- c(NA, head(processed_data$id, -1))
processed_data$lon_lag <- c(NA, head(processed_data$lon, -1))
processed_data$lat_lag <- c(NA, head(processed_data$lat, -1))

processed_data$id_lag <- ifelse(processed_data$id == processed_data$id_lag,
                                processed_data$id_lag,
                                NA)

processed_data$lon_lag <- ifelse(processed_data$id == processed_data$id_lag,
                                 processed_data$lon_lag,
                                 NA)

processed_data$lat_lag <- ifelse(processed_data$id == processed_data$id_lag,
                                 processed_data$lat_lag,
                                 NA)

# calculate distance between two measurements
calculate_distance_in_meters_between_coordinates <- function(lon_a, lat_a, lon_b, lat_b) {

  if(anyNA(c(lon_a, lat_a, lon_b, lat_b))) return(NA)

  distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)

}



processed_data$distance_meters <- mapply(lon_a = processed_data$lon,
                                         lat_a = processed_data$lat,
                                         lon_b = processed_data$lon_lag,
                                         lat_b = processed_data$lat_lag,
                                         FUN = calculate_distance_in_meters_between_coordinates)

# aggregate distances by time interval and individual
data_agg_id_date <- aggregate(distance_meters ~ date + id, data = processed_data, FUN = sum)

# get number of aggregated observations per individual
aggregate(cbind(count = id) ~ id, data = data_agg_id_date, FUN = function(x){NROW(x)})

# TODO: remove the selection of a single individual
# select individual
id <- sample(individuals, 1)

# plot timeseries for selected individual
data_to_plot <- data_agg_id_date[data_agg_id_date$id == id, ]
start_date <- min(data_to_plot$date)
end_date <- max(data_to_plot$date)

if (dim(data_to_plot)[1] > 30) {
  scale <- "1 week"
} else {
  scale <- "1 day"
}


########################
# Time serie plot
########################


# TODO: plot all time series
output$timeseries_plot <- renderPlot({
ggplot(data_to_plot, aes(x = date, y = distance_meters, group = 1)) +
  geom_line(linewidth = 0.75) +
  scale_x_date(breaks = seq(start_date, end_date, by = scale)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggtitle(paste0("Distance in meters moved per day for individual ", id, " between ", start_date, " and ", end_date))
})


# TODO: plot all time series
    # Render the Plotly plot
    output$ts_plot <- renderPlotly({
        p <- plot_ly(data_to_plot, x = ~date, y = ~distance_meters, type = 'scatter', mode = 'lines', name = id) %>%
#            add_trace(y = ~Series2, name = 'Series 2', mode = 'lines') %>%
#            add_trace(y = ~Series3, name = 'Series 3', mode = 'lines') %>%
            layout(title = paste0("Distance in meters moved per day for individual ", id, " between ", start_date, " and ", end_date))
            
        p # Return the plot
    })




######################
# Map plot
######################



# TODO: check how to print proper names in legend / same as timeseries
  #### make map as reactive object to be able to save it ####
  mapFinal <- reactive({
    mv <- mvObj()
    cols <- colorFactor(gnuplot(), domain=namesIndiv(mv))
    map1 <- leaflet(mv) %>% addTiles()
    if(class(mv)=="MoveStack"){
      mvL <- move::split(mv)
      for(i in mvL){
        map1 <- addPolylines(map1, lng = coordinates(i)[,1],lat = coordinates(i)[,2],color=~cols(namesIndiv(i)),weight=5, opacity=0.7 ,highlightOptions = highlightOptions(color = "red",opacity = 1,weight = 2, bringToFront = TRUE))
      }
    }else{map1 <- addPolylines(map1, lng = coordinates(mv)[,1],lat = coordinates(mv)[,2],color=~cols(namesIndiv(mv)),weight=5, opacity=0.7 ,highlightOptions = highlightOptions(color = "red",opacity = 1,weight = 2, bringToFront = TRUE))}
    map1  %>% addLegend(position= "topright", pal=cols, values=namesIndiv(mv), labels=namesIndiv(mv) ,opacity = 0.7) %>%
      addScaleBar(position="bottomleft",options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = F, updateWhenIdle = TRUE))
  })


  ### render map to be able to see it ####
  output$mymap <- renderLeaflet({
    mapFinal()
  })



########################
# Statistics table
########################


# create empty dataframe to store summary data
summary <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(summary) <- c("individual", "last_n_days", "number_observations", "total_distance", "mean_distance")

# compute summary statistics for last n days per individual
for(individual in individuals) {

  for (last_n_days in c(3, 7, 14, 21, 28)) {

  # filter data based on individual
  individual_aggregated_data <- data_agg_id_date[data_agg_id_date$id == individual, ]

  # get max date
  max_date <- max(individual_aggregated_data$date)

  # filter data based on date
  individual_aggregated_data_filtered <- individual_aggregated_data[individual_aggregated_data$date > max_date - last_n_days, ]

  # create empty dataframe to store individual summary data
  individual_summary_data <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(individual_summary_data) <- c("individual", "last_n_days", "number_observations", "total_distance", "mean_distance")
  individual_summary_data[1, 1] = individual
  individual_summary_data[1, 2] = last_n_days

  # compute summary statistics
  individual_summary_data[1, 3] = dim(individual_aggregated_data_filtered)[1]
  individual_summary_data[1, 4] = sum(individual_aggregated_data_filtered$distance_meters)
  individual_summary_data[1, 5] = mean(individual_aggregated_data_filtered$distance_meters)

  # append summary data to existing dataframe
  summary <- rbind(summary, individual_summary_data)

  }

}
   




  # plot a table
  output$table <- DT::renderDataTable({
    # You need to replace this with your actual data
    df <- data.frame(Category = c("Stat 1", "Stat 2", "Stat 3"),
                     Value = c(123, 456, 789))

    datatable(summary)
  })


#   # plot time series
#   output$timeseries_plot <- renderPlot({
#    # You need to replace this with your actual data
#    df <- data.frame(time = Sys.time() + seq(-10, 0, by = 1),
#                     value = rnorm(11))
#
 #   ggplot(df, aes(time, value)) +
 #     geom_line() +
 #     labs(x = "Time", y = "Value", title = "Time Series") +
 #     theme_minimal()
 # })


  return(mvObj)
}
