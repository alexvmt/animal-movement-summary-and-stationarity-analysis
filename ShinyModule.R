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
             selectInput(ns("dropdown_indi"), "Individium:",
		  # TODO: make reactive, depending on given data set
                  choices = c("Choice 1", "Choice 2", "Choice 3")),
	     selectInput(ns("dropdown_date"), "Date range:",
                  # TODO: decide if we show the last days of the time series or from today()
                 choices = list("last day" = 1, "last week" = 7, "last month" = 30, "last 6 months" = 180, "last year" = 365, "all time" = 9999), selected = c("last year" = 365))
            #      # TODO: decide if we add this. default should be daily.
            #      choices = c("daily", "hourly"), selected = "daily"),
	    # TODO: remove outlier: unprobable distances > 100km/h per day
	    # checkbox   
	),
	column(9,
        #plotOutput(ns("timeseries_plot"))
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
  )
}

shinyModule <- function(input, output, session, data) {

#### interactive object to read in .RData file  ####
mvObj <- reactive({ data })


observe({
    # Wait until the data is loaded
    if (is.null(data)) return()
    # TODO: find a better/more stable way than gsub for removing the X
    # Update the dropdown with the unique values from the 'id' column of the loaded data
    updateSelectInput(session, "dropdown_indi", choices = c(gsub("^X", "", namesIndiv(data)), "all"), selected = "all")
})


# data depends on user input
rctv_processed_data <- reactive({

    # get dimensions and unique ids
    data_tmp <- as.data.frame(data)


    # TODO: check if necessary to filter / which id column is the right one?
    # rename columns
    id_col <- "individual.local.identifier"    #"tag-local-identifier"
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


    # select time window
    last_n_days <- as.numeric(input$dropdown_date)


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


    rctv_processed_data <- processed_data
    rctv_processed_data

}) # end of reactive data processing


rctv_agg_data <- reactive({

            # aggregate distances by time interval and individual
            data_agg_id_date <- aggregate(distance_meters ~ date + id, data = rctv_processed_data(), FUN = sum)

    data_agg_id_date

})


########################
# Time serie plot
########################

## plot all time series
#output$timeseries_plot <- renderPlot({	
#  ggplot(data_to_plot, aes(x = date, y = distance_meters, group = 1)) +
#  geom_line(linewidth = 0.75) +
#  scale_x_date(breaks = seq(start_date, end_date, by = scale)) +
#  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#  ggtitle(paste0("Distance in meters moved per day for individual ", id, " between ", start_date, " and ", end_date))
#})


# Render the Plotly plot
output$ts_plot <- renderPlotly({

    # load reactive data
    data_agg_id_date <- rctv_agg_data()

    if(input$dropdown_indi=="all"){
        # TODO: define a function to plot all and not just the first given one
        id <- data_agg_id_date$id[1]
    }else{
        id <- input$dropdown_indi
    }


    # plot timeseries for selected individual
    data_to_plot <- data_agg_id_date[data_agg_id_date$id == id, ]
    start_date <- min(data_to_plot$date)
    end_date <- max(data_to_plot$date)

    if (dim(data_to_plot)[1] > 30) {
    scale <- "1 week"
    } else {
    scale <- "1 day"
    }

    p <- plot_ly(data_to_plot, x = ~date, y = ~distance_meters, type = 'scatter', mode = 'lines', name = id) %>%
#            add_trace(y = ~Series2, name = 'Series 2', mode = 'lines') %>%
#            add_trace(y = ~Series3, name = 'Series 3', mode = 'lines') %>%
            layout(title = paste0("Distance per individual ", id, " between ", start_date, " and ", end_date))
            
    p # Return the plot
})




######################
# Map plot
######################


# TODO: cite repo if we intend to take this as is otherwise maybe use data frame
# TODO: check how to print proper names in legend / same as timeseries
# TODO: filter for only one individual
#### make map as reactive object to be able to save it ####
mapFinal <- reactive({
    mv <- mvObj()
    if(input$dropdown_indi == "all"){
		# do nothing
	}else{
		# filter individuum
		#mv <- mv[[input$dropdown_indi]]
	}
    
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

rctv_summary <- reactive({

    data_agg_id_date <- rctv_agg_data()

    # get individuals
    individuals <- unique(data_agg_id_date$id)

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
        individual_agg_data <- data_agg_id_date[data_agg_id_date$id == individual, ]

        # calculate missing days / account for all set to 99999
        missing_days <- as.numeric(input$dropdown_date) - dim(individual_agg_data)[1]
        missing_days <- ifelse(missing_days < 0, 0, missing_days)

        # calculate below average of today
        avg_distance <- mean(individual_agg_data$distance_meters)
        sd_distance  <- sd(individual_agg_data$distance_meters)
        max_date     <- max(individual_agg_data$date)
        meters_today <- individual_agg_data[individual_agg_data$date== max_date, "distance_meters"] 
        below_today  <- ifelse(meters_today < avg_distance - (1.5 * sd_distance), "yes", "no")

        # store values
        individual_summary_data <- c(unique(individual_agg_data$id),
                        dim(individual_agg_data)[1],
                        missing_days,
                        below_today,
                        round(sum(individual_agg_data$distance_meters), 2),
                        round(avg_distance, 2)
                        )

        # append summary data to existing dataframe
        summary[nrow(summary)+1, ] <- individual_summary_data

    }
    

    summary

})


  # plot a table
  output$table <- DT::renderDataTable({
    # render table
    datatable(rctv_summary())
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
