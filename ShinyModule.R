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




shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  tagList(
    titlePanel("Plot data"),
    fluidRow(
	column(4,
             selectInput("dropdown", "Dropdown:",
                  choices = c("Choice 1", "Choice 2", "Choice 3")),
	     actionButton("toggle", "Toggle")
	),
	column(8,
		plotOutput(ns("timeseries_plot"))

	)
    ),
    fluidRow(
        column(4,
	       # empty for now
		actionButton("toggle", "Toggle")
        ),
        column(4,
                leafletOutput(ns("mymap"))
        ),
	column(4,
                DT::dataTableOutput(ns("table"))
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


  # plot a table
  output$table <- DT::renderDataTable({
    # You need to replace this with your actual data
    df <- data.frame(Category = c("Stat 1", "Stat 2", "Stat 3"),
                     Value = c(123, 456, 789))

    datatable(df)
  })


   # plot time series
   output$timeseries_plot <- renderPlot({
    # You need to replace this with your actual data
    df <- data.frame(time = Sys.time() + seq(-10, 0, by = 1),
                     value = rnorm(11))

    ggplot(df, aes(time, value)) +
      geom_line() +
      labs(x = "Time", y = "Value", title = "Time Series") +
      theme_minimal()
  })


  return(mvObj)
}
