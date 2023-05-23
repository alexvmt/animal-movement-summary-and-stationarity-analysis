library("shiny")
library("move")

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal() -> logger.trace()

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id) ## all IDs of UI functions need to be wrapped in ns()
  tagList(
    titlePanel("Plot of Track"),
    numericInput(inputId = ns("ind"), label = "Select one indivdual", value = 1),
    plotOutput(ns("plot"))
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns ## all IDs of UI functions need to be wrapped in ns()
  current <- reactiveVal(data)
  
  output$plot <- renderPlot({
    plot(data[[input$ind]])
  })
  
  return(reactive({ current() })) ## if data are not modified, the unmodified input data must be returned
}
