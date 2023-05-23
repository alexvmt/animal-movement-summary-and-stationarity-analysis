# tie everything together
# the following files will NOT bundled into the final app - they are just helpers in the SDK
source("src/common/logger.R")
source("src/common/runtime_configuration.R")
source("src/io/app_files.R")
source("src/io/io_handler.R")
source("src/io/shiny_bookmark_handler.R")
source("src/io/rds.R")

Sys.setenv(tz="UTC")

ui <- function(request) { 
  fluidPage(
    tags$head(singleton(tags$script(src = 'ws-keep-alive-fix.js'))),
    tags$link(rel = "stylesheet", type = "text/css", href = "ws-keep-alive-fix.css"),
    shinyModuleUserInterface("shinyModule"),
    dataTableOutput("table"), #Is necessary for storing result

    # ws-heartbeat fix
    # kudos: https://github.com/rstudio/shiny/issues/2110#issuecomment-419971302
    textOutput("ws_heartbeat"),
    # store the current state (as a shiny bookmark)
    bookmarkButton(title="Save state",class="btn btn-outline-success")
  )
}

server <- function(input, output, session) {
  tryCatch(
  {
    data <- readInput(sourceFile())
    shinyModuleArgs <- c(shinyModule, "shinyModule")
    if (!is.null(data)) {
        shinyModuleArgs[["data"]] <- data
    }

    result <- do.call(callModule, shinyModuleArgs)

    observeEvent(
      session,
      {
        restoreShinyBookmark(session)
      },
      once = TRUE
    )

    output$table <- renderDataTable({
      storeResult(result(), outputFile())
      notifyDone("SHINY")
    })
  },
  error = function(e) {
    # error handler picks up where error was generated
    print(paste("ERROR: ", e))
    storeToFile(e, errorFile())
    if (grepl("[code 10]", e$message, fixed=TRUE)) {
      stopApp(10)
    } else {
      stop(e) # re-throw the exception
    }
  })

  # ws-heartbeat fix
  # kudos: https://github.com/rstudio/shiny/issues/2110#issuecomment-419971302
  output$ws_heartbeat <- renderText({
    req(input$heartbeat)
    input$heartbeat
  })
  
  # hook after persisting the bookmark
  # see https://shiny.rstudio.com/articles/advanced-bookmarking.html
  onBookmarked(function(url) {
    saveBookmarkAsLatest(url)
  })
}
