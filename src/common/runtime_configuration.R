configuration <- function() {
    configurationString <- Sys.getenv(x = "CONFIGURATION", "{}")

    result <- if(configurationString != "") {
        jsonlite::fromJSON(txt=configurationString)
    } else {
        NULL
    }

    if (Sys.getenv(x = "PRINT_CONFIGURATION", "no") == "yes") {
        logger.debug("parse stored configuration: \'%s\'", configurationString)
        logger.info("app will be started with configuration:\n%s", jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE))
    }
    result
}

storeConfiguration <- function(configuration) {
  jsonlite::write_json(configuration, "./data/output/configuration.json", auto_unbox = TRUE)
  Sys.setenv(CONFIGURATION = jsonlite::toJSON(configuration, auto_unbox = TRUE))
  logger.info("Stored configuration of shinyModule to 'configuration.json'")
}
