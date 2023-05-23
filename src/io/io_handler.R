readInput <- function(sourceFile) {
    inputType <- Sys.getenv(x = "INPUT_TYPE", "move::moveStack")
    # for now every input-type can be read via RDS
    logger.debug("Read input of type '%s'", inputType)
    return(readRdsInput(sourceFile))
}

storeResult <- function(result, outputFile) {
    outputType <- Sys.getenv(x = "OUTPUT_TYPE", "move::moveStack")
    # for now every output-type can be written via RDS
    logger.debug("Write output of type '%s'", outputType)
    return(storeRdsOutput(result, outputFile))
}

storeToFile <- function(result, outputFile) {
    if(!is.null(outputFile) && outputFile != "" && !is.null(result)) {
        logger.debug("Writing to file %s", outputFile)
        write(paste(result), file = outputFile)
    } else {
        logger.debug("Skip writing to file: no output File or result is missing")
    }
}

sourceFile <- function() {
    result <- Sys.getenv(x = "SOURCE_FILE", "")
    logger.debug("sourceFile: %s", result)
    result
}

outputFile <- function() {
    result <- Sys.getenv(x = "OUTPUT_FILE", "")
    logger.debug("outputFile: %s", result)
    result
}

errorFile <- function() {
  result <- Sys.getenv(x = "ERROR_FILE", "")
  logger.debug("errorFile: %s", result)
  result
}

notifyDone <- function(executionRuntime) {
    logger.trace("I'm ready.")
}