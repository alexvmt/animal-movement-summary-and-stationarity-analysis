library("fs")

bookmarkRootDir = "shiny_bookmarks" # it seems impossible to change the root-dir :/
bookmarkDir <- paste0(bookmarkRootDir, "/latest")
# `input.rds` is the expected file name by shiny!
bookmarkFileName <- "input.rds"

saveBookmarkAsLatest <- function(url) {
  stateId <- parseQueryString(sub("^.*\\?", "", url))$`_state_id_`
  if(!dir_exists(bookmarkDir)){
    dir_create(bookmarkDir)
  }
  file_move(
    path = path("shiny_bookmarks", stateId, bookmarkFileName),
    new_path = path(bookmarkDir, bookmarkFileName)
  )
  dir_delete(path("shiny_bookmarks", stateId))
  logger.info(paste("Moved shiny bookmark", stateId, "to", bookmarkDir))
}

restoreShinyBookmark <- function(session) {
  if(file_exists(path(bookmarkDir, bookmarkFileName)) && is.null(parseQueryString(session$clientData$url_search)$`_state_id_`)) {
    updateQueryString(queryString = "?_state_id_=latest")
    logger.info("Reloading session b/c of detected (not yet loaded) shiny bookmark")
    session$reload()
  }
}