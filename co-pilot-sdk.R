## Provided testing datasets in `./data/raw`: 
## "input1_greylgeese.rds", "input2_whitefgeese.rds", "input3_stork.rds", "input4_goat.rds"  
## for own data: file saved as a .rds containing a object of class MoveStack
inputFileName = "./data/raw/input2_whitefgeese.rds" 
## optionally change the output file name
unlink("./data/output/", recursive = TRUE) # delete "output" folder if it exists, to have a clean start for every run
dir.create("./data/output/") # create folder for output
outputFileName = "./data/output/output.rds" 

# this file is the home of your app code and will be bundled into the final app on MoveApps
source("ShinyModule.R")

# setup your environment
Sys.setenv(
    SOURCE_FILE = inputFileName, 
    OUTPUT_FILE = outputFileName, 
    ERROR_FILE="./data/output/error.log", 
    APP_ARTIFACTS_DIR ="./data/output/artifacts"
)

# simulate running your app on MoveApps
source("src/moveapps.R")
shinyApp(ui, server, enableBookmarking="server")
