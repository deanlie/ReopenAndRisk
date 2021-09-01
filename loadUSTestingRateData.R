source("./updateStateLevelSerializedDataFiles.R")

loadUSTestingRateData <- function() {
  updateStateLevelSerializedDataFilesAsNecessary()
  
  US_Testing_Rate <<- read_csv("./DATA/US_Testing_Rate.csv",
                               col_types = myTSColTypes())
  US_State_Testing_Rate <<- read_csv("./DATA/US_State_Testing_Rate.csv",
                                     col_types = myTSColTypes())
}
  