source("./updateStateLevelSerializedDataFiles.R")

loadUSTestingRateData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSTestingRateData\n")
  }
  
  updateStateLevelSerializedDataFilesAsNecessary(traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend)
  
  US_Testing_Rate <<- read_csv("./DATA/US_Testing_Rate.csv",
                               col_types = myTSColTypes())
  US_State_Testing_Rate <<- read_csv("./DATA/US_State_Testing_Rate.csv",
                                     col_types = justCKColTypes())
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSTestingRateData\n")
  }
}
  