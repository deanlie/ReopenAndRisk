source("./updateStateLevelSerializedDataFiles.R")

# OUCH RED_FLAG Move this into loadAllUSData.R
# OUCH RED_FLAG Respect staticDataQ flag

loadUSTestingRateData <- function(staticDataQ = FALSE,
                                  traceThisRoutine = FALSE,
                                  prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSTestingRateData\n")
  }
  
  # OUCH I think load a type... handles this
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
  