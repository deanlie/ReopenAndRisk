source("updateStateLevelSerializedDataFiles.R")
source("columnUtilities.R")

loadUSMortalityRateData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSMortalityRateData\n")
  }

  updateStateLevelSerializedDataFilesAsNecessary(traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend)
  
  US_Mortality_Rate <<- read_csv("./DATA/US_Case_Fatality_Ratio.csv",
                                 col_types = myTSColTypes())
  US_State_Mortality_Rate <<- read_csv("./DATA/US_State_Case_Fatality_Ratio.csv",
                                       col_types = justCKColTypes())
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSMortalityRateData\n")
  }
}
