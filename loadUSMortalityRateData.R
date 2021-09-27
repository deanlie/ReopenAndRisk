source("updateStateLevelSerializedDataFiles.R")
source("columnUtilities.R")

# OUCH RED_FLAG Move this into loadAllUSData.R
# OUCH RED_FLAG Respect staticDataQ flag

loadUSMortalityRateData <- function(staticDataQ = FALSE,
                                    traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSMortalityRateData\n")
  }

  # OUCH I think load a type... handles this
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
