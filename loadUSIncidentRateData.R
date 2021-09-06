source("mostRecentDataDate.R")
source("updateStateLevelSerializedDataFiles.R")
source("computeNewAndGrowth.R")
source("columnUtilities.R")

loadUSIncidentRateData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSIncidentRateData\n")
  }

  if (traceThisRoutine) {
    # cat(file = stderr(), myPrepend, "\n")    
  }

  updateToThisDate <- expectedLatestUpdateDataDate()
  updateStateLevelSerializedDataFilesAsNecessary()

  US_Incident_Rate <<- read_csv("./DATA/US_Incident_Rate.csv",
                                col_types = myTSColTypes())
  US_State_Incident_Rate <<- read_csv("./DATA/US_State_Incident_Rate.csv",
                                      col_types = justCKColTypes())
  US_Incident_Rate_G7 <<- movingAverageGrowth(US_Incident_Rate,
                                              updateToThisDate, 28, 7,
                                              tibbleName="US_Incident_Rate",
                                              traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_Incident_Rate_G7 <<- movingAverageGrowth(US_State_Incident_Rate,
                                                    updateToThisDate, 28, 7,
                                                    tibbleName="US_State_Incident_Rate",
                                                    traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_Incident_Rate_A7 <<- movingAverageData(US_Incident_Rate,
                                            updateToThisDate, 28, 7,
                                            tibbleName="US_Incident_Rate",
                                            traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_Incident_Rate_A7 <<- movingAverageData(US_State_Incident_Rate,
                                                  updateToThisDate, 28, 7,
                                                  tibbleName="US_State_Incident_Rate",
                                                  traceThisRoutine = traceThisRoutine, prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSIncidentRateData\n")
  }
}
