source("./mostRecentDataDate.R")
source("./updateStateLevelSerializedDataFiles.R")
source("./computeNewAndGrowth.R")

loadUSIncidentRateData <- function() {
  traceThisRoutine = FALSE
  myPrepend = "From loadUSIncidentRateData"
  updateToThisDate <- expectedLatestUpdateDataDate()
  updateStateLevelSerializedDataFilesAsNecessary()
  
  US_Incident_Rate <<- read_csv("./DATA/US_Incident_Rate.csv",
                                col_types = cols(.default = col_double(),
                                                 Province_State = col_logical(),
                                                 Combined_Key = col_character()))
  US_State_Incident_Rate <<- read_csv("./DATA/US_State_Incident_Rate.csv",
                                      col_types = cols(.default = col_double(),
                                                       Combined_Key = col_character()))
  US_Incident_Rate_G7 <<- movingAverageGrowth(US_Incident_Rate,
                                              updateToThisDate,
                                              28, 7, nFirstCols=3,
                                              tibbleName="US_Incident_Rate",
                                              traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_Incident_Rate_G7 <<- movingAverageGrowth(US_State_Incident_Rate,
                                                    updateToThisDate,
                                                    28, 7, nFirstCols=3,
                                                    tibbleName="US_State_Incident_Rate",
                                                    traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_Incident_Rate_A7 <<- movingAverageData(US_Incident_Rate,
                                            updateToThisDate,
                                            28, 7, nFirstCols=3,
                                            tibbleName="US_Incident_Rate",
                                            traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_Incident_Rate_A7 <<- movingAverageData(US_State_Incident_Rate,
                                                  updateToThisDate,
                                                  28, 7, nFirstCols=3,
                                                  tibbleName="US_State_Incident_Rate",
                                                  traceThisRoutine = traceThisRoutine, prepend = myPrepend)
}
