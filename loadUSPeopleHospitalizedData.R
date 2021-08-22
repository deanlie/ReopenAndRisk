source("./mostRecentDataDate.R")
source("./updateStateLevelSerializedDataFiles.R")
source("./computeNewAndGrowth.R")

loadUSPeopleHospitalizedData <- function() {
  traceThisRoutine = FALSE
  myPrepend = "From loadUSPeopleHospitalizedData"

  updateToThisDate <- expectedLatestUpdateDataDate()
  updateStateLevelSerializedDataFilesAsNecessary()
  
  US_People_Hospitalized <<- read_csv("./DATA/US_People_Hospitalized.csv",
                                      col_types = cols(.default = col_double(),
                                                       Province_State = col_logical(),
                                                       Combined_Key = col_character()))
  US_State_People_Hospitalized <<- read_csv("./DATA/US_State_People_Hospitalized.csv",
                                            col_types = cols(.default = col_double(),
                                                             Combined_Key = col_character()))
  US_People_Hospitalized_G7 <<- movingAverageGrowth(US_People_Hospitalized,
                                                    updateToThisDate,
                                                    28, 7, nFirstCols=3,
                                                    tibbleName="US_People_Hospitalized",
                                                    traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_People_Hospitalized_G7 <<- movingAverageGrowth(US_State_People_Hospitalized,
                                                          updateToThisDate,
                                                          28, 7, nFirstCols=3,
                                                          tibbleName="US_State_People_Hospitalized",
                                                          traceThisRoutine = traceThisRoutine, prepend = myPrepend)
}