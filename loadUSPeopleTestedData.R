source("./mostRecentDataDate.R")
source("./updateStateLevelSerializedDataFiles.R")
source("./computeNewAndGrowth.R")
source("./variableFieldNames.R")

loadUSPeopleTestedData <- function() {
  traceThisRoutine = FALSE
  myPrepend = "From loadUSPeopleTestedData"
  updateToThisDate <- expectedLatestUpdateDataDate()
  
  vfn <- variableFieldNames(updateToThisDate)
  
  updateStateLevelSerializedDataFilesAsNecessary(traceThisRoutine = FALSE)
  
  US_People_Tested <<- read_csv(paste("./DATA/US_", vfn$numberTested, ".csv", sep=""),
                                col_types=cols(.default = col_double(),
                                               Province_State = col_logical(),
                                               Combined_Key = col_character()))
  US_State_People_Tested <<- read_csv(paste("./DATA/US_State_", vfn$numberTested, ".csv", sep=""),
                                      col_types=cols(.default=col_double(),
                                                     Combined_Key = col_character()))
  
  US_People_Tested_G7 <<- movingAverageGrowth(US_People_Tested,
                                              updateToThisDate,
                                              28, 7, nFirstCols=3,
                                              tibbleName="US_People_Tested",
                                              traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_People_Tested_G7 <<- movingAverageGrowth(US_State_People_Tested,
                                                    updateToThisDate,
                                                    28, 7, nFirstCols=3,
                                                    tibbleName="US_State_People_Tested",
                                                    traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_People_Tested_A7 <<- movingAverageData(US_People_Tested,
                                            updateToThisDate,
                                            28, 7, nFirstCols=3,
                                            tibbleName="US_People_Tested",
                                            traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_People_Tested_A7 <<- movingAverageData(US_State_People_Tested,
                                                  updateToThisDate,
                                                  28, 7, nFirstCols=3,
                                                  tibbleName="US_State_People_Tested",
                                                  traceThisRoutine = traceThisRoutine, prepend = myPrepend)
}
