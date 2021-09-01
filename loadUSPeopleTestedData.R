source("mostRecentDataDate.R")
source("updateStateLevelSerializedDataFiles.R")
source("computeNewAndGrowth.R")
source("columnUtilities.R")

loadUSPeopleTestedData <- function(traceThisRoutine = FALSE, prepend = "CALLER??") {
  myPrepend = paste("  ", prepend, sep =)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSPeopleTestedData\n")
  }
  updateToThisDate <- expectedLatestUpdateDataDate()
  
  # browser()

  updateStateLevelSerializedDataFilesAsNecessary(traceThisRoutine = FALSE)
  
  US_People_Tested <<- read_csv("./DATA/US_Total_Test_Results.csv",
                                col_types = myTSColTypes())
  US_State_People_Tested <<- read_csv("./DATA/US_State_Total_Test_Results.csv",
                                      col_types = justCKTypes())
  
  US_People_Tested_G7 <<- movingAverageGrowth(US_People_Tested,
                                              updateToThisDate, 28, 7,
                                              tibbleName="US_People_Tested",
                                              traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_People_Tested_G7 <<- movingAverageGrowth(US_State_People_Tested,
                                                    updateToThisDate, 28, 7,
                                                    tibbleName="US_State_People_Tested",
                                                    traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_People_Tested_A7 <<- movingAverageData(US_People_Tested,
                                            updateToThisDate, 28, 7,
                                            tibbleName="US_People_Tested",
                                            traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_People_Tested_A7 <<- movingAverageData(US_State_People_Tested,
                                                  updateToThisDate, 28, 7,
                                                  tibbleName="US_State_People_Tested",
                                                  traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSPeopleTestedData\n")
  }
}
