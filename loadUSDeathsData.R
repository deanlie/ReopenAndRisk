source("./mostRecentDataDate.R")
source("./updateTimeSeriesDataFilesAsNecessary.R")
source("./computeNewAndGrowth.R")

# loadUSDeathsData <- function() {
#   traceThisRoutine = FALSE
#   myPrepend = "From loadUSDeathsData"
#   updateToThisDate <- expectedLatestUpdateDataDate()
#   updateTimeSeriesDataFilesAsNecessary()
# 
#   US_Deaths <<- read_csv("./DATA/US_Deaths.csv",
#                          col_types=cols(.default = col_double(),
#                                         Province_State = col_character(),
#                                         Combined_Key = col_character()))
#   US_State_Deaths <<- read_csv("./DATA/US_State_Deaths.csv",
#                                col_types=cols(.default = col_double(),
#                                               Province_State = col_character(),
#                                               Combined_Key = col_character()))
#   US_County_Deaths <<- read_csv("./DATA/US_County_Deaths.csv",
#                                 col_types=cols(.default = col_double(),
#                                                Admin2 = col_character(),
#                                                Province_State = col_character(),
#                                                Combined_Key = col_character()))
# 
#   US_Deaths_A7 <<- movingAverageData(US_Deaths,
#                                      updateToThisDate, 28, 7,
#                                      tibbleName="US_Deaths",
#                                      traceThisRoutine = traceThisRoutine, prepend = myPrepend)
#   US_Deaths_G7 <<- movingAverageGrowth(US_Deaths,
#                                        updateToThisDate, 28, 7,
#                                        tibbleName="US_Deaths",
#                                        traceThisRoutine = traceThisRoutine, prepend = myPrepend)
# 
#   US_State_Deaths_A7 <<- movingAverageData(US_State_Deaths,
#                                            updateToThisDate, 28, 7,
#                                            tibbleName="US_State_Deaths",
#                                            traceThisRoutine = traceThisRoutine, prepend = myPrepend)
#   US_State_Deaths_G7 <<- movingAverageGrowth(US_State_Deaths,
#                                              updateToThisDate, 28, 7,
#                                              tibbleName="US_State_Deaths",
#                                              traceThisRoutine = traceThisRoutine, prepend = myPrepend)
# 
#   US_County_Deaths_A7 <<- movingAverageData(US_County_Deaths,
#                                             updateToThisDate, 28, 7,
#                                             tibbleName="US_County_Deaths",
#                                             traceThisRoutine = traceThisRoutine, prepend = myPrepend)
#   US_County_Deaths_G7 <<- movingAverageGrowth(US_County_Deaths,
#                                               updateToThisDate, 28, 7,
#                                               tibbleName="US_County_Deaths",
#                                               traceThisRoutine = traceThisRoutine, prepend = myPrepend)
# }
