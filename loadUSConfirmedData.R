source("./mostRecentDataDate.R")
source("./updateTimeSeriesDataFilesAsNecessary.R")
source("./computeNewAndGrowth.R")

loadUSConfirmedData <- function() {
  traceThisRoutine = FALSE
  myPrepend = "From loadUSConfirmedData"
  updateToThisDate <- expectedLatestUpdateDataDate()
  updateTimeSeriesDataFilesAsNecessary()

  US_Confirmed <<- read_csv("./DATA/US_Confirmed.csv",
                            col_types=cols(.default = col_double(),
                                           Province_State = col_logical(),
                                           Combined_Key = col_character())) %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_State_Confirmed <<- read_csv("./DATA/US_State_Confirmed.csv",
                                  col_types=cols(.default = col_double(),
                                                 Province_State = col_character(),
                                                 Combined_Key = col_character())) %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_County_Confirmed <<- read_csv("./DATA/US_County_Confirmed.csv",
                                   col_types=cols(.default = col_double(),
                                                  Admin2 = col_character(),
                                                  Province_State = col_character(),
                                                  Combined_Key = col_character())) %>%
    filter(!str_detect(Combined_Key, "Princess"))

  US_Confirmed_G7 <<- movingAverageGrowth(US_Confirmed,
                                          updateToThisDate,
                                          28, 7, nFirstCols=3,
                                          tibbleName="US_Confirmed",
                                          traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_Confirmed_G7 <<- movingAverageGrowth(US_State_Confirmed,
                                                updateToThisDate,
                                                28, 7, nFirstCols=3,
                                                tibbleName="US_State_Confirmed",
                                                traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_County_Confirmed_G7 <<- movingAverageGrowth(US_County_Confirmed,
                                                 updateToThisDate,
                                                 28, 7, nFirstCols=3,
                                                 tibbleName="US_County_Confirmed",
                                                 traceThisRoutine = traceThisRoutine, prepend = myPrepend)

  US_Confirmed_A7 <<- movingAverageData(US_Confirmed,
                                        updateToThisDate,
                                        28, 7, nFirstCols=3,
                                        tibbleName="US_Confirmed",
                                        traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_Confirmed_A7 <<- movingAverageData(US_State_Confirmed,
                                              updateToThisDate,
                                              28, 7, nFirstCols=3,
                                              tibbleName="US_State_Confirmed",
                                              traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_County_Confirmed_A7 <<- movingAverageData(US_County_Confirmed,
                                               updateToThisDate,
                                               28, 7, nFirstCols=3,
                                               tibbleName="US_County_Confirmed",
                                               traceThisRoutine = traceThisRoutine, prepend = myPrepend)
}
