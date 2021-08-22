source("./mostRecentDataDate.R")
source("./updateStateLevelSerializedDataFiles.R")
source("./computeNewAndGrowth.R")

loadUSHospitalizationRateData <- function() {
  traceThisRoutine = FALSE
  myPrepend = "From loadUSHospitalizationRateData"
  updateToThisDate <- expectedLatestUpdateDataDate()
  updateStateLevelSerializedDataFilesAsNecessary()
  
  US_Hospitalization_Rate <<- read_csv("./DATA/US_Hospitalization_Rate.csv",
                                       col_types = cols(.default = col_double(),
                                                        Province_State = col_logical(),
                                                        Combined_Key = col_character()))
  US_State_Hospitalization_Rate <<- read_csv("./DATA/US_State_Hospitalization_Rate.csv",
                                             col_types = cols(.default = col_double(),
                                                              Combined_Key = col_character()))
  
  US_Hospitalization_Rate_G7 <<- movingAverageGrowth(US_Hospitalization_Rate,
                                                     updateToThisDate,
                                                     28, 7, nFirstCols=3,
                                                     tibbleName="US_Hospitalization_Rate",
                                                     traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_Hospitalization_Rate_G7 <<- movingAverageGrowth(US_State_Hospitalization_Rate,
                                                           updateToThisDate,
                                                           28, 7, nFirstCols=3,
                                                           tibbleName="US_State_Hospitalization_Rate",
                                                           traceThisRoutine = traceThisRoutine, prepend = myPrepend)
}
