library(tidyverse)

source("downloadJHUData.R")

developGetVaccDataByGeography <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered developGetVaccDataByGeography\n")
  }
  dailyData0 <- getURLOrStop(Vacc_URL(),
                          col_types = cols(.default = col_double(),
                                           Province_State = col_character(),
                                           Country_Region = col_character(),
                                           Date = col_date(format = ""),
                                           Vaccine_Type = col_character(),
                                           Combined_Key = col_character()),
                          traceThisRoutine = traceThisRoutine,
                          prepend = myPrepend)

  dailyData <- downloadVaccDailyUpdateData(traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "dim dailyData0",
        dim(dailyData0)[1], dim(dailyData0)[2], "\n")
    cat(file = stderr(), myPrepend, "dim dailyData",
        dim(dailyData)[1], dim(dailyData)[2], "\n")
    cat(file = stderr(), prepend, "Leaving developGetVaccDataByGeography\n")
  }
  
  return(list(D0 = dailyData0, D = dailyData))
}

getAndSaveVaccDailyData <- function(traceThisRoutine = FALSE) {
   foo <- developGetVaccDataByGeography(traceThisRoutine)
   
   write_csv(foo$D0, "./DATA/CACHE/VACC_DAILY_0.csv")
   write_csv(foo$D,  "./DATA/CACHE/VACC_DAILY.csv")
   
   return(foo)
} 