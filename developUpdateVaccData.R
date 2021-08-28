library(tidyverse)

source("downloadJHUData.R")

developGetVaccDataByGeography <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered developGetVaccDataByGeography\n")
  }

  dailyData <- downloadVaccDailyUpdateData(traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving developGetVaccDataByGeography\n")
  }
  
  return(dailyData)
}

getAndSaveVaccDailyData <- function(traceThisRoutine = FALSE) {
   foo <- developGetVaccDataByGeography(traceThisRoutine)
   
   # write_csv(foo,  "./DATA/CACHE/VACC_DAILY.csv")
   
   return(foo)
} 