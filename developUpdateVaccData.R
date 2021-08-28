library(tidyverse)

source("downloadJHUData.R")

developGetVaccDataByGeography <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered developGetVaccDataByGeography\n")
  }

  dailyData <- getURLFromSpecsOrStop(vaccDailyUpdateDataSpecs(),
                                     traceThisRoutine = traceThisRoutine,
                                     prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving developGetVaccDataByGeography\n")
  }
  
  return(dailyData)
}

getAndSaveVaccDailyData <- function(traceThisRoutine = FALSE) {
   foo <- getURLFromSpecsOrStop(vaccDailyUpdateDataSpecs(),
                                traceThisRoutine = traceThisRoutine,
                                prepend = myPrepend)
   
   write_csv(foo,  "./DATA/CACHE/VACC_DAILY.csv")
   
   return(foo)
}

getAndSaveVaccTimelineData <- function(traceThisRoutine = FALSE) {
  foo <- getURLFromSpecsOrStop(vaccTimeSeriesDataSpecs(),
                               traceThisRoutine = traceThisRoutine,
                               prepend = "TEST")
  
  write_csv(foo,  "./DATA/CACHE/VACC_TIMELINE.csv")
  
  return(foo)
}
