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
   rawData <- getURLFromSpecsOrStop(vaccDailyUpdateDataSpecs(),
                                traceThisRoutine = traceThisRoutine,
                                prepend = "VaccDailyUpdate")
   
   write_csv(rawData, vaccDailyUpdateDataSpecs()$PATH)
   
   return(rawData)
}

getAndSaveVaccTimelineData <- function(traceThisRoutine = FALSE) {
  rawData <- getURLFromSpecsOrStop(vaccTimeSeriesDataSpecs(),
                               traceThisRoutine = traceThisRoutine,
                               prepend = "VaccTimeline")
  
  write_csv(rawData, vaccTimeSeriesDataSpecs()$PATH)
  
  return(rawData)
}

testSuite <- function(traceThisRoutine = FALSE) {
  getAndSaveVaccDailyData(traceThisRoutine = traceThisRoutine)
  getAndSaveVaccTimelineData(traceThisRoutine = traceThisRoutine)
}
