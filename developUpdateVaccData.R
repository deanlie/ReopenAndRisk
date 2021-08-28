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

testSuite <- function() {
  ss <- vaccDailyUpdateDataSpecs()
  cat(file=stderr(), "vaccDailyUpdateDataSpecs() ->", ss$PATH, "\n")
  ss <- vaccDailyUpdateDataSpecs(NULL)
  cat(file=stderr(), "vaccDailyUpdateDataSpecs(NULL) ->", ss$PATH, "\n")
  ss <- vaccDailyUpdateDataSpecs(mdy("08-05-2021"))
  cat(file=stderr(), "vaccDailyUpdateDataSpecs(mdy('08-05-2021')) ->", ss$PATH, "\n")
  
  ss <- vaccTimeSeriesDataSpecs()
  cat(file=stderr(), "vaccTimeSeriesDataSpecs() ->", ss$PATH, "\n")
  ss <- vaccTimeSeriesDataSpecs(NULL)
  cat(file=stderr(), "vaccTimeSeriesDataSpecs(NULL) ->", ss$PATH, "\n")
  ss <- vaccTimeSeriesDataSpecs(mdy("08-05-2021"))
  cat(file=stderr(), "vaccTimeSeriesDataSpecs(mdy('08-05-2021')) ->", ss$PATH, "\n")
  
  ss <- pVaccTimeSeriesDataSpecs()
  cat(file=stderr(), "pVaccTimeSeriesDataSpecs() ->", ss$PATH, "\n")
  ss <- pVaccTimeSeriesDataSpecs(NULL)
  cat(file=stderr(), "pVaccTimeSeriesDataSpecs(NULL) ->", ss$PATH, "\n")
  ss <- pVaccTimeSeriesDataSpecs(mdy("08-05-2021"))
  cat(file=stderr(), "pVaccTimeSeriesDataSpecs(mdy('08-05-2021')) ->", ss$PATH, "\n")
}
