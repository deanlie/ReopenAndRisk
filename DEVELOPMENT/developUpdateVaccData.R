library(tidyverse)

source("downloadJHUData.R")
source("testDiscardNewDataFilewise.R")
source("updateTimeSeriesDataFilesAsNecessary.R")
source("columnUtilities.R")

getAndSaveVaccDailyData <- function(traceThisRoutine = FALSE) {
   rawData <- getDataFromSpecsMaybeSave(vaccDailyUpdateDataSpecs(),
                                traceThisRoutine = traceThisRoutine,
                                prepend = "VaccDailyUpdate")

   return(rawData)
}

getAndSaveVaccTimelineData <- function(traceThisRoutine = FALSE) {
  rawData <- getDataFromSpecsMaybeSave(vaccTimeSeriesDataSpecs(),
                               traceThisRoutine = traceThisRoutine,
                               prepend = "VaccTimeline")

  return(rawData)
}

recreateVaccFilesFor829And830 <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered recreateVaccFilesFor829And830\n")
  }

  system("cp ./DATA/CACHE/US_V_Test_RR.csv ./DATA/US_Vaccinations.csv")
  system("cp ./DATA/CACHE/US_S_V_Test_RR.csv ./DATA/US_State_Vaccinations.csv")

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after first copies\n")
  }

  updateDataForUSVaccTimeSeries(traceThisRoutine = traceThisRoutine,
                                prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after first updateDataForUSVaccTimeSeries\n")
  }
  
  discardDataOutsideDateRangeFromAFile("./DATA/US_Vaccinations.csv",
                                       vaccColTypes(),
                                       mdy("06-15-2021"),
                                       mdy("09-01-2021"),
                                       "US_Vaccinations.csv",
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)
  discardDataOutsideDateRangeFromAFile("./DATA/US_State_Vaccinations.csv",
                                       vaccColTypes(),
                                       mdy("06-15-2021"),
                                       mdy("09-01-2021"),
                                       "US_State_Vaccinations.csv",
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_Vaccinations_30.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vaccinations_30.csv")
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after first 2 discards\n")
  }
  
  discardDataOutsideDateRangeFromAFile("./DATA/US_Vaccinations.csv",
                                       vaccColTypes(),
                                       mdy("06-15-2021"),
                                       mdy("08-28-2021"),
                                       "CACHE/US_V_Test_RR.csv",
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)
  
  discardDataOutsideDateRangeFromAFile("./DATA/US_State_Vaccinations.csv",
                                       vaccColTypes(),
                                       mdy("06-15-2021"),
                                       mdy("08-28-2021"),
                                       "CACHE/US_S_V_Test_RR.csv",
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_Vaccinations_29.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vaccinations_29.csv")
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after 2nd two discards\n")
  }
  
  discardDataOutsideDateRangeFromAFile("./DATA/US_Vaccinations.csv",
                                       vaccColTypes(),
                                       mdy("06-15-2021"),
                                       mdy("08-27-2021"),
                                       "CACHE/US_V_Test_RR.csv",
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)
  
  discardDataOutsideDateRangeFromAFile("./DATA/US_State_Vaccinations.csv",
                                       vaccColTypes(),
                                       mdy("06-15-2021"),
                                       mdy("08-27-2021"),
                                       "CACHE/US_S_V_Test_RR.csv",
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after 3rd 2 discards\n")
  }
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_Vaccinations_28.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vaccinations_28.csv")

  # US_Res$T0 = originalData, US_Res$T1 = truncatedTibble))
  # US_Res$T0 = originalData, US_Res$T1 = truncatedTibble))

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving recreateVaccFilesFor829And830\n")
  }
}

testSuite <- function(traceThisRoutine = FALSE) {
  myPrepend <- "  "
  cat(file = stderr(), "Enter testSuite\n")

  cat(file = stderr(), "  Process .._28\n")
  
  system("cp ./DATA/CACHE/US_Vaccinations_28.csv ./DATA/US_Vaccinations.csv")
  system("cp ./DATA/CACHE/US_State_Vaccinations_28.csv ./DATA/US_State_Vaccinations.csv")

  updateDataForUSVaccTimeSeries(traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_Vaccinations_From28.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vaccinations_From28.csv")
  
  cat(file = stderr(), "  Process .._29\n")

  system("cp ./DATA/CACHE/US_Vaccinations_29.csv ./DATA/US_Vaccinations.csv")
  system("cp ./DATA/CACHE/US_State_Vaccinations_29.csv ./DATA/US_State_Vaccinations.csv")
  
  updateDataForUSVaccTimeSeries(traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_Vaccinations_From29.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vaccinations_From29.csv")
  
  cat(file = stderr(), "  Process .._30\n")
  
  system("cp ./DATA/CACHE/US_Vaccinations_30.csv ./DATA/US_Vaccinations.csv")
  system("cp ./DATA/CACHE/US_State_Vaccinations_30.csv ./DATA/US_State_Vaccinations.csv")
  
  updateDataForUSVaccTimeSeries(traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_Vaccinations_From30.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vaccinations_From30.csv")
  
  return(dataList)
}

  
