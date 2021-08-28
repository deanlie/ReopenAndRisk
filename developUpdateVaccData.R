library(tidyverse)

source("downloadJHUData.R")
source("testDiscardNewDataFilewise.R")

vaccColTypes <- function() {
  return(cols(.default = col_double(),
              Combined_Key = col_character(),
              Datum = col_character(),
              Loc_Datum = col_character()))
}

developGetVaccDataByGeography <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered developGetVaccDataByGeography\n")
  }
  
  # Get US vacc data tibble
  # Get State vacc data tibble
  # See loadUSVaccinationData.R  
  US_Vaccinations_As_Filed <- read_csv("./DATA/CACHE/US_Vacc_Test.csv", #"./DATA/US_Vaccinations.csv",
                                       col_types = vaccColTypes())
  
  US_State_Vaccinations_As_Filed <- read_csv("./DATA/CACHE/US_Vacc_Test.csv", #"./DATA/US_State_Vaccinations.csv",
                                             col_types = vaccColTypes())

  # If latest date of US tibble is yesterday
  stateDataColNames <- names(US_State_Vaccinations_As_Filed)
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Last state data col is",
        stateDataColNames[length(stateDataColNames)], "\n")
  }
  if (mdy(stateDataColNames[length(stateDataColNames)]) == (Sys.Date() - 1)) {
    # vacc data is up to date
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Up to date!\n")
    }
  } else {
    # Else
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Must update!\n")
    }
    #   Get vacc timeline tibble (from URL if need be)
    #
    #   Get latest date of state vacc data file
    #   For (that date until yesterday)
    #     Filter that date data out of vacc timeline tibble
    #     Add it onto state vacc data tibble, you have code
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving developGetVaccDataByGeography\n")
  }
  
  return(list(STATE = US_State_Vaccinations_As_Filed,
              US = US_Vaccinations_As_Filed))
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

setupTestFiles <- function() {
  system("cp ./DATA/CACHE/US_Vaccinations.csv ./DATA/CACHE/US_Vacc_Test.csv")
  system("cp ./DATA/CACHE/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vacc_Test.csv")
  discardDataOutsideDateRangeFromAFile("./DATA/CACHE/US_Vacc_Test.csv",
                                       vaccColTypes(),
                                       mdy("08-01-2021"),
                                       mdy("08-10-2021"),
                                       "CACHE/US_Vacc_Test.csv")
  discardDataOutsideDateRangeFromAFile("./DATA/CACHE/US_Vacc_Test.csv",
                                       vaccColTypes(),
                                       mdy("08-01-2021"),
                                       mdy("08-10-2021"),
                                       "CACHE/US_Vacc_Test.csv")
}

testSuite <- function(traceThisRoutine = FALSE) {
  dataList <- developGetVaccDataByGeography(traceThisRoutine = traceThisRoutine,
                                            prepend = "TEST")
}
