library(tidyverse)
library(lubridate)

source("./computeNewAndGrowth.R")
source("./mostRecentDataDate.R")
source("./updateTimeSeriesDataFilesAsNecessary.R")
source("./loadUSConfirmedData.R")
source("./loadUSDeathsData.R")
source("./loadUSPeopleTestedData.R")
source("./loadUSHospitalizationRateData.R")
source("./loadUSHospitalizationRateData.R")
source("./loadUSIncidentRateData.R")
source("./loadUSMortalityRateData.R")
source("./loadUSPeopleHospitalizedData.R")
source("./loadUSTestingRateData.R")
source("./loadUSVaccinationData.R")

loadATypeOfData <- function(US_leaf, State_leaf, County_leaf,
                            theType, computeNew, computeAvg,
                            traceThisRoutine = FALSE, prepend = "CALLER??") {
  # Functions local to this routine
  readLeaf <- function(aLeaf, theScope) {
    aPath <- paste("./DATA/", aLeaf, ".csv", sep = "")
    if (theScope == "County") {
      colTypes <- cols(.default = col_double(),
                       Province_State = col_character(),
                       Combined_Key = col_character(),
                       Admin2 = col_character())
    } else {
      colTypes <- cols(.default = col_double(),
                       Province_State = col_character(),
                       Combined_Key = col_character())
    }
    aTibble <- read_csv(aPath, col_types=colTypes) %>%
      filter(!str_detect(Combined_Key, "Princess"))
  }
  
  newFromCumulative <- function(inputTibble, updateToThisDate,
                                theScope, theType,
                                nDays = 35, nFirst = 3,
                                traceThisRoutine = FALSE, prepend = "") {
    myPrepend <- paste(prepend, "  ", sep = "")
    if (traceThisRoutine) {
      cat(file = stderr(), prepend, "Entered newFromCumulative, nFirst = ", nFirst, "\n")
    }
    outputList <- computeNewOnDayAndGrowthRate(inputTibble,
                                               updateToThisDate,
                                               nDays,
                                               getGrowthRate = FALSE,
                                               nonzeroOnly = FALSE,
                                               tibbleName = paste(theScope,
                                                                  theType,
                                                                  "Cumulative",
                                                                  sep = ""),
                                               traceThisRoutine = traceThisRoutine,
                                               prepend = myPrepend)
    myPrepend <- paste(prepend, "  ", sep = "")
    if (traceThisRoutine) {
      cat(file = stderr(), prepend, "Leaving newFromCumulative\n")
    }
    outputTibble <- outputList$new
  }
  
  movingAverageFromCumulative <- function(inputTibble, updateToThisDate,
                                          theScope, theType,
                                          nDays = 28, nAvg = 7, nFirst = 3,
                                          traceThisRoutine = traceThisRoutine,
                                          prepend = "CALLER??") {
    movingAverageGrowth(inputTibble, updateToThisDate, nDays, nAvg, nFirstCols = nFirst,
                        tibbleName = paste(theScope, theType, "Cumulative", sep = ""))
  }
  
  # Mainline of  this routine
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadATypeOfData\n")
  }
  
  updateToThisDate <- expectedLatestUpdateDataDate()
  updateTimeSeriesDataFilesAsNecessary()
  
  US_Cumulative     <- readLeaf(US_leaf, "US")
  State_Cumulative  <- readLeaf(State_leaf, "State")
  if (is_null(County_leaf)) {
    County_Cumulative <- NULL
  } else {
    County_Cumulative <- readLeaf(County_leaf, "County")
    if (traceThisRoutine) {
      names_p <- paste(names(County_Cumulative)[1:5])
      cat(file = stderr(), myPrepend, "County_Cumulative names:", names_p, "...\n")
    }
  }
  
  if (computeNew) {
    US_New <- newFromCumulative(US_Cumulative, updateToThisDate,
                                "US", theType,
                                traceThisRoutine = traceThisRoutine,
                                prepend = myPrepend)
    State_New <- newFromCumulative(State_Cumulative, updateToThisDate,
                                   "State", theType,
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
    if (is_null(County_leaf)) {
      County_New <- NULL
    } else {
      County_New <- newFromCumulative(County_Cumulative, updateToThisDate,
                                      "County", theType,
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      if (traceThisRoutine) {
        names_p <- paste(names(County_New)[1:5])
        cat(file = stderr(), myPrepend, "County_New names:", names_p, "...\n")
      }
    }
  } else {
    US_New <- NULL
    State_New <- NULL
    County_New <- NULL
  }
  
  if (computeAvg) {
    US_Cumulative_A7 <- movingAverageData(US_Cumulative, updateToThisDate, 28, 7, nFirstCols = 3,
                                          tibbleName = paste("US", theType, "Cumulative", sep=""),
                                          traceThisRoutine = traceThisRoutine,
                                          prepend = myPrepend)
    State_Cumulative_A7 <- movingAverageData(State_Cumulative, updateToThisDate, 28, 7, nFirstCols = 3,
                                             tibbleName = paste("State", theType, "Cumulative", sep=""),
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)
    if (is_null(County_leaf)) {
      County_Cumulative_A7 <- NULL
    } else {
      County_Cumulative_A7 <- movingAverageData(County_Cumulative, updateToThisDate, 28, 7, nFirstCols = 3,
                                                tibbleName = paste("County", theType, "Cumulative", sep=""),
                                                traceThisRoutine = traceThisRoutine,
                                                prepend = myPrepend)
      if (traceThisRoutine) {
        names_p <- paste(names(County_Cumulative_A7)[1:5])
        cat(file = stderr(), myPrepend, "County_Cumulative_A7 names:", names_p, "...\n")
      }
    }
    if (computeNew) {
      US_New_A7 <- movingAverageFromCumulative(US_Cumulative, updateToThisDate,
                                               "US", "New", nDays = 28, nAvg = 7, nFirst = 3,
                                               traceThisRoutine = traceThisRoutine,
                                               prepend = myPrepend)
      State_New_A7 <- movingAverageFromCumulative(State_Cumulative, updateToThisDate,
                                                  "State", "New", nDays = 28, nAvg = 7, nFirst = 3,
                                                  traceThisRoutine = traceThisRoutine,
                                                  prepend = myPrepend)
      if (is_null(County_leaf)) {
        County_New_A7 <- NULL
      } else {
        County_New_A7  <- movingAverageFromCumulative(County_Cumulative, updateToThisDate,
                                                      "County", "New", nDays = 28, nAvg = 7, nFirst = 3,
                                                      traceThisRoutine = traceThisRoutine,
                                                      prepend = myPrepend)
        if (traceThisRoutine) {
          names_p <- paste(names(County_New_A7)[1:5])
          cat(file = stderr(), myPrepend, "County_New_A7 names:", names_p, "...\n")
        }
      }
    } else {
      US_New_A7 <- NULL
      State_New_A7 <- NULL
      County_New_A7 <- NULL
    }
  } else {
    US_Cumulative_A7 <- NULL
    State_Cumulative_A7 <- NULL
    County_Cumulative_A7 <- NULL
    US_New_A7 <- NULL
    State_New_A7 <- NULL
    County_New_A7 <- NULL
  }
  

if (traceThisRoutine) {
  cat(file = stderr(), prepend, "Leaving loadATypeOfData\n")
}
  
  list(US_C = US_Cumulative, US_N = US_New,
       US_C_A = US_Cumulative_A7, US_N_A = US_New_A7,
       State_C = State_Cumulative, State_N = State_New,
       State_C_A = State_Cumulative_A7, State_N_A = State_New_A7,
       County_C = County_Cumulative, County_N = County_New,
       County_C_A = County_Cumulative_A7, County_N_A = County_New_A7)
}

loadUSConfirmedData <- function() {
  AllConfirmedData <- loadATypeOfData("US_Confirmed", "US_State_Confirmed",
                                      "US_County_Confirmed", "Confirmed")
  
  US_Confirmed_Cumulative <<- AllConfirmedData$US_C %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_Confirmed_Cumulative_A7 <<- AllConfirmedData$US_C_A %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_State_Confirmed_Cumulative <<- AllConfirmedData$State_C %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_State_Confirmed_Cumulative_A7 <<- AllConfirmedData$State_C_A %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_County_Confirmed_Cumulative <<- AllConfirmedData$County_C %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_County_Confirmed_Cumulative_A7 <<- AllConfirmedData$County_C_A %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_Confirmed_New_A7 <<- AllConfirmedData$US_N_A %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_State_Confirmed_New_A7 <<- AllConfirmedData$State_N_A %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_County_Confirmed_New_A7 <<- AllConfirmedData$County_N_A %>%
    filter(!str_detect(Combined_Key, "Princess"))
}

normalizeByPopulation <- function(aTibble) {
  if ("Combined_Key" %in% names(aTibble)) {
    returnMe <- US_Population %>%
      select(Combined_Key, Population) %>%
      inner_join(aTibble, by = "Combined_Key") %>%
      mutate(across(matches(".*2.$"), ~ .x * 100000 / Population))
  } else {
    errorCondition("no 'Combined_Key' column in aTibble??")
    returnMe <- aTibble
  }
  return(returnMe)
}

loadUSDeathsData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSDeathsData (in loadAllUSData.R)\n")
  }
  AllDeathsData <- loadATypeOfData("US_Deaths", "US_State_Deaths", "US_County_Deaths", "Deaths",
                                   traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  US_Deaths_Cumulative <<- AllDeathsData$US_C
  US_Deaths_Cumulative_A7 <<- AllDeathsData$US_C_A
  US_State_Deaths_Cumulative <<- AllDeathsData$State_C
  US_State_Deaths_Cumulative_A7 <<- AllDeathsData$State_C_A
  US_County_Deaths_Cumulative <<- AllDeathsData$County_C
  US_County_Deaths_Cumulative_A7 <<- AllDeathsData$County_C_A
  US_Deaths_New <<- AllDeathsData$US_N
  US_Deaths_New_A7 <<- AllDeathsData$US_N_A
  US_State_Deaths_New <<- AllDeathsData$State_N
  US_State_Deaths_New_A7 <<- AllDeathsData$State_N_A
  US_County_Deaths_New <<- AllDeathsData$County_N
  US_County_Deaths_New_A7 <<- AllDeathsData$County_N_A
  
  US_Deaths_Per100K_Cumulative <<- normalizeByPopulation(US_Deaths_Cumulative)
  US_Deaths_Per100K_Cumulative_A7 <<- normalizeByPopulation(US_Deaths_Cumulative_A7)
  US_State_Deaths_Per100K_Cumulative <<- normalizeByPopulation(US_State_Deaths_Cumulative)
  US_State_Deaths_Per100K_Cumulative_A7 <<- normalizeByPopulation(US_State_Deaths_Cumulative_A7)
  US_County_Deaths_Per100K_Cumulative <<- normalizeByPopulation(US_County_Deaths_Cumulative)
  US_County_Deaths_Per100K_Cumulative_A7 <<- normalizeByPopulation(US_County_Deaths_Cumulative_A7)
  US_Deaths_Per100K_New <<- normalizeByPopulation(US_Deaths_New)
  US_Deaths_Per100K_New_A7 <<- normalizeByPopulation(US_Deaths_New_A7)
  US_State_Deaths_Per100K_New <<- normalizeByPopulation(US_State_Deaths_New)
  US_State_Deaths_Per100K_New_A7 <<- normalizeByPopulation(US_State_Deaths_New_A7)
  US_County_Deaths_Per100K_New <<- normalizeByPopulation(US_County_Deaths_New)
  US_County_Deaths_Per100K_New_A7 <<- normalizeByPopulation(US_County_Deaths_New_A7)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "...created Per100K files\n")
    cat(file = stderr(), prepend, "Exiting loadUSDeathsData (loadAllUSData.R)\n")
  }
}

loadUSTestResultsData <- function() {
  AllTestResultsData <- loadATypeOfData("US_Total_Test_Results", "US_State_Total_Test_Results",
                                        NULL, "Total_Test_Results")

  US_People_Tested <<- AllTestResultsData$US_C
  US_State_People_Tested <<- AllTestResultsData$State_C
  US_People_Tested_G7 <<- AllTestResultsData$US_N_A
  US_State_People_Tested_G7 <<- AllTestResultsData$State_N_A
  US_People_Tested_A7 <<- AllTestResultsData$US_C_A
  US_State_People_Tested_A7 <<- AllTestResultsData$State_C_A
}

loadAllUSData <- function() {
  aDate = today("EST")

  US_Population <<- read_csv("./DATA/US_Population.csv",
                            col_types=cols(FIPS = col_double(),
                                           Province_State = col_character(),
                                           Combined_Key = col_character(),
                                           CountyName = col_character(),
                                           Population = col_double()))
  
  loadUSVaccinationData()

  loadUSConfirmedData()
  
  loadUSDeathsData()
  
  loadUSPeopleTestedData()
  
  loadUSHospitalizationRateData()
  
  loadUSIncidentRateData()

  loadUSMortalityRateData(aDate)

  loadUSPeopleHospitalizedData()

  loadUSTestingRateData()

  CountiesByState <<- US_County_Confirmed %>%
    mutate(State = Province_State, County = Admin2, .keep="none") %>%
    filter(str_detect(County, "Out of", negate=TRUE)) %>%
    filter(str_detect(County, "Unassigned", negate=TRUE)) %>%
    filter(str_detect(County, "Dukes and Nantucket", negate=TRUE)) %>%
    filter(str_detect(County, "Michigan Department of Corrections", negate=TRUE)) %>%
    filter(str_detect(County, "Federal Correctional Institution", negate=TRUE)) %>%
    filter(str_detect(County, "Central Utah", negate=TRUE)) %>%
    filter(str_detect(County, "Southeast Utah", negate=TRUE)) %>%
    filter(str_detect(County, "Southwest Utah", negate=TRUE)) %>%
    filter(str_detect(County, "TriCounty", negate=TRUE)) %>%
    filter(str_detect(County, "Weber-Morgan", negate=TRUE))

  States <<- unique(CountiesByState$State)
}
