library(tidyverse)
library(lubridate)

source("./computeNewAndGrowth.R")
source("./mostRecentDataDate.R")
source("./updateTimeSeriesDataFilesAsNecessary.R")
source("./loadUSPeopleTestedData.R")
source("./loadUSIncidentRateData.R")
source("./loadUSMortalityRateData.R")
source("./loadUSTestingRateData.R")
source("./loadUSVaccinationData.R")

loadATypeOfDataY <- function(theType, colTypes, computeCounty,
                            computeNew, computeAvg, computePercent,
                            traceThisRoutine = FALSE, prepend = "") {
  #####################################
  #  Functions local to this routine  #
  #####################################
  readLeaf <- function(aLeaf, colTypes) {
    aPath <- paste("./DATA/", aLeaf, sep = "")
    aTibble <- read_csv(aPath, col_types = colTypes) %>%
      filter(!str_detect(Combined_Key, "Princess"))
  }

  rem1000 <- function(n) {
    as.integer(n - 1000 * floor(n / 1000))
  }

  newFromCumulative <- function(inputTibble, updateToThisDate,
                                theScope, theType,
                                nDays = 35,
                                traceThisRoutine = FALSE, prepend = "") {
    myPrepend <- paste(prepend, "  ", sep = "")
    if (traceThisRoutine) {
      cat(file = stderr(), prepend, "Entered newFromCumulative\n")
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

  # This routine for debugging only
  dumpLocaleData <- function(aTibble, aMessage, CombinedKeyValue = "Massachusetts, US") {
    print(paste(CombinedKeyValue, aMessage))
    theData <- filter(aTibble, Combined_Key == {CombinedKeyValue})
    theLength <- dim(theData)[2]
    print(paste("  Length:", theLength))
    print(paste("  First cols:", paste(names(theData)[1:3])))
    print(paste("  First data:", theData[1,1], theData[1,2], theData[1,3]))
    print(paste("  Last cols:", paste(names(theData)[(theLength - 2):theLength])))
    print(digits = 1, paste("  Last data:", as.integer(theData[1,theLength - 9]),
                            as.integer(theData[1,theLength - 8]),
                            as.integer(theData[1,theLength - 7]),
                            as.integer(theData[1,theLength - 6]),
                            as.integer(theData[1,theLength - 5]),
                            as.integer(theData[1,theLength - 4]),
                            as.integer(theData[1,theLength - 3]),
                            as.integer(theData[1,theLength - 2]),
                            as.integer(theData[1,theLength - 1]),
                            as.integer(theData[1,theLength])))
    
  }

  ######################################
  #      Mainline of  this routine     #
  ######################################

  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadATypeOfDataY\n")
  }

  updateToThisDate <- expectedLatestUpdateDataDate()
  updateTimeSeriesDataFilesAsNecessary()
  
  US_leaf <- paste("US_", theType, ".csv", sep = "")
  State_leaf <- paste("US_State_", theType, ".csv", sep = "")
  if (computeCounty) {
    County_leaf <- paste("US_County_", theType, ".csv", sep = "")
  } else {
    County_leaf <- NA
  }
  
  US_Cumulative    <- readLeaf(US_leaf, colTypes)
  State_Cumulative <- readLeaf(State_leaf, colTypes)
  if (computeCounty) {
    County_Cumulative <- readLeaf(County_leaf, myCountyTSColTypes())
    if (traceThisRoutine) {
      names_p <- paste(names(County_Cumulative)[1:5])
      cat(file = stderr(), myPrepend, "County_Cumulative names:", names_p, "...\n")
    }
  } else {
    County_Cumulative <- NULL
  }

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Before if(computeNew) block\n")
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

    # dumpLocaleData(State_New, "State_New")

    if (computeCounty) {
      County_New <- newFromCumulative(County_Cumulative, updateToThisDate,
                                      "County", theType,
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      if (traceThisRoutine) {
        names_p <- paste(names(County_New)[1:5])
        cat(file = stderr(), myPrepend, "County_New names:", names_p, "...\n")
      }
    } else {
      County_New <- NULL
    }
  } else {
    US_New <- NULL
    State_New <- NULL
    County_New <- NULL
  }

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Before if(computeAvg) block\n")
  }

  if (computeAvg) {
    US_Cumulative_A7 <- movingAverageData(US_Cumulative, updateToThisDate, 28, 7,
                                          tibbleName = paste("US", theType, "Cumulative", sep=""),
                                          traceThisRoutine = traceThisRoutine,
                                          prepend = myPrepend)
    State_Cumulative_A7 <- movingAverageData(State_Cumulative, updateToThisDate, 28, 7,
                                             tibbleName = paste("State", theType, "Cumulative", sep=""),
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)

    # dumpLocaleData(State_Cumulative_A7, "State_Cumulative_A7")

    if (computeCounty) {
      County_Cumulative_A7 <- movingAverageData(County_Cumulative, updateToThisDate, 28, 7,
                                                tibbleName = paste("County", theType, "Cumulative", sep=""),
                                                traceThisRoutine = traceThisRoutine,
                                                prepend = myPrepend)
      if (traceThisRoutine) {
        names_p <- paste(names(County_Cumulative_A7)[1:5])
        cat(file = stderr(), myPrepend, "County_Cumulative_A7 names:", names_p, "...\n")
      }
    } else {
      County_Cumulative_A7 <- NULL
    }

    if (computeNew) {
      US_G7 <- movingAverageGrowth(US_Cumulative, updateToThisDate, 28, 7,
                                   tibbleName = paste("US", theType, "Cumulative", sep = ""),
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
      
      State_G7 <- movingAverageGrowth(State_Cumulative, updateToThisDate, 28, 7,
                                      tibbleName = paste("State", theType, "Cumulative", sep = ""),
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      
      # dumpLocaleData(State_G7, "State_G7")
      
      if (computeCounty) {
        County_G7 <- movingAverageGrowth(County_Cumulative, updateToThisDate, 28, 7,
                                         tibbleName = paste("County", theType, "Cumulative", sep = ""),
                                         traceThisRoutine = traceThisRoutine,
                                         prepend = myPrepend)
        if (traceThisRoutine) {
          names_p <- paste(names(County_G7)[1:5])
          cat(file = stderr(), myPrepend, "County_G7 names:", names_p, "...\n")
        }
      } else {
        County_G7 <- NULL
      }
    } else {
      US_G7 <- NULL
      State_G7 <- NULL
      County_G7 <- NULL
    }
  } else {
    US_Cumulative_A7 <- NULL
    State_Cumulative_A7 <- NULL
    County_Cumulative_A7 <- NULL
    US_G7 <- NULL
    State_G7 <- NULL
    County_G7 <- NULL
    US_G7 <- NULL
    State_G7 <- NULL
    County_G7 <- NULL
  }
  
  if (computePercent) {
    pop_Data <- US_Population %>%
      mutate(FIPSREM = rem1000(FIPS), .keep = "all") %>% # New column is remainder of FIPS / 1000
      filter(FIPSREM == 0) %>%                           # This selects US & States
      select(-FIPSREM) %>%                               # Discard that column
      select(Combined_Key, Population)

    US_PopCumulative <- inner_join(pop_Data, US_Cumulative, by = "Combined_Key")
    State_PopCumulative <- inner_join(pop_Data, State_Cumulative, by = "Combined_Key")

    usDims = dim(US_PopCumulative)
    usStDm = dim(State_PopCumulative)

    US_CumulativePcts <- US_PopCumulative %>%
      mutate(across(matches(".*2.$"), ~ 100.0 * .x / Population))
    State_CumulativePcts <- State_PopCumulative %>%
      mutate(across(matches(".*2.$"), ~ 100.0 * .x / Population))

    getNAvgs <- 28

    US_CumulativePcts_A7 <- movingAverageData(US_CumulativePcts,
                                              updateToThisDate,
                                              getNAvgs, 7,
                                              tibbleName="US_CumulativePcts",
                                              traceThisRoutine = traceThisRoutine,
                                              prepend = myPrepend)
    State_CumulativePcts_A7 <- movingAverageData(State_CumulativePcts,
                                                    updateToThisDate,
                                                    getNAvgs, 7,
                                                    tibbleName="State_CumulativePcts",
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
  } else {
    US_CumulativePcts <- NULL
    State_CumulativePcts <- NULL
    US_CumulativePcts_A7 <- NULL
    State_CumulativePcts_A7 <- NULL
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadATypeOfDataY\n")
  }
  
  list(US_C = US_Cumulative, US_C_P = US_CumulativePcts,
       US_C_A = US_Cumulative_A7, US_C_PA7 = US_CumulativePcts_A7,
       US_N = US_New, US_N_A = US_G7, US_G = US_G7,
       State_C = State_Cumulative, State_C_P = State_CumulativePcts,
       State_C_A = State_Cumulative_A7, State_C_PA7 = State_CumulativePcts_A7,
       State_N = State_New, State_N_A = State_G7, State_G = State_G7,
       County_C = County_Cumulative,
       County_C_A = County_Cumulative_A7, County_N_A = County_G7, County_G = County_G7,
       County_N = County_New)
}

loadATypeOfData <- function(theType, computeCounty,
                            computeNew, computeAvg,
                            hasProvState = TRUE,
                            traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadATypeOfData\n")
  }
  
  hasPercent = FALSE
  if (hasProvState) {
    colTypes <- myTSColTypes()
  } else {
    colTypes <- justCKColTypes()
  }

  result <- loadATypeOfDataY(theType, colTypes, computeCounty,
                             computeNew, computeAvg, hasPercent,
                             traceThisRoutine = traceThisRoutine, prepend = myPrepend)
    
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadATypeOfData\n")
  }
 return(result)
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

loadUSConfirmedData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSConfirmedData\n")
  }
  
  allConfirmedData <- loadATypeOfData("Confirmed", TRUE,
                              TRUE, TRUE,
                              traceThisRoutine = FALSE,
                              prepend = myPrepend)
  
  US_Confirmed <<- allConfirmedData$US_C
  US_State_Confirmed <<- allConfirmedData$State_C
  US_County_Confirmed <<- allConfirmedData$County_C

  US_Confirmed_A7 <<- allConfirmedData$US_C_A
  US_State_Confirmed_A7 <<- allConfirmedData$State_C_A
  US_County_Confirmed_A7 <<- allConfirmedData$County_C_A

  US_Confirmed_G7 <<- allConfirmedData$US_N_A
  US_State_Confirmed_G7 <<- allConfirmedData$State_N_A
  US_County_Confirmed_G7 <<- allConfirmedData$County_N_A

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSConfirmedData\n")
  }
}

loadUSDeathsData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSDeathsData (in loadAllUSData.R)\n")
  }

  AllDeathsData <- loadATypeOfData("Deaths", TRUE,
                                   TRUE, TRUE,
                                   traceThisRoutine = FALSE,
                                   prepend = myPrepend)
  
  US_Deaths <<- AllDeathsData$US_C
  US_Deaths_A7 <<- AllDeathsData$US_C_A
  US_State_Deaths <<- AllDeathsData$State_C
  US_State_Deaths_A7 <<- AllDeathsData$State_C_A
  US_County_Deaths <<- AllDeathsData$County_C
  US_County_Deaths_A7 <<- AllDeathsData$County_C_A
  US_Deaths_New <<- AllDeathsData$US_N
  US_Deaths_G7 <<- AllDeathsData$US_N_A
  US_State_Deaths_New <<- AllDeathsData$State_N
  US_State_Deaths_G7 <<- AllDeathsData$State_N_A
  US_County_Deaths_New <<- AllDeathsData$County_N
  US_County_Deaths_G7 <<- AllDeathsData$County_N_A
  
  US_Deaths_Per100K <<- normalizeByPopulation(US_Deaths)
  US_Deaths_Per100K_A7 <<- normalizeByPopulation(US_Deaths_A7)
  US_State_Deaths_Per100K <<- normalizeByPopulation(US_State_Deaths)
  US_State_Deaths_Per100K_A7 <<- normalizeByPopulation(US_State_Deaths_A7)
  US_County_Deaths_Per100K <<- normalizeByPopulation(US_County_Deaths)
  US_County_Deaths_Per100K_A7 <<- normalizeByPopulation(US_County_Deaths_A7)
  US_Deaths_Per100K_New <<- normalizeByPopulation(US_Deaths_New)
  US_Deaths_Per100K_G7 <<- normalizeByPopulation(US_Deaths_G7)
  US_State_Deaths_Per100K_New <<- normalizeByPopulation(US_State_Deaths_New)
  US_State_Deaths_Per100K_G7 <<- normalizeByPopulation(US_State_Deaths_G7)
  US_County_Deaths_Per100K_New <<- normalizeByPopulation(US_County_Deaths_New)
  US_County_Deaths_Per100K_G7 <<- normalizeByPopulation(US_County_Deaths_G7)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "...created Per100K files\n")
    cat(file = stderr(), prepend, "Leaving loadUSDeathsData\n")
  }
}

loadUSTestResultsData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSTestResultsData\n")
  }
  
  AllTestResultsData <- loadATypeOfData("Total_Test_Results", FALSE,
                                        TRUE, TRUE,
                                        hasProvState = FALSE,
                                        traceThisRoutine = FALSE, prepend = myPrepend)

  US_People_Tested <<- AllTestResultsData$US_C
  US_State_People_Tested <<- AllTestResultsData$State_C
  US_People_Tested_G7 <<- AllTestResultsData$US_N_A
  US_State_People_Tested_G7 <<- AllTestResultsData$State_N_A
  US_People_Tested_A7 <<- AllTestResultsData$US_C_A
  US_State_People_Tested_A7 <<- AllTestResultsData$State_C_A

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSTestResultsData\n")
  }
}

loadAllUSData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadAllUSData (in loadAllUSData.R)\n")
  }

  aDate = today("EST")

  US_Population <<- read_csv("./DATA/US_Population.csv",
                            col_types = populationColTypes())
  US_State_Population_Est <<- read_csv("./DATA/US_State_Population_Est.csv",
                                       col_types = estPopulationColTypes())

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after read US_State_Population_Est\n")
  }
  
  updateTimeSeriesDataFilesAsNecessary(traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)
  updateStateLevelSerializedDataFilesAsNecessary(traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend)
  
  loadUSVaccinationData(traceThisRoutine = traceThisRoutine, prepend = myPrepend)

  loadUSConfirmedData(traceThisRoutine = traceThisRoutine, prepend = myPrepend)

  loadUSDeathsData(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  loadUSTestResultsData(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after loadUSTestResultsData\n")
  }
  
  loadUSIncidentRateData()

  loadUSMortalityRateData(aDate)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after loadUSMortalityRateData\n")
  }
  
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

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadAllUSData (in loadAllUSData.R)\n")
  }
}
