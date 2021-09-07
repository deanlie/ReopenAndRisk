library(tidyverse)
library(lubridate)

source("./computeNewAndGrowth.R")
source("./mostRecentDataDate.R")
source("./updateTimeSeriesDataFilesAsNecessary.R")
source("./loadUSMortalityRateData.R")
source("./loadUSTestingRateData.R")

loadATypeOfData <- function(theType, colTypes, stateColTypes, computeCounty,
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
                                theScope, theType, nDays,
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

  ######################################
  #      Mainline of  this routine     #
  ######################################

  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadATypeOfData\n")
  }
  
  results <- list(US_C = NULL,      US_C_P = NULL,
                  US_C_A = NULL,    US_C_PA7 = NULL,
                  US_N = NULL,      US_N_A = NULL,
                  State_C = NULL,   State_C_P = NULL,
                  State_C_A = NULL, State_C_PA7 = NULL,
                  State_N = NULL,   State_N_A = NULL,
                  County_C = NULL,  County_C_A = NULL,
                  County_N = NULL,  County_N_A = NULL)

  updateToThisDate <- expectedLatestUpdateDataDate()
  updateTimeSeriesDataFilesAsNecessary()
  
  US_leaf <- paste("US_", theType, ".csv", sep = "")
  State_leaf <- paste("US_State_", theType, ".csv", sep = "")
  if (computeCounty) {
    County_leaf <- paste("US_County_", theType, ".csv", sep = "")
  } else {
    County_leaf <- NA
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "before readLeaf", US_leaf, "...\n")
  }
  results$US_C  <- readLeaf(US_leaf, colTypes) # US_Cumulative
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "before readLeaf", State_leaf, "...\n")
  }
  results$State_C <- readLeaf(State_leaf, stateColTypes)
  if (computeCounty) {
    results$County_C <- readLeaf(County_leaf, myCountyTSColTypes())
    if (traceThisRoutine) {
      names_p <- paste(names(results$County_C)[1:5])
      cat(file = stderr(), myPrepend, "results$County_C names:", names_p, "...\n")
    }
  }

  getNAvgs <- 28
  nDaysData <- 35
  averageOverDays <- 7

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Before if(computeNew) block\n")
  }

  if (computeNew) {
    results$US_N <- newFromCumulative(results$US_C, updateToThisDate,
                                "US", theType, nDaysData,
                                traceThisRoutine = traceThisRoutine,
                                prepend = myPrepend)
    results$State_N <- newFromCumulative(results$State_C, updateToThisDate,
                                   "State", theType, nDaysData,
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)

    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$State_N, "results$State_N",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }

    if (computeCounty) {
      results$County_N <- newFromCumulative(results$County_C, updateToThisDate,
                                      "County", theType,
                                      nDays = nDaysData,
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      if (traceThisRoutine) {
        conciseEndsOfTibbleRow(results$County_N, "results$County_N",
                               nFirst = 4, nLast = 4,
                               traceThisRoutine = traceThisRoutine,
                               prepend = myPrepend)
      }
    }
  }

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Before if(computeAvg) block\n")
  }

  if (computeAvg) {
    results$US_C_A <- movingAverageData(results$US_C, updateToThisDate, getNAvgs, averageOverDays,
                                          tibbleName = paste("US", theType, "Cumulative", sep=""),
                                          traceThisRoutine = traceThisRoutine,
                                          prepend = myPrepend)
    results$State_C_A <- movingAverageData(results$State_C, updateToThisDate, getNAvgs, averageOverDays,
                                             tibbleName = paste("State", theType, "Cumulative", sep=""),
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)

    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$State_C_A, "State_Cumulative_A7",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }

    if (computeCounty) {
      results$County_C_A <- movingAverageData(results$County_C, updateToThisDate, getNAvgs, averageOverDays,
                                                tibbleName = paste("County", theType, "Cumulative", sep=""),
                                                traceThisRoutine = traceThisRoutine,
                                                prepend = myPrepend)

      if (traceThisRoutine) {
        conciseEndsOfTibbleRow(results$County_C_A, "results$County_C_A",
                               nFirst = 4, nLast = 4,
                               traceThisRoutine = traceThisRoutine,
                               prepend = myPrepend)
      }
    }

    if (computeNew) {
      results$US_N_A <- movingAverageGrowth(results$US_C, updateToThisDate, getNAvgs, averageOverDays,
                                   tibbleName = paste("US", theType, "Cumulative", sep = ""),
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
      
      results$State_N_A <- movingAverageGrowth(results$State_C, updateToThisDate, getNAvgs, averageOverDays,
                                      tibbleName = paste("State", theType, "Cumulative", sep = ""),
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      
      if (traceThisRoutine) {
        conciseEndsOfTibbleRow(results$State_N_A, "results$State_N_A",
                               nFirst = 4, nLast = 4,
                               traceThisRoutine = traceThisRoutine,
                               prepend = myPrepend)
      }
      
      if (computeCounty) {
        results$County_N_A <- movingAverageGrowth(results$County_C, updateToThisDate, getNAvgs, averageOverDays,
                                         tibbleName = paste("County", theType, "Cumulative", sep = ""),
                                         traceThisRoutine = traceThisRoutine,
                                         prepend = myPrepend)

        if (traceThisRoutine) {
          conciseEndsOfTibbleRow(results$County_N_A, "results$County_N_A",
                                 nFirst = 4, nLast = 4,
                                 traceThisRoutine = traceThisRoutine,
                                 prepend = myPrepend)
        }
      }
    }
  }
  
  if (computePercent) {
    pop_Data <- US_Population %>%
      mutate(FIPSREM = rem1000(FIPS), .keep = "all") %>% # New column is remainder of FIPS / 1000
      filter(FIPSREM == 0) %>%                           # This selects US & States
      select(-FIPSREM) %>%                               # Discard that column
      select(Combined_Key, Population)

    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$US_C, "results$US_C",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }
      
    US_PopCumulative <- inner_join(pop_Data, results$US_C, by = "Combined_Key")

    #OUCH
    if (traceThisRoutine) {
        conciseEndsOfTibbleRow(US_PopCumulative, "US_PopCumulative",
                               nFirst = 4, nLast = 4,
                               traceThisRoutine = traceThisRoutine,
                               prepend = myPrepend)
    }
    
    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$State_C, "results$State_C",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }
    
    State_PopCumulative <- inner_join(pop_Data, results$State_C, by = "Combined_Key")

    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(State_PopCumulative, "State_PopCumulative",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }

    usDims = dim(US_PopCumulative)
    usStDm = dim(State_PopCumulative)

    results$US_C_P <- US_PopCumulative %>%
      mutate(across(matches(".*2.$"), ~ 100.0 * .x / Population))
    results$State_C_P <- State_PopCumulative %>%
      mutate(across(matches(".*2.$"), ~ 100.0 * .x / Population))

    results$US_C_PA7 <- movingAverageData(results$US_C_P,
                                              updateToThisDate,
                                              getNAvgs, averageOverDays,
                                              tibbleName="results$US_C_P",
                                              traceThisRoutine = traceThisRoutine,
                                              prepend = myPrepend)
    
    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$US_C_PA7, "results$US_C_PA7",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }

    results$State_C_PA7 <- movingAverageData(results$State_C_P,
                                                    updateToThisDate,
                                                    getNAvgs, averageOverDays,
                                                    tibbleName="results$State_C_PA7",
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
    
    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$State_C_PA7, "results$State_C_PA7",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadATypeOfData\n")
  }

  return(results)
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
  
  computeCounty <- TRUE
  computeNew <- TRUE
  computeAvg <- TRUE
  computePercent <- FALSE
  
  allConfirmedData <- loadATypeOfData("Confirmed",
                                      myTSColTypes(), myTSColTypes(),
                                      computeCounty, computeNew,
                                      computeAvg, computePercent,
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

  return(allConfirmedData)
}

loadUSDeathsData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSDeathsData (in loadAllUSData.R)\n")
  }
  
  computeCounty <- TRUE
  computeNew <- TRUE
  computeAvg <- TRUE
  computePercent <- FALSE
  
  allDeathsData <- loadATypeOfData("Deaths",
                                   myTSColTypes(), myTSColTypes(),
                                   computeCounty, computeNew,
                                   computeAvg, computeAvg,
                                   traceThisRoutine = FALSE,
                                   prepend = myPrepend)
  
  US_Deaths <<- allDeathsData$US_C
  US_Deaths_A7 <<- allDeathsData$US_C_A
  US_State_Deaths <<- allDeathsData$State_C
  US_State_Deaths_A7 <<- allDeathsData$State_C_A
  US_County_Deaths <<- allDeathsData$County_C
  US_County_Deaths_A7 <<- allDeathsData$County_C_A
  US_Deaths_New <<- allDeathsData$US_N
  US_Deaths_G7 <<- allDeathsData$US_N_A
  US_State_Deaths_New <<- allDeathsData$State_N
  US_State_Deaths_G7 <<- allDeathsData$State_N_A
  US_County_Deaths_New <<- allDeathsData$County_N
  US_County_Deaths_G7 <<- allDeathsData$County_N_A
  
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
  
  return(allDeathsData)
}

loadUSTestResultsData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSTestResultsData\n")
  }

  computeCounty <- FALSE
  computeNew <- TRUE
  computeAvg <- TRUE
  computePercent <- FALSE
  
  allTestResultsData <- loadATypeOfData("Total_Test_Results",
                                        justCKColTypes(), justCKColTypes(),
                                        computeCounty, computeNew,
                                        computeAvg, computePercent,
                                        traceThisRoutine = FALSE,
                                        prepend = myPrepend)

  US_People_Tested <<- allTestResultsData$US_C
  US_State_People_Tested <<- allTestResultsData$State_C
  US_People_Tested_G7 <<- allTestResultsData$US_N_A
  US_State_People_Tested_G7 <<- allTestResultsData$State_N_A
  US_People_Tested_A7 <<- allTestResultsData$US_C_A
  US_State_People_Tested_A7 <<- allTestResultsData$State_C_A

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSTestResultsData\n")
  }
  
  return(allTestResultsData)
}

loadUSVaccinationData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSVaccinationData\n")
  }
  
  computeCounty <- FALSE
  computeNew <- FALSE
  computeAvg <- TRUE
  computePercent <- TRUE
  
  allVaccinationData <- loadATypeOfData("Vaccinations",
                                        vaccColTypes(), vaccColTypes(),
                                        computeCounty, computeNew,
                                        computeAvg, computePercent,
                                        traceThisRoutine = FALSE,
                                        prepend = myPrepend)
  
  US_Vaccination_Pcts <<- allVaccinationData$US_C_P
  US_State_Vaccination_Pcts <<- allVaccinationData$State_C_P
  US_Vaccination_Pcts_A7 <<- allVaccinationData$US_C_PA7
  US_State_Vaccination_Pcts_A7 <<- allVaccinationData$State_C_PA7
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSVaccinationData\n")
  }
  
  return(allVaccinationData)
}


loadUSIncidentRateData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSIncidentRateData\n")
  }
  
  computeCounty <- FALSE
  computeNew <- TRUE
  computeAvg <- TRUE
  computePercent <- TRUE
  
  allIncidentRateData <- loadATypeOfData("Incident_Rate",
                                         myTSColTypes(), justCKColTypes(),
                                         computeCounty, computeNew,
                                         computeAvg, computePercent,
                                         traceThisRoutine = traceThisRoutine,
                                         prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSIncidentRateData\n")
  }

  US_Incident_Rate <<- allIncidentRateData$US_C
  US_State_Incident_Rate <<- allIncidentRateData$State_C

  US_Incident_Rate_A7 <<- allIncidentRateData$US_C_A
  US_State_Incident_Rate_A7 <<- allIncidentRateData$State_C_A

  US_Incident_Rate_G7 <<- allIncidentRateData$US_N_A
  US_State_Incident_Rate_G7 <<- allIncidentRateData$State_N_A

  return(allIncidentRateData)
}
  
loadAllUSData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadAllUSData (in loadAllUSData.R)\n")
  }

  #OUCH  
  traceFlagOnEntry <- traceThisRoutine
  traceThisRoutine <- FALSE

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
  
  loadUSVaccinationData(traceThisRoutine = traceThisRoutine,
                        prepend = myPrepend)

  loadUSConfirmedData(traceThisRoutine = traceThisRoutine,
                      prepend = myPrepend)

  loadUSDeathsData(traceThisRoutine = traceThisRoutine,
                   prepend = myPrepend)
  
  loadUSTestResultsData(traceThisRoutine = traceThisRoutine,
                        prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after loadUSTestResultsData\n")
  }

  loadUSIncidentRateData(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  traceThisRoutine <- traceFlagOnEntry

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after loadUSIncidentRateData\n")
  }

  loadUSMortalityRateData(aDate)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after loadUSMortalityRateData\n")
  }

  loadUSTestingRateData()
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after loadUSTestingRateData\n")
  }
  
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
