library(tidyverse)
library(lubridate)

source("computeNewAndGrowth.R")
source("mostRecentDataDate.R")
source("updateTimeSeriesDataFilesAsNecessary.R")
source("updateStateLevelSerializedDataFiles.R")
source("columnUtilities.R")
source("diagnosticRoutines.R")

loadATypeOfData <- function(staticDataQ, theType, colTypes, stateColTypes,
                            computeCounty = FALSE,
                            computeNew = FALSE,
                            computeAvg = FALSE,
                            computePercent = FALSE,
                            traceThisRoutine = FALSE, prepend = "") {
  #####################################
  #  Functions local to this routine  #
  #####################################
  readLeaf <- function(staticDataQ, aLeaf, colTypes) {
    if (staticDataQ) {
      theDirectory <- "./DATA/STATIC/"
    } else {
      theDirectory <- "./DATA/"
    }
    aPath <- paste(theDirectory, aLeaf, sep = "")
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
    
    # OUCH
    traceFlagOnEntry <- traceThisRoutine
    traceThisRoutine <- FALSE

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
    if (traceFlagOnEntry) {
      cat(file = stderr(), prepend, "Leaving newFromCumulative\n")
    }
    outputTibble <- outputList$new
  }

  ######################################
  #      Mainline of  this routine     #
  ######################################

  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadATypeOfData, staticDataQ =", staticDataQ, "\n")
  }
  
  traceFlagOnEntry <- traceThisRoutine
  
  results <- list(US = NULL,         US_Pct = NULL,
                  US_Avg = NULL,     US_PctAvg = NULL,
                  US_New = NULL,     US_NewAvg = NULL,
                  State = NULL,      State_Pct = NULL,
                  State_Avg = NULL,  State_PctAvg = NULL,
                  State_New = NULL,  State_NewAvg = NULL,
                  County = NULL,     County_Avg = NULL,
                  County_New = NULL, County_NewAvg = NULL)

  updateToThisDate <- expectedLatestUpdateDataDate()

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
  results$US <- readLeaf(staticDataQ, US_leaf, colTypes) # US_Cumulative
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "before readLeaf", State_leaf, "...\n")
  }
  results$State <- readLeaf(staticDataQ, State_leaf, stateColTypes)
  if (computeCounty) {
    results$County <- readLeaf(staticDataQ, County_leaf, myCountyTSColTypes())
    if (traceThisRoutine) {
      names_p <- paste(names(results$County)[1:5])
      cat(file = stderr(), myPrepend, "results$County names:", names_p, "...\n")
    }
  }

  getNAvgs <- 28
  nDaysData <- 35
  averageOverDays <- 7

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Before if(computeNew) block\n")
  }

  if (computeNew) {
    results$US_New <- newFromCumulative(results$US, updateToThisDate,
                                "US", theType, nDaysData,
                                traceThisRoutine = traceThisRoutine,
                                prepend = myPrepend)
    results$State_New <- newFromCumulative(results$State, updateToThisDate,
                                   "State", theType, nDaysData,
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)

    # OUCH
    traceThisRoutine <- FALSE

    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$State_New, "results$State_New",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }

    if (computeCounty) {
      results$County_New <- newFromCumulative(results$County, updateToThisDate,
                                      "County", theType,
                                      nDays = nDaysData,
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      if (traceThisRoutine) {
        conciseEndsOfTibbleRow(results$County_New, "results$County_New",
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
    results$US_Avg <- movingAverageData(results$US, updateToThisDate, getNAvgs, averageOverDays,
                                          tibbleName = paste("US", theType, "Cumulative", sep=""),
                                          traceThisRoutine = traceThisRoutine,
                                          prepend = myPrepend)
    results$State_Avg <- movingAverageData(results$State, updateToThisDate, getNAvgs, averageOverDays,
                                             tibbleName = paste("State", theType, "Cumulative", sep=""),
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)

    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$State_Avg, "State_Avg",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }

    if (computeCounty) {
      results$County_Avg <- movingAverageData(results$County, updateToThisDate, getNAvgs, averageOverDays,
                                                tibbleName = paste("County", theType, "Cumulative", sep=""),
                                                traceThisRoutine = traceThisRoutine,
                                                prepend = myPrepend)

      if (traceThisRoutine) {
        conciseEndsOfTibbleRow(results$County_Avg, "results$County_Avg",
                               nFirst = 4, nLast = 4,
                               traceThisRoutine = traceThisRoutine,
                               prepend = myPrepend)
      }
    }

    if (computeNew) {
      results$US_NewAvg <- movingAverageGrowth(results$US, updateToThisDate, getNAvgs, averageOverDays,
                                   tibbleName = paste("US", theType, "Cumulative", sep = ""),
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
      
      results$State_NewAvg <- movingAverageGrowth(results$State, updateToThisDate, getNAvgs, averageOverDays,
                                      tibbleName = paste("State", theType, "Cumulative", sep = ""),
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      
      if (traceThisRoutine) {
        conciseEndsOfTibbleRow(results$State_NewAvg, "results$State_NewAvg",
                               nFirst = 4, nLast = 4,
                               traceThisRoutine = traceThisRoutine,
                               prepend = myPrepend)
      }
      
      if (computeCounty) {
        results$County_NewAvg <- movingAverageGrowth(results$County, updateToThisDate, getNAvgs, averageOverDays,
                                         tibbleName = paste("County", theType, "Cumulative", sep = ""),
                                         traceThisRoutine = traceThisRoutine,
                                         prepend = myPrepend)

        if (traceThisRoutine) {
          conciseEndsOfTibbleRow(results$County_NewAvg, "results$County_NewAvg",
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
      conciseEndsOfTibbleRow(results$US, "results$US",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }
      
    US_PopCumulative <- inner_join(pop_Data, results$US, by = "Combined_Key")

    #OUCH
    if (traceThisRoutine) {
        conciseEndsOfTibbleRow(US_PopCumulative, "US_PopCumulative",
                               nFirst = 4, nLast = 4,
                               traceThisRoutine = traceThisRoutine,
                               prepend = myPrepend)
    }
    
    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$State, "results$State",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }
    
    State_PopCumulative <- inner_join(pop_Data, results$State, by = "Combined_Key")

    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(State_PopCumulative, "State_PopCumulative",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }

    usDims = dim(US_PopCumulative)
    usStDm = dim(State_PopCumulative)

    results$US_Pct <- US_PopCumulative %>%
      mutate(across(matches(".*2.$"), ~ 100.0 * .x / Population))
    results$State_Pct <- State_PopCumulative %>%
      mutate(across(matches(".*2.$"), ~ 100.0 * .x / Population))

    results$US_PctAvg <- movingAverageData(results$US_Pct,
                                              updateToThisDate,
                                              getNAvgs, averageOverDays,
                                              tibbleName="results$US_Pct",
                                              traceThisRoutine = traceThisRoutine,
                                              prepend = myPrepend)
    
    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$US_PctAvg, "results$US_PctAvg",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }

    results$State_PctAvg <- movingAverageData(results$State_Pct,
                                                    updateToThisDate,
                                                    getNAvgs, averageOverDays,
                                                    tibbleName="results$State_PctAvg",
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
    
    if (traceThisRoutine) {
      conciseEndsOfTibbleRow(results$State_PctAvg, "results$State_PctAvg",
                             nFirst = 4, nLast = 4,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
    }
  }
  
  if (traceFlagOnEntry) {
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

loadUSConfirmedData <- function(staticDataQ = FALSE, traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered loadUSConfirmedData, staticDataQ =", staticDataQ, "\n")
  }

  if (isTRUE(getOption("shiny.testmode"))) {
    cat(file = stderr(), "loadUSConfirmedData TEST MODE!\n")
    staticDataQ <- TRUE
  }

  traceThisRoutine <- FALSE
  allConfirmedData <- loadATypeOfData(staticDataQ, "Confirmed",
                                      myTSColTypes(), myTSColTypes(),
                                      computeCounty = TRUE,
                                      computeNew = TRUE,
                                      computeAvg = TRUE,
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
  
  US_Confirmed <<- allConfirmedData$US
  US_State_Confirmed <<- allConfirmedData$State
  US_County_Confirmed <<- allConfirmedData$County

  US_Confirmed_A7 <<- allConfirmedData$US_Avg
  US_State_Confirmed_A7 <<- allConfirmedData$State_Avg
  US_County_Confirmed_A7 <<- allConfirmedData$County_Avg

  US_Confirmed_G7 <<- allConfirmedData$US_NewAvg
  US_State_Confirmed_G7 <<- allConfirmedData$State_NewAvg
  US_County_Confirmed_G7 <<- allConfirmedData$County_NewAvg

  US_Confirmed_Per100K <<- normalizeByPopulation(US_Confirmed)
  US_Confirmed_Per100KAvg <<- normalizeByPopulation(US_Confirmed_A7)
  US_State_Confirmed_Per100K <<- normalizeByPopulation(US_State_Confirmed)
  US_State_Confirmed_Per100KAvg <<- normalizeByPopulation(US_State_Confirmed_A7)
  US_County_Confirmed_Per100K <<- normalizeByPopulation(US_County_Confirmed)
  US_County_Confirmed_Per100KAvg <<- normalizeByPopulation(US_County_Confirmed_A7)
  # US_Confirmed_Per100K_New <<- normalizeByPopulation(US_Confirmed_New)
  US_Confirmed_Per100K_G7 <<- normalizeByPopulation(US_Confirmed_G7)
  # US_State_Confirmed_Per100K_New <<- normalizeByPopulation(US_State_Confirmed_New)
  US_State_Confirmed_Per100K_G7 <<- normalizeByPopulation(US_State_Confirmed_G7)
  # US_County_Confirmed_Per100K_New <<- normalizeByPopulation(US_County_Confirmed_New)
  US_County_Confirmed_Per100K_G7 <<- normalizeByPopulation(US_County_Confirmed_G7)
  
  # STOP
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving loadUSConfirmedData\n")
  }

  return(allConfirmedData)
}

loadUSDeathsData <- function(staticDataQ = FALSE, traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered loadUSDeathsData, staticDataQ =", staticDataQ, "\n")
  }
  
  if (isTRUE(getOption("shiny.testmode"))) {
    cat(file = stderr(), "loadUSDeathsData TEST MODE!\n")
    staticDataQ <- TRUE
  }

  traceThisRoutine <-  FALSE
  allDeathsData <- loadATypeOfData(staticDataQ, "Deaths",
                                   myTSColTypes(), myTSColTypes(),
                                   computeCounty = TRUE,
                                   computeNew = TRUE,
                                   computeAvg = TRUE,
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
  
  US_Deaths <<- allDeathsData$US
  US_Deaths_A7 <<- allDeathsData$US_Avg
  US_State_Deaths <<- allDeathsData$State
  US_State_Deaths_A7 <<- allDeathsData$State_Avg
  US_County_Deaths <<- allDeathsData$County
  US_County_Deaths_A7 <<- allDeathsData$County_Avg
  US_Deaths_New <<- allDeathsData$US_New
  US_Deaths_G7 <<- allDeathsData$US_NewAvg
  US_State_Deaths_New <<- allDeathsData$State_New
  US_State_Deaths_G7 <<- allDeathsData$State_NewAvg
  US_County_Deaths_New <<- allDeathsData$County_New
  US_County_Deaths_G7 <<- allDeathsData$County_NewAvg
  
  US_Deaths_Per100K <<- normalizeByPopulation(US_Deaths)
  US_Deaths_Per100KAvg <<- normalizeByPopulation(US_Deaths_A7)
  US_State_Deaths_Per100K <<- normalizeByPopulation(US_State_Deaths)
  US_State_Deaths_Per100KAvg <<- normalizeByPopulation(US_State_Deaths_A7)
  US_County_Deaths_Per100K <<- normalizeByPopulation(US_County_Deaths)
  US_County_Deaths_Per100KAvg <<- normalizeByPopulation(US_County_Deaths_A7)
  US_Deaths_Per100K_New <<- normalizeByPopulation(US_Deaths_New)
  US_Deaths_Per100K_G7 <<- normalizeByPopulation(US_Deaths_G7)
  US_State_Deaths_Per100K_New <<- normalizeByPopulation(US_State_Deaths_New)
  US_State_Deaths_Per100K_G7 <<- normalizeByPopulation(US_State_Deaths_G7)
  US_County_Deaths_Per100K_New <<- normalizeByPopulation(US_County_Deaths_New)
  US_County_Deaths_Per100K_G7 <<- normalizeByPopulation(US_County_Deaths_G7)
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), myPrepend, "...created Per100K files\n")
    cat(file = stderr(), prepend, "Leaving loadUSDeathsData\n")
  }
  
  return(allDeathsData)
}

loadUSTestResultsData <- function(staticDataQ = FALSE, traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered loadUSTestResultsData, staticDataQ =", staticDataQ, "\n")
  }
  
  if (isTRUE(getOption("shiny.testmode"))) {
    cat(file = stderr(), "loadUSTestResultsData TEST MODE!\n")
    staticDataQ <- TRUE
  }

  traceThisRoutine <- FALSE
  allTestResultsData <- loadATypeOfData(staticDataQ, "Total_Test_Results",
                                        justCKColTypes(), justCKColTypes(),
                                        computeNew = TRUE,
                                        computeAvg = TRUE,
                                        traceThisRoutine = traceThisRoutine,
                                        prepend = myPrepend)

  US_People_Tested <<- allTestResultsData$US
  US_State_People_Tested <<- allTestResultsData$State
  US_People_Tested_G7 <<- allTestResultsData$US_NewAvg
  US_State_People_Tested_G7 <<- allTestResultsData$State_NewAvg
  US_People_Tested_A7 <<- allTestResultsData$US_Avg
  US_State_People_Tested_A7 <<- allTestResultsData$State_Avg

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving loadUSTestResultsData\n")
  }
  
  return(allTestResultsData)
}

loadUSVaccinationData <- function(staticDataQ = FALSE, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered loadUSVaccinationData\n")
  }

  if (isTRUE(getOption("shiny.testmode"))) {
    cat(file = stderr(), "loadUSVaccinationData TEST MODE!\n")
    staticDataQ <- TRUE
  }

  traceThisRoutine <- FALSE
  allVaccinationData <- loadATypeOfData(staticDataQ, "Vaccinations",
                                        vaccColTypes(), vaccColTypes(),
                                        computeAvg = TRUE,
                                        computePercent = TRUE,
                                        traceThisRoutine = traceThisRoutine,
                                        prepend = myPrepend)

  US_Vaccination_Pcts <<- allVaccinationData$US_Pct
  US_State_Vaccination_Pcts <<- allVaccinationData$State_Pct
  US_Vaccination_Pcts_A7 <<- allVaccinationData$US_PctAvg
  US_State_Vaccination_Pcts_A7 <<- allVaccinationData$State_PctAvg

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving loadUSVaccinationData\n")
  }

  return(allVaccinationData)
}

loadUSIncidentRateData <- function(staticDataQ = FALSE, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered loadUSIncidentRateData, staticDataQ =", staticDataQ, "\n")
  }
  
  if (isTRUE(getOption("shiny.testmode"))) {
    cat(file = stderr(), "loadUSIncidentRateData TEST MODE!\n")
    staticDataQ <- TRUE
  }

  traceThisRoutine <- FALSE
  allIncidentRateData <- loadATypeOfData(staticDataQ, "Incident_Rate",
                                         myTSColTypes(), justCKColTypes(),
                                         computeNew = TRUE,
                                         computeAvg = TRUE,
                                         computePercent = TRUE,
                                         traceThisRoutine = traceThisRoutine,
                                         prepend = myPrepend)

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving loadUSIncidentRateData\n")
  }

  US_Incident_Rate <<- allIncidentRateData$US
  US_State_Incident_Rate <<- allIncidentRateData$State

  US_Incident_Rate_A7 <<- allIncidentRateData$US_Avg
  US_State_Incident_Rate_A7 <<- allIncidentRateData$State_Avg

  US_Incident_Rate_G7 <<- allIncidentRateData$US_NewAvg
  US_State_Incident_Rate_G7 <<- allIncidentRateData$State_NewAvg

  return(allIncidentRateData)
}

loadUSTestingRateData <- function(staticDataQ = FALSE,
                                  traceThisRoutine = FALSE,
                                  prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered loadUSTestingRateData\n")
  }
  
  if (isTRUE(getOption("shiny.testmode"))) {
    cat(file = stderr(), "loadUSTestingRateData TEST MODE!\n")
    staticDataQ <- TRUE
  }
  
  computeCounty <- FALSE
  computeNew <- FALSE
  computeAvg <- FALSE
  computePercent <- FALSE
  
  allTestingRateData <- loadATypeOfData(staticDataQ, "Testing_Rate",
                                        myTSColTypes(), justCKColTypes(),
                                        computeCounty, computeNew,
                                        computeAvg, computePercent,
                                        traceThisRoutine = traceThisRoutine,
                                        prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSTestingRateData\n")
  }

  US_Testing_Rate <<- allTestingRateData$US
  US_State_Testing_Rate <<- allTestingRateData$State
}

loadUSMortalityRateData <- function(staticDataQ = FALSE,
                                  traceThisRoutine = FALSE,
                                  prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered loadUSMortalityRateData\n")
  }
  
  if (isTRUE(getOption("shiny.testmode"))) {
    cat(file = stderr(), "loadUSMortalityRateData TEST MODE!\n")
    staticDataQ <- TRUE
  }
  
  traceThisRoutine <- FALSE
  
  allMortalityRateData <- loadATypeOfData(staticDataQ, "Case_Fatality_Ratio",
                                        myTSColTypes(), justCKColTypes(),
                                        traceThisRoutine = traceThisRoutine,
                                        prepend = myPrepend)
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving loadUSMortalityRateData\n")
  }
  
  US_Mortality_Rate <<- allMortalityRateData$US
  US_State_Mortality_Rate <<- allMortalityRateData$State
}

loadAllUSData <- function(staticDataQ = FALSE, traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered loadAllUSData\n")
  }

  aDate = today("EST")

  US_Population <<- read_csv("./DATA/US_Population.csv",
                            col_types = populationColTypes())
  US_State_Population_Est <<- read_csv("./DATA/US_State_Population_Est.csv",
                                       col_types = estPopulationColTypes())

  traceThisRoutine <- FALSE
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after read US_State_Population_Est\n")
  }
  
  if (isTRUE(getOption("shiny.testmode"))) {
    cat(file = stderr(), "loadUSVaccinationData TEST MODE!\n")
    staticDataQ <- TRUE
  }
  
  if (!staticDataQ) {
    updateTimeSeriesDataFilesAsNecessary(traceThisRoutine = traceThisRoutine,
                                         prepend = myPrepend)
    updateStateLevelSerializedDataFilesAsNecessary(traceThisRoutine = traceThisRoutine,
                                                   prepend = myPrepend)
  }

  loadUSVaccinationData(staticDataQ = staticDataQ,
                        traceThisRoutine = traceThisRoutine,
                        prepend = myPrepend)

  loadUSConfirmedData(staticDataQ = staticDataQ,
                      traceThisRoutine = traceThisRoutine,
                      prepend = myPrepend)

  loadUSDeathsData(staticDataQ = staticDataQ,
                   traceThisRoutine = traceThisRoutine,
                   prepend = myPrepend)

  loadUSTestResultsData(staticDataQ = staticDataQ,
                        traceThisRoutine = traceThisRoutine,
                        prepend = myPrepend)

  loadUSIncidentRateData(staticDataQ = staticDataQ,
                         traceThisRoutine = traceThisRoutine,
                         prepend = myPrepend)

  loadUSMortalityRateData(staticDataQ = staticDataQ,
                          traceThisRoutine = traceThisRoutine,
                          prepend = myPrepend)

  loadUSTestingRateData(staticDataQ = staticDataQ,
                        traceThisRoutine = traceThisRoutine,
                        prepend = myPrepend)

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

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving loadAllUSData\n")
  }
}
