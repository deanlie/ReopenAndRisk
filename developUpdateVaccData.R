library(tidyverse)

source("downloadJHUData.R")
source("testDiscardNewDataFilewise.R")
source("updateTimeSeriesDataFilesAsNecessary.R")

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
  
  US_State_Vaccinations_As_Filed <- read_csv("./DATA/CACHE/US_State_Vacc_Test.csv", #"./DATA/US_State_Vaccinations.csv",
                                             col_types = vaccColTypes())

  # If latest date of US tibble is yesterday
  stateDataColNames <- names(US_State_Vaccinations_As_Filed)
  lastStateDataDateString <- stateDataColNames[length(stateDataColNames)]
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Last state data col is",
        lastStateDataDateString, "\n")
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
    #     Get the file version
    if (file.exists(vaccTimeSeriesDataSpecs()$PATH)) {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "We have the data locally!\n")
      }
      updateDataSource <- read_csv(vaccTimeSeriesDataSpecs()$PATH,
                                   col_types = vaccTimeSeriesDataSpecs()$COLS)
    } else {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Must go to the internet for update data!\n")
      }
      updateDataSource <- getURLFromSpecsOrStop(vaccTimeSeriesDataSpecs(),
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = "VaccTimeline")
      
    }
    #
    #   Get latest date of state vacc data file
    lastDateWeHave = lastStateDataDateString
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "To be clear, last date we have is",
          lastDateWeHave, "\n")
    }
    
    firstDateWeNeed <- mdy(lastDateWeHave) + 1
    lastDateWeNeed <- Sys.Date() - 1
    # For (that date until yesterday)
    for (aDate in firstDateWeNeed:lastDateWeNeed) {
      theDateString <- format(as_date(aDate), "20%y-%m-%d")
      
      if (traceThisRoutine) {
        cat(file = stderr(), prepend, "Filter for 'Date =", theDateString, "\n")
      }
      # Filter that date data out of vacc timeline tibble
      filteredStateUpdateData <- updateDataSource %>%
        filter(Date == theDateString) %>%
        filter(!is.na(FIPS )) %>%
        arrange(FIPS) %>%
        filter(Vaccine_Type == "All") %>%
        select(FIPS, Province_State, Combined_Key,
               Doses_alloc, Doses_shipped, Doses_admin,
               Stage_One_Doses, Stage_Two_Doses)

      US_to_prepend <- summarise(filteredUpdateData,
                                 FIPS = 0, Province_State = "US", Combined_Key = "US",
                                 Doses_alloc = sum(Doses_alloc, na.rm = TRUE),
                                 Doses_shipped = sum(Doses_shipped, na.rm = TRUE),
                                 Doses_admin = sum(Doses_admin, na.rm = TRUE),
                                 Stage_One_Doses = sum(Stage_One_Doses, na.rm = TRUE),
                                 Stage_Two_Doses = sum(Stage_Two_Doses, na.rm = TRUE))
      
      # OUCH Swap in code from "developUpdateStateVaccFileFromTimeline()"
      # Add it onto state vacc data tibble, you have code
      if (traceThisRoutine) {
        cat(file = stderr(), prepend, "Append that data to the file\n")
      }
      
    }
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving developGetVaccDataByGeography\n")
  }
  
  return(list(STATE = US_State_Vaccinations_As_Filed,
              US = US_Vaccinations_As_Filed,
              UDD = updateDataSource))
}

developUpdateStateVaccFileFromTimeline <- function(oldStateTibble,
                                                   timelineTibble,
                                                   dateString,
                                                   traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered developUpdateStateVaccFileFromTimeline\n")
  }
  
  filteredUpdateData <- timelineTibble %>%
    filter(Date == theDateString) %>%
    filter(!is.na(FIPS )) %>%
    arrange(FIPS) %>%
    filter(Vaccine_Type == "All") %>%
    select(FIPS, Province_State, Combined_Key,
           Doses_alloc, Doses_shipped, Doses_admin,
           Stage_One_Doses, Stage_Two_Doses)
  
  if (traceThisRoutine) {
    bigDim <- dim(timelineTibble)
    smlDim <- dim(filteredUpdateData)
    cat(file = stderr(), myPrepend, "dim timelineTibble = ", bigDim[1], bigDim[2], "\n")
    cat(file = stderr(), myPrepend, "dim filteredUpdateData = ", smlDim[1], smlDim[2], "\n")
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving developUpdateStateVaccFileFromTimeline\n")
  }
  
  return(filteredUpdateData)
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
  myPrepend <- "TEST"
  # # dataList <- developGetVaccDataByGeography(traceThisRoutine = traceThisRoutine,
  # #                                           prepend = "TEST")
  # 
  # # Get old state tibble
  # oldStateTibble <- read_csv("./DATA/CACHE/US_State_Vacc_Test.csv", #"./DATA/US_State_Vaccinations.csv",
  #                            col_types = vaccColTypes())
  # 
  # if (traceThisRoutine) {
  #   cat(file = stderr(), myPrepend, "returned with oldStateTibble\n")
  # }
  # 
  # # Get timeline tibble
  # timelineTibble <- read_csv(vaccTimeSeriesDataSpecs(mdy("08-28-2021"))$PATH,
  #                            col_types = vaccTimeSeriesDataSpecs()$COLS)
  # 
  #   if (traceThisRoutine) {
  #     cat(file = stderr(), myPrepend, "returned with timelineTibble\n")
  #   }
  # 
  #   # date string is in old tracing output
  #   dateString <- "2021-08-11"
  #   # filteredUpdateData <- timelineTibble %>%
  #   #   filter(Date == dateString) %>%
  #   #   filter(!is.na(FIPS )) %>%
  #   #   arrange(FIPS) %>%
  #   #   filter(Vaccine_Type == "All") %>%
  #   #   select(FIPS, Province_State, Combined_Key,
  #   #          Doses_alloc, Doses_shipped, Doses_admin,
  #   #          Stage_One_Doses, Stage_Two_Doses)
  #   
  #   updatedStateTibble <- developUpdateStateVaccFileFromTimeline(oldStateTibble,
  #                                                                timelineTibble,
  #                                                                dateString,
  #                                                                traceThisRoutine = traceThisRoutine,
  #                                                                prepend = myPrepend)
  #   return(updatedStateTibble)
  #   
  # return(filteredUpdateData)
  
  # Test refactor of getVaccDataByGeography
  res0 <- getVaccDataByGeography0(traceThisRoutine = traceThisRoutine, prepend = "TEST")
  res1 <- getVaccDataByGeography(traceThisRoutine = traceThisRoutine, prepend = "TEST")
  
  return(list(R0 = res0, R1 = res1))
}

  
