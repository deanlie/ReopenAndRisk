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

developGetVaccDataByGeography <- function(writeResults = FALSE,
                                          traceThisRoutine = FALSE,
                                          prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered developGetVaccDataByGeography\n")
  }
  
  # Get US vacc data tibble
  # Get State vacc data tibble
  # See loadUSVaccinationData.R  
  US_Vaccinations_As_Filed <- read_csv("./DATA/US_Vaccinations.csv",
                                       col_types = vaccColTypes())
  
  US_State_Vaccinations_As_Filed <- read_csv("./DATA/US_State_Vaccinations.csv",
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
    updateDataSource <- getDataFromSpecsMaybeSave(vaccTimeSeriesDataSpecs(),
                                                  traceThisRoutine = traceThisRoutine,
                                                  prepend = myPrepend)

    #   Get latest date of state vacc data file
    lastDateWeHave = lastStateDataDateString
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "To be clear, last date we have is",
          lastDateWeHave, "\n")
    }
    
    firstDateWeNeed <- mdy(lastDateWeHave) + 1
    lastDateWeNeed <- firstDateWeNeed + 1 # OUCH Sys.Date() - 1
    
    buildUSData <- US_Vaccinations_As_Filed
    buildStateData <- US_State_Vaccinations_As_Filed

    # For (that date until yesterday)
    for (aDateInt in firstDateWeNeed:lastDateWeNeed) {
      aDate <- as_date(aDateInt)
      theDateString <- format(aDate, "20%y-%m-%d")
      
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Filter for 'Date =", theDateString, "\n")
      }
      # Filter that date data out of vacc timeline tibble
      filteredStateUpdateData <- updateDataSource %>%
        filter(Date == theDateString)

      dataByGeography <- allGeogVaccDataFromOneDay(filteredStateUpdateData,
                                traceThisRoutine = traceThisRoutine, prepend = myPrepend)

      columnDate <- paste(month(aDate),
                          day(aDate),
                          (year(aDate) - 2000), sep="/")
      
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "columnDate is", columnDate, "\n")
      }

      # Gather, then join on combined_key to both
      # US and state files.
      allData <- dataByGeography %>%
        gather("Datum", "latest", 4:8) %>%
        mutate(Combined_Key = Combined_Key,
               Datum = Datum,
               Loc_Datum = paste(Province_State, Datum, sep="_"),
               "{columnDate}" := latest,
               .keep = "none")
      
      newData <- allData %>%
        select(-Combined_Key) %>%
        select(-Datum)

      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Will append that data to the file\n")
      }

      buildUSData <- left_join(buildUSData, newData, by="Loc_Datum")
      buildStateData <- left_join(buildStateData, newData, by="Loc_Datum")

      if (traceThisRoutine) {
        lnUS <- length(names(buildUSData))
        cat(file = stderr(), myPrepend, "Last column (there are", lnUS, ") of US data is now",
            names(buildUSData)[lnUS],"\n")
        lnState <- length(names(buildStateData))
        cat(file = stderr(), myPrepend, "Last column (there are", lnState, ") of State data is now",
            names(buildStateData)[lnState],"\n")
      }
    }
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving developGetVaccDataByGeography\n")
  }

  if (writeResults) {
    if (traceThisRoutine) {
      cat(file = stderr(), prepend, "Writing updated files!\n")
    }
    write_csv(buildUSData, "./DATA/US_Vaccinations.csv")
    write_csv(buildStateData, "./DATA/US_State_Vaccinations.csv")
  }

  return(list(STATE = buildStateData,
              US = buildUSData,
              OLD_STATE = US_State_Vaccinations_As_Filed,
              OLD_US = US_Vaccinations_As_Filed,
              FUDD = updateDataSource))
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

setupTestFiles <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered setupTestFiles\n")
  }
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_V_Test.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_S_V_Test.csv")

  US_Res <- discardDataOutsideDateRangeFromAFile("./DATA/CACHE/US_V_Test.csv",
                                                 vaccColTypes(),
                                                 mdy("08-01-2021"),
                                                 mdy("08-10-2021"),
                                                 "CACHE/US_V_Test.csv",
                                                 traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend)
  # US_Res$T0 = originalData, US_Res$T1 = truncatedTibble))
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "\n")    
  }
  State_Res <- discardDataOutsideDateRangeFromAFile("./DATA/CACHE/US_S_V_Test.csv",
                                                    vaccColTypes(),
                                                    mdy("08-01-2021"),
                                                    mdy("08-10-2021"),
                                                    "CACHE/US_S_V_Test.csv",
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
  # US_Res$T0 = originalData, US_Res$T1 = truncatedTibble))
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving ROUTINE_NAME\n")
  }
}

testSuite <- function(traceThisRoutine = FALSE) {
  myPrepend <- "TEST"

  # Test refactor of getVaccDataByGeography
  # set up test
  # system("cp ./DATA/CACHE/US_S_V_Test.csv ./DATA/US_State_Vaccinations.csv")
  # system("cp ./DATA/CACHE/US_V_Test.csv ./DATA/US_Vaccinations.csv")

  dataList <- developGetVaccDataByGeography(writeResults = TRUE,
                                            traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)
 
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

  return(dataList)
}

  
