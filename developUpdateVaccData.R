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

updateVaccDataFiles <- function(writeResults = FALSE,
                                traceThisRoutine = FALSE,
                                prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateVaccDataFiles\n")
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
  lastDateInVaccFiles <- mdy(stateDataColNames[length(stateDataColNames)])
  
  buildUSData <- US_Vaccinations_As_Filed
  buildStateData <- US_State_Vaccinations_As_Filed

  if (lastDateInVaccFiles == (Sys.Date() - 1)) {
    # vacc data is up to date
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Up to date!\n")
    }
    updateDataSource = NULL
  } else {
    # If the only missing data is yesterday's, only download the small data file
    if (lastDateInVaccFiles == (Sys.Date() - 2)) {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "We will look for daily update data!\n")
      }
      specsToUse <- vaccDailyUpdateDataSpecs()
    } else {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "We need the big update data timeline!\n")
      }
      specsToUse <- vaccTimeSeriesDataSpecs()
    }
    updateDataSource <- getDataFromSpecsMaybeSave(specsToUse,
                                                  traceThisRoutine = FALSE,
                                                  prepend = myPrepend)

    #   Get latest date of state vacc data file
    lastDateWeHave = lastStateDataDateString
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "To be clear, last date we have is",
          lastDateWeHave, "\n")
    }
    
    firstDateWeNeed <- mdy(lastDateWeHave) + 1
    lastDateWeNeed <- Sys.Date() - 1

    # For (that date until yesterday)
    for (aDateInt in firstDateWeNeed:lastDateWeNeed) {
      aDate <- as_date(aDateInt)
      theDateString <- format(aDate, "20%y-%m-%d")
      
      if (FALSE) {
        cat(file = stderr(), myPrepend, "Filter for 'Date =", theDateString, "\n")
      }
      # Filter that date data out of vacc timeline tibble
      filteredStateUpdateData <- updateDataSource %>%
        filter(Date == theDateString)

      dataByGeography <- allGeogVaccDataFromOneDay(filteredStateUpdateData,
                                traceThisRoutine = FALSE, prepend = myPrepend)

      columnDate <- paste(month(aDate),
                          day(aDate),
                          (year(aDate) - 2000), sep="/")
      
      if (FALSE) {
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

      if (FALSE) {
        cat(file = stderr(), myPrepend, "Will append that data to the file\n")
      }

      buildUSData <- left_join(buildUSData, newData, by="Loc_Datum")
      buildStateData <- left_join(buildStateData, newData, by="Loc_Datum")
    }
  }
  
  if (traceThisRoutine) {
    lnUS <- length(names(buildUSData))
    cat(file = stderr(), myPrepend, "Last column (there are", lnUS, ") of US data is now",
        names(buildUSData)[lnUS],"\n")
    lnState <- length(names(buildStateData))
    cat(file = stderr(), myPrepend, "Last column (there are", lnState, ") of State data is now",
        names(buildStateData)[lnState],"\n")
  }

  if (writeResults) {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Writing updated files!\n")
    }
    write_csv(buildUSData, "./DATA/US_Vaccinations.csv")
    write_csv(buildStateData, "./DATA/US_State_Vaccinations.csv")
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateVaccDataFiles\n")
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

  updateVaccDataFiles(writeResults = TRUE,
                      traceThisRoutine = traceThisRoutine,
                      prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after first updateVaccDataFiles\n")
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

  dataList <- updateVaccDataFiles(writeResults = TRUE,
                                  traceThisRoutine = traceThisRoutine,
                                  prepend = myPrepend)
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_Vaccinations_From28.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vaccinations_From28.csv")
  
  cat(file = stderr(), "  Process .._29\n")

  system("cp ./DATA/CACHE/US_Vaccinations_29.csv ./DATA/US_Vaccinations.csv")
  system("cp ./DATA/CACHE/US_State_Vaccinations_29.csv ./DATA/US_State_Vaccinations.csv")
  
  dataList <- updateVaccDataFiles(writeResults = TRUE,
                                  traceThisRoutine = traceThisRoutine,
                                  prepend = myPrepend)
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_Vaccinations_From29.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vaccinations_From29.csv")
  
  cat(file = stderr(), "  Process .._30\n")
  
  system("cp ./DATA/CACHE/US_Vaccinations_30.csv ./DATA/US_Vaccinations.csv")
  system("cp ./DATA/CACHE/US_State_Vaccinations_30.csv ./DATA/US_State_Vaccinations.csv")
  
  dataList <- updateVaccDataFiles(writeResults = TRUE,
                                  traceThisRoutine = traceThisRoutine,
                                  prepend = myPrepend)
  
  system("cp ./DATA/US_Vaccinations.csv ./DATA/CACHE/US_Vaccinations_From30.csv")
  system("cp ./DATA/US_State_Vaccinations.csv ./DATA/CACHE/US_State_Vaccinations_From30.csv")
  
  return(dataList)
}

  
