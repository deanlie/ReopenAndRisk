# Make US data directly from the JHU time series file

library(tidyverse)
library(lubridate)
library(stringi)
library(RCurl)

source("dateFormatRoutines.R")
source("mostRecentDataDate.R")
source("dataIsCurrent.R")
source("columnUtilities.R")
source("URLFunctions.R")
source("verifyFileListLatestUpdates.R")

# Get (at maximum) latest 60 columns available in data download
deriveRecentUSDataFromCountyLevelData <- function(rawData,
                                                  traceThisRoutine = FALSE,
                                                  prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered deriveRecentUSDataFromCountyLevelData\n")
  }

  aDay <- today("EST") - 60
  aDateString <- paste(month(aDay), day(aDay), (year(aDay) - 2000), sep="/")
  # indexVector is all FALSE except TRUE for i such that names(rawData)[i] == aDateString
  indexVector <- str_detect(names(rawData), aDateString)
  # ... so names(rawData)[theIndex] == aDateString
  theIndex <- (1:length(indexVector))[indexVector]
  if (is.na(theIndex)) {
    theIndex <- 60 # as of 2020-11-22, this came out to 257
  }
  
  countyData <- rawData %>%
    as_tibble() %>%
    select(Admin2,
           Province_State,
           Combined_Key,
           all_of(theIndex):last_col())
  
  stateData <- countyData %>%
    group_by(Province_State) %>%
    summarise(across(where(is.numeric), sum), .groups="drop") %>%
    mutate(Combined_Key = paste(Province_State, ", US", sep=""), .after=Province_State)
  
  USData <- stateData %>%
    summarise(Province_State = "", Combined_Key = "US",
              across(where(is.numeric), sum), .groups="drop")
  
  list(Counties = countyData, States = stateData, US = USData)
}

updateDataForUSTimeSeriesType <- function(aType, traceThisRoutine = FALSE, prepend = "") {
    
  # Local function! Haven't seen those since Pascal!
  tryWrite <- function(aTibble, aPath) {
    w1 <- try(write_csv(aTibble, aPath))
    if (class(w1)[1] == "try-error") {
      print(paste("Write to ", aPath, " failed!", sep=""))
      print(paste("   ", attributes(w1)["condition"]))
    }
  }
  
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataFilesForUSTimeSeriesType\n")
  }
  options(show.error.messages = traceThisRoutine)
  
  lcType <- stri_trans_tolower(aType)

  if (traceThisRoutine) {
    cat(file = stderr(), paste("rawDataURL = ", TS_URL(lcType, "US"), sep=""), "\n")
  }

  if (aType %in% c("Confirmed", "Deaths", "Recovered")) {  
    rawData <- read_csv(TS_URL(lcType, "US"),
                        col_types = TSColTypes())

    threeTibbles <- deriveRecentUSDataFromCountyLevelData(rawData)

    theDataDir <- "./DATA/"
    US_data_path <- paste(theDataDir, "US_", aType, ".csv", sep="")
    US_state_data_path <- paste(theDataDir, "US_State_", aType, ".csv", sep="")
    US_county_data_path <- paste(theDataDir, "US_County_", aType, ".csv", sep="")
    
    if (file.access(theDataDir, mode = 2) == 0) {
      tryWrite(threeTibbles$US, US_data_path)
      tryWrite(threeTibbles$States, US_state_data_path)
      tryWrite(threeTibbles$Counties, US_county_data_path)
    } else {
      print(paste("No write access to ", theDataDir, sep=""))
    }
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateDataFilesForUSTimeSeriesType\n")
  }
  options(show.error.messages = TRUE)
}

updateDataFilesForUSTimeSeriesTypeIfNeeded <- function(aType,
                                                       traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataFilesForUSTimeSeriesTypeIfNeeded\n")
    cat(file = stderr(), myPrepend, "aType is", aType, "\n")
  }

  desiredLatestDateSlashes <- expectedLatestUpdateDataDateSlashes()
  
  # If ./DATA/US_Confirmed.csv has yesterday's date as its last column name,
  #    it (and therefore presumably all the data) has been updated
  
  US_data_path <- paste("./DATA/", "US_", aType, ".csv", sep="")
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "US_data_path was", US_data_path, "\n")
  }
  # OUCH refactor read_csv calls with error handling, if (file.exists()) instead of try
  US_data <- try(read_csv(US_data_path,
                          col_types = myTSColTypes()))
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "class of return from read_csv was",
        class(US_data)[1], "\n") 
  }
  if (class(US_data)[1] == "try-error") {
    # We couldn't read the file! Better create all data files!
    updateDataForUSTimeSeriesType(aType, traceThisRoutine = traceThisRoutine,
                                  prepend = myPrepend)
  } else {
    # The file exists. is it up to date?
    if (names(US_data[dim(US_data)[2]]) != desiredLatestDateSlashes) {
      # It's not up to date. Is newer data available?
      if (url.exists(updateData_URL())) {
        # Better create all data files!
        updateDataForUSTimeSeriesType(aType, traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      } else {
        cat(file = stderr(), myPrepend,
            paste("Time series data for", desiredLatestDateSlashes, "is not available\n", sep = " "))
      }
    }
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateDataFilesForUSTimeSeriesTypeIfNeeded\n")
  }
}

# START Refactor this:
updateDataFilesForUSTimeSeriesTypeIfNeededB <- function(staticDataQ,
                                                        traceThisRoutine = FALSE,
                                                        prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataFilesForUSTimeSeriesTypeIfNeededB\n")
  }

  filesToUpdate <- c("US_Confirmed.csv",
                     "US_State_Confirmed.csv",
                     "US_County_Confirmed.csv",
                     "US_Deaths.csv",
                     "US_State_Deaths.csv",
                     "US_County_Deaths.csv",
                     "US_Vaccinations.csv",
                     "US_State_Vaccinations.csv")
  
  if(staticDataQ) {
    desiredLatestDateSlashes <- "11/24/21"
  } else {
    desiredLatestDateSlashes <- expectedLatestUpdateDataDateSlashes()
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "desiredLatestDateSlashes =", desiredLatestDateSlashes, "\n")
  }

  # If ./DATA/US_Confirmed.csv has yesterday's date as its last column name,
  #    it (and therefore presumably all the data) has been updated
  
  for (aType in c("Confirmed", "Deaths")) {
    # US_data_path <- paste("./DATA/", "US_", aType, ".csv", sep="")
    # if (traceThisRoutine) {
    #   cat(file = stderr(), myPrepend, "US_data_path was", US_data_path, "\n")
    # }
    # # OUCH refactor read_csv calls with error handling, if (file.exists()) instead of try
    # US_data <- try(read_csv(US_data_path,
    #                         col_types = myTSColTypes()))
    # if (traceThisRoutine) {
    #   cat(file = stderr(), myPrepend, "class of return from read_csv was",
    #       class(US_data)[1], "\n") 
    # }
    # if (class(US_data)[1] == "try-error") {
    #   # We couldn't read the file! Better create all data files!
    #   updateDataForUSTimeSeriesType(aType, traceThisRoutine = traceThisRoutine,
    #                                 prepend = myPrepend)
    # } else {
    #   # The file exists. is it up to date?
    #   if (names(US_data[dim(US_data)[2]]) != desiredLatestDateSlashes) {
    #     # It's not up to date. Is newer data available?
    #     if (url.exists(updateData_URL())) {
    #       # Better create all data files!
    #       updateDataForUSTimeSeriesType(aType, traceThisRoutine = traceThisRoutine,
    #                                     prepend = myPrepend)
    #     } else {
    #       cat(file = stderr(), myPrepend,
    #           paste("Time series data for", desiredLatestDateSlashes, "is not available\n", sep = " "))
    #     }
    #   }
    # }
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateDataFilesForUSTimeSeriesTypeIfNeededB\n")
  }
}

# Get vaccination data for one day from EITHER
# the big TS file OR the daily update file;
# Make US data as summary of state data
allGeogVaccDataFromOneDay <- function(rawData,
                                      traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered allGeogVaccDataFromOneDay\n")
  }

  justStateData <- rawData %>%
  as_tibble() %>%
  filter(!is.na(FIPS )) %>%
  arrange(FIPS) %>%
  filter(Vaccine_Type == "All") %>%
  select(FIPS, Province_State, Combined_Key,
         Doses_alloc, Doses_shipped, Doses_admin,
         Stage_One_Doses, Stage_Two_Doses)
  
  if(traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Back from justStateData pipeline\n")
  }

  US_to_prepend <- summarise(justStateData,
                             FIPS = 0, Province_State = "US", Combined_Key = "US",
                             Doses_alloc = sum(Doses_alloc, na.rm = TRUE),
                             Doses_shipped = sum(Doses_shipped, na.rm = TRUE),
                             Doses_admin = sum(Doses_admin, na.rm = TRUE),
                             Stage_One_Doses = sum(Stage_One_Doses, na.rm = TRUE),
                             Stage_Two_Doses = sum(Stage_Two_Doses, na.rm = TRUE))
  
  dataByGeography <- bind_rows(US_to_prepend, justStateData)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving allGeogVaccDataFromOneDay\n")
  }
  
  return(dataByGeography)
}

getVaccDataByGeography <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered getVaccDataByGeography\n")
  }
  
  rawData <- getURLFromSpecsOrStop(vaccDailyUpdateDataSpecs(),
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Back from getURLFromSpecsOrStop\n")
    cat(file = stderr(), myPrepend, "dim rawData =", dim(rawData)[1], dim(rawData)[2], "\n")
  }
  
  preparedData <- allGeogVaccDataFromOneDay(rawData,
                                            traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving getVaccDataByGeography\n")
  }
  
  return(preparedData)
}

gatheredVaccDataByGeography <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered gatheredVaccDataByGeography\n")
  }

  theDate <- today("EST")
  columnDate <- paste(month(theDate),
                      day(theDate),
                      (year(theDate) - 2000), sep="/")
  allData <- getVaccDataByGeography(traceThisRoutine) %>%
    gather("Datum", "latest", 4:8) %>%
    mutate(Combined_Key = Combined_Key,
           Datum = Datum,
           Loc_Datum = paste(Province_State, Datum, sep="_"),
           "{columnDate}" := latest,
           .keep = "none")
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving gatheredVaccDataByGeography\n")
  }
  
  return(allData)  
}

saveTwoVaccinationDataFiles <- function(theData, traceThisRoutine = FALSE, prepend = "") {
  # Local function! Haven't seen those since Pascal!
  tryWrite <- function(aTibble, aPath) {
    w1 <- try(write_csv(aTibble, aPath))
    if (class(w1)[1] == "try-error") {
      print(paste("Write to ", aPath, " failed!", sep=""))
      print(paste("   ", attributes(w1)["condition"]))
    }
  }
  
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered saveTwoVaccinationDataFiles\n")
  }

  US_Data <- theData %>%
    filter(Combined_Key == "US")
  State_Data <- theData %>%
    filter(Combined_Key != "US")

  US_file_name <- "./DATA/US_Vaccinations.csv"
  US_State_file_name <- "./DATA/US_State_Vaccinations.csv"
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "US file   ", US_file_name, "\n")
    cat(file = stderr(), myPrepend, "State file", US_State_file_name, "\n")
  }
  
  tryWrite(US_Data, US_file_name)
  tryWrite(State_Data, US_State_file_name)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving saveTwoVaccinationDataFiles\n")
  }
}

saveVaccinationTimeSeriesData <- function(theData, traceThisRoutine = FALSE, prepend = "") {
  # Save gathered data as ./DATA/US_Vaccinations.csv
  #                   and ./DATA/US_State_Vaccinations.csv
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered saveVaccinationTimeSeriesData\n")
  }
  saveTwoVaccinationDataFiles(theData, traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving saveVaccinationTimeSeriesData\n")
  }
}

makeInitialVaccDataFiles <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Entered makeInitialVaccDataFiles'","\n")
  }
  BuildingData <- gatheredVaccDataByGeography(traceThisRoutine = traceThisRoutine,
                                              prepend = myPrepend)
  saveVaccinationTimeSeriesData(BuildingData, traceThisRoutine = traceThisRoutine,
                                prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Leaving makeInitialVaccDataFiles","\n")
  }
}

updateDataForUSVaccTimeSeries <- function(traceThisRoutine = FALSE,
                                          prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataForUSVaccTimeSeries\n")
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
  
  lastDateWeNeed <- expectedLatestUpdateDataDate(UT_UpdateHour = 13)
  
  if (lastDateInVaccFiles >= lastDateWeNeed) { # It won't be greater
    # vacc data is up to date
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Up to date!\n")
    }
    updateDataSource = NULL
  } else {
    # I thought I could save some trouble by using the daily vaccination data URL,
    #  but it is updated multiple times a day and has empty data before all states
    #  have reported. To get a full days data, I have to use the big data timeline URL.

    updateDataSource <- getDataFromSpecs(vaccTimeSeriesDataSpecs(),
                                         traceThisRoutine = FALSE,
                                         prepend = myPrepend)
    
    #   Get latest date of state vacc data file
    lastDateWeHave = lastStateDataDateString
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "To be clear, last date we have is",
          lastDateWeHave, "\n")
    }
    
    firstDateWeNeed <- mdy(lastDateWeHave) + 1

    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Dates needed:",
          format(firstDateWeNeed, "20%y-%m-%d"), "-", format(lastDateWeNeed), "\n")
    }
    
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
                                                   traceThisRoutine = traceThisRoutine,
                                                   prepend = myPrepend)

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
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Writing updated files!\n")
  }
  write_csv(buildUSData, "./DATA/US_Vaccinations.csv")
  write_csv(buildStateData, "./DATA/US_State_Vaccinations.csv")
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateDataForUSVaccTimeSeries\n")
  }
}

updateDataFilesForUSVaccTimeSeriesIfNeeded <- function(staticDataQ = FALSE,
                                                       traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataFilesForUSVaccTimeSeriesIfNeeded\n")
  }
  if(staticDataQ) {
    desiredLatestDateSlashes <- "11/24/21"
    dataDir <- "./DATA/STATIC/"
  } else {
    desiredLatestDateSlashes <- expectedLatestUpdateDataDateSlashes(UT_UpdateHour = 13)
    dataDir <- "./DATA/"
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "desiredLatestDateSlashes =", desiredLatestDateSlashes, "\n")
  }
  US_data_path <- paste(dataDir, "US_Vaccinations.csv", sep="")
  
  US_data <- try(read_csv(US_data_path,
                          col_types = vaccColTypes()))
  if (class(US_data)[1] == "try-error") {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "./DATA/US_Vaccinations.csv not read\n")
    }
    # We couldn't read the file! Creating it gets us all the data we can find.
    # OUCH pass staticDataQ  and desiredLatestDateSlashes here
    makeInitialVaccDataFiles(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "./DATA/US_Vaccinations.csv was read\n")
    }
    # The file exists. is it up to date?
    if (names(US_data)[dim(US_data)[2]] == desiredLatestDateSlashes) {
      if (traceThisRoutine) {
        cat(file=stderr(), myPrepend, "US_Vaccinations.csv is up to date", "\n")
      }
    } else {
      if (traceThisRoutine) {
        cat(file=stderr(), myPrepend, "last date in US_Vaccinations.csv:",
            names(US_data)[dim(US_data)[2]], "\n")
        cat(file=stderr(), myPrepend, "desiredLatestDateSlashes:", desiredLatestDateSlashes, "\n")
      }
      # It's not up to date. Is newer data available?
      if (url.exists(Vacc_TS_URL()) && url.exists(peopleVacc_URL())) {
        # Better create all data files!
        # OUCH pass staticDataQ  and desiredLatestDateSlashes here
        updateDataForUSVaccTimeSeries(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
      } else {
        if (traceThisRoutine) {
          cat(file=stderr(), myPrepend, "Vaccination data for",
              desiredLatestDateSlashes, "is not available", "\n")
        }
      }
    }
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateDataFilesForUSVaccTimeSeriesIfNeeded\n")
  }
}

# Refactor this:

updateDataFilesForUSVaccTimeSeriesIfNeededB <- function(staticDataQ,
                                                        updateDataTibble,
                                                        traceThisRoutine = FALSE,
                                                        prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataFilesForUSVaccTimeSeriesIfNeededB\n")
  }

  if(staticDataQ) {
    desiredLatestDateSlashes <- "11/24/21"
    desiredLatestDate <- mdy("11/24/21")
    dataDir <- "./DATA/STATIC/"
  } else {
    desiredLatestDateSlashes <- expectedLatestUpdateDataDateSlashes(UT_UpdateHour = 13)
    desiredLatestDate <- expectedLatestUpdateDataDate(UT_UpdateHour = 13)
    dataDir <- "./DATA/"
  }

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "desiredLatestDateSlashes =", desiredLatestDateSlashes, "\n")
  }
  US_data_path <- paste(dataDir, "US_Vaccinations.csv", sep="")
  US_data <- try(read_csv(US_data_path,
                          col_types = vaccColTypes()))
  if (class(US_data)[1] == "try-error") {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, US_data_path, "not read\n")
    }
    # We couldn't read the file! Creating it gets us all the data we can find.
    makeInitialVaccDataFiles(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, US_data_path, "was read\n")
    }
    # The file exists. is it up to date?
    if (names(US_data)[dim(US_data)[2]] == desiredLatestDateSlashes) {
      if (traceThisRoutine) {
        cat(file=stderr(), myPrepend, US_data_path, "is up to date", "\n")
      }
    } else {
      if (traceThisRoutine) {
        cat(file=stderr(), myPrepend, "last date in", US_data_path,
            names(US_data)[dim(US_data)[2]], "\n")
        cat(file=stderr(), myPrepend, "desiredLatestDateSlashes:", desiredLatestDateSlashes, "\n")
      }
      updateDataForUSVaccTimeSeries(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
    }
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateDataFilesForUSVaccTimeSeriesIfNeededB\n")
  }
}

updateTimeSeriesDataFilesAsNecessary <- function(staticDataQ = FALSE,
                                                 traceThisRoutine = FALSE,
                                                 prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")

  # Key point for tracing vaccination data update!
  # Set the "traceThisRoutine" to TRUE for debugging

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateTimeSeriesDataFilesAsNecessary\n")
  }
  
  # OUCH do that date filtering stuff here, load 
  confirmedAndDeathsFiles <- c("US_Confirmed.csv",
                               "US_State_Confirmed.csv",
                               "US_County_Confirmed.csv",
                               "US_Deaths.csv",
                               "US_State_Deaths.csv",
                               "US_County_Deaths.csv")

  if(staticDataQ) {
    desiredLatestDate <- mdy("11/24/21")
    dataDir <- "./DATA/STATIC/"
  } else {
    desiredLatestDate <- expectedLatestUpdateDataDate()
    dataDir <- "./DATA/"
  }
  
  mismatches <- verifyFileListLatestUpdates(confirmedAndDeathsFiles,
                                            desiredLatestDate,
                                            dataDir,
                                            traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)
  
  # OUCH
  while (dim(mismatches)[1] > 0) {
    firstDate <- mismatches$lastUpdate[1] + 1
    firstGroup <- filter(mismatches, lastUpdate <= {{firstDate}})
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, dim(firstGroup)[1],
          "files only updated to", firstDate, "\n")
    }
    remainingMismatches <- filter(mismatches, lastUpdate >= {{firstDate}})
    if (dim(remainingMismatches)[1] > 0) {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, dim(remainingMismatches)[1],
            "files remaining\n")
      }
      nextDate <- remainingMismatches$lastUpdate[1]
    } else {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "... that's all.\n")
      }
      nextDate <- desiredLatestDate
    }
    for (aDate in (firstDate:nextDate)) {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Updating to", aDate, "\n")
      }    
    }
    mismatches <- remainingMismatches
  }

  for (aType in c("Confirmed", "Deaths")) {
    updateDataFilesForUSTimeSeriesTypeIfNeeded(aType, traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateTimeSeriesDataFilesAsNecessary\n")
  }
}
