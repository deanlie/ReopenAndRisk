# Make US data directly from the JHU time series file

library(tidyverse)
library(lubridate)
library(stringi)
library(RCurl)

source("./dateFormatRoutines.R")
source("./mostRecentDataDate.R")
source("./dataIsCurrent.R")

# Get (at maximum) latest 60 columns available in data download
deriveRecentUSDataFromCountyLevelData <- function(rawData) {
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

updateDataForUSTimeSeriesType <- function(aType) {
    
  # Local function! Haven't seen those since Pascal!
  tryWrite <- function(aTibble, aPath) {
    w1 <- try(write_csv(aTibble, aPath))
    if (class(w1)[1] == "try-error") {
      print(paste("Write to ", aPath, " failed!", sep=""))
      print(paste("   ", attributes(w1)["condition"]))
    }
  }
  
  traceThisRoutine <- FALSE
  options(show.error.messages = traceThisRoutine)
  
  lcType <- stri_trans_tolower(aType)

  if (traceThisRoutine) {
    print(paste("rawDataURL = ", TS_URL(lcType, "US"), sep=""))
  }

  if (aType %in% c("Confirmed", "Deaths", "Recovered")) {  
    rawData <- read_csv(TS_URL(lcType, "US"),
                        col_types = cols(.default = col_double(),
                                         iso2 = col_character(),
                                         iso3 = col_character(),
                                         Admin2 = col_character(),
                                         Province_State = col_character(),
                                         Country_Region = col_character(),
                                         Combined_Key = col_character()))
    # ,
    # col_types = cols(.default = col_double(),
    #                  Province_State = col_logical(),
    #                  Combined_Key = col_character())
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
  options(show.error.messages = TRUE)
}

updateDataFilesForUSTimeSeriesTypeIfNeeded <- function(aType,
                                                       traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataFilesForUSTimeSeriesTypeIfNeeded\n")
    cat(file = stderr(), myPrepend, "aType is", aType, "\n")
  }

  desiredLatestDateSlashes <- expectedLatestUpdateDataDateSlashes()
  
  # If ./DATA/US_Confirmed.csv has yesterday's date as its last column name,
  #    it (and therefore presumably all the data) has been updated
  
  US_data_path <- paste("./DATA/", "US_", aType, ".csv", sep="")
  # OUCH refactor read_csv calls with error handling, if (file.exists()) instead of try
  US_data <- try(read_csv(US_data_path,
                          col_types=cols(.default = col_double(),
                                         Province_State = col_logical(),
                                         Combined_Key = col_character())))
  if (class(US_data)[1] == "try-error") {
    # We couldn't read the file! Better create all data files!
    updateDataForUSTimeSeriesType(aType)
  } else {
    # The file exists. is it up to date?
    if (names(US_data[dim(US_data)[2]]) != desiredLatestDateSlashes) {
      # It's not up to date. Is newer data available?
      if (url.exists(updateData_URL())) {
        # Better create all data files!
        updateDataForUSTimeSeriesType(aType)
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

getVaccDataByGeography <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered getVaccDataByGeography\n")
  }

  rawData <- getURLFromSpecsOrStop(vaccDailyUpdateDataSpecs(),
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
    
  justStateData <- rawData %>%
    as_tibble() %>%
    filter(!is.na(FIPS )) %>%
    arrange(FIPS) %>%
    filter(Vaccine_Type == "All") %>%
    select(FIPS, Province_State, Combined_Key,
           Doses_alloc, Doses_shipped, Doses_admin,
           Stage_One_Doses, Stage_Two_Doses)

  US_to_prepend <- summarise(justStateData,
                             FIPS = 0, Province_State = "US", Combined_Key = "US",
                             Doses_alloc = sum(Doses_alloc, na.rm = TRUE),
                             Doses_shipped = sum(Doses_shipped, na.rm = TRUE),
                             Doses_admin = sum(Doses_admin, na.rm = TRUE),
                             Stage_One_Doses = sum(Stage_One_Doses, na.rm = TRUE),
                             Stage_Two_Doses = sum(Stage_Two_Doses, na.rm = TRUE))

  dataByGeography <- bind_rows(US_to_prepend, justStateData)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving getVaccDataByGeography\n")
  }

  return(dataByGeography)
}

gatheredVaccDataByGeography <- function(traceThisRoutine = FALSE) {
  if (traceThisRoutine) {
    print("in gatheredVaccDataByGeography")
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
    print("in gatheredVaccDataByGeography (2)")
  }
  
  return(allData)  
}

saveTwoVaccinationDataFiles <- function(theData, directory, suffix,
                                        traceThisRoutine = FALSE, prepend = "") {
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
  
  US_file_name <- paste("./", directory, "/US_Vaccinations", suffix, ".csv", sep = "")
  US_State_file_name <- paste("./", directory, "/US_State_Vaccinations", suffix, ".csv", sep = "")
  
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
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered saveVaccinationTimeSeriesData\n")
  }

  saveTwoVaccinationDataFiles(theData, "DATA", "", traceThisRoutine, myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving saveVaccinationTimeSeriesData\n")
  }
}

makeInitialVaccDataFiles <- function(traceThisRoutine = FALSE) {
  if (traceThisRoutine) {
    print("In 'makeInitialVaccDataFiles'")
  }
  BuildingData <- gatheredVaccDataByGeography()
  saveVaccinationTimeSeriesData(BuildingData, traceThisRoutine)
}

updateDataForUSVaccTimeSeries <- function(traceThisRoutine = FALSE, prepend = "") {
  # OUCH more tracing here, please!!!
  
  myPrepend <- paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataForUSVaccTimeSeries\n")
  }
  
  US_data_path <- paste("./DATA/", "US_Vaccinations.csv", sep="")
  # OUCH refactor all read_csv calls out
  US_data <- try(read_csv(US_data_path,
                          col_types=cols(.default = col_double(),
                                         Combined_Key = col_character(),
                                         Datum = col_character(),
                                         Loc_Datum = col_character())))
  if (class(US_data)[1] == "try-error") {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "./DATA/US_Vaccinations.csv not read\n")
    }
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "./DATA/US_Vaccinations.csv was read\n")
    }
    US_State_data_path <- paste("./DATA/", "US_State_Vaccinations.csv", sep="")
    US_State_data <- try(read_csv(US_State_data_path,
                                  col_types=cols(.default = col_double(),
                                                 Combined_Key = col_character(),
                                                 Datum = col_character(),
                                                 Loc_Datum = col_character())))
    if (class(US_data)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "./DATA/US_State_Vaccinations.csv not read\n")
      }
    } else {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "./DATA/US_State_Vaccinations.csv was read\n")
      }
    }
    baseData <- bind_rows(US_data, US_State_data)
  }
  newData <- gatheredVaccDataByGeography() %>%
    select(-Combined_Key) %>%
    select(-Datum)
  allData <- left_join(baseData, newData, by="Loc_Datum")

  saveVaccinationTimeSeriesData(allData, traceThisRoutine, myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateDataForUSVaccTimeSeries\n")
  }
}

updateDataFilesForUSVaccTimeSeriesIfNeeded <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataFilesForUSVaccTimeSeriesIfNeeded\n")
  }

  todayDate <- today("EST")
  desiredLatestDateSlashes <- paste(month(todayDate),
                                    day(todayDate),
                                    (year(todayDate) - 2000), sep="/")
  US_data_path <- paste("./DATA/", "US_Vaccinations.csv", sep="")
  US_data <- try(read_csv(US_data_path,
                          col_types=cols(.default = col_double(),
                                         Combined_Key = col_character(),
                                         Datum = col_character(),
                                         Loc_Datum = col_character())))
  if (class(US_data)[1] == "try-error") {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "./DATA/US_Vaccinations.csv not read\n")
    }
    # We couldn't read the file! Creating it gets us all the data we can find.
    makeInitialVaccDataFiles()
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "./DATA/US_Vaccinations.csv was read\n")
    }
    # The file exists. is it up to date?
    if (names(US_data[dim(US_data)[2]]) != desiredLatestDateSlashes) {
      # It's not up to date. Is newer data available?
      if (url.exists(Vacc_URL())) {
        # OUCH refactor anything that uses URL
        # Better create all data files!
        updateDataForUSVaccTimeSeries(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
      } else {
        cat(file = stderr(), myPrepend, "Remote file", Vacc_URL(), "doesn't exist??\n")
        cat(file = stderr(), myPrepend, paste("Vaccination data for", desiredLatestDateSlashes, "is not available\n", sep = " "))
        browser()
      }
    }
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateDataFilesForUSVaccTimeSeriesIfNeeded\n")
  }
}

updateTimeSeriesDataFilesAsNecessary <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateTimeSeriesDataFilesAsNecessary\n")
  }

  for (aType in c("Confirmed", "Deaths")) {
    updateDataFilesForUSTimeSeriesTypeIfNeeded(aType)
  }

  updateDataFilesForUSVaccTimeSeriesIfNeeded(traceThisRoutine = traceThisRoutine, prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateTimeSeriesDataFilesAsNecessary\n")
  }
}
