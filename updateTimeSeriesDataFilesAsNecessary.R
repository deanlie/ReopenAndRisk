# Make US data directly from the JHU time series file

library(tidyverse)
library(lubridate)
library(stringi)
library(RCurl)

source("./dateFormatRoutines.R")
source("./mostRecentDataDate.R")
source("./dataIsCurrent.R")

# Synthesize the URL of the Johns Hopkins time series data file
# Arguments: type = {"confirmed", "deaths"}, locale = {"US", "global"}
TS_URL <- function(type, locale) {
  U_out <- paste(JHU_repository(),
                 "csse_covid_19_time_series/",
                 "time_series_covid19_",
                 type, "_", locale, ".csv",
                 sep = "")
}

Vacc_URL <- function() {
  U_out <- paste("https://raw.githubusercontent.com/govex/COVID-19/",
                 "master/data_tables/vaccine_data/",
                 "us_data/hourly/vaccine_data_us.csv",
                 sep = "")
}

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
    cat(file=stderr(),"rawDataURL = ", TS_URL(lcType, "US"),"\n")
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

updateDataFilesForUSTimeSeriesTypeIfNeeded <- function(aType) {
  desiredLatestDateSlashes <- expectedLatestUpdateDataDateSlashes()
  
  # If ./DATA/US_Confirmed.csv has yesterday's date as its last column name,
  #    it (and therefore presumably all the data) has been updated
  
  US_data_path <- paste("./DATA/", "US_", aType, ".csv", sep="")
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
        print(paste("Time series data for", desiredLatestDateSlashes, "is not available", sep = " "))
      }
    }
  }
}

getVaccDataByGeography <- function(traceThisRoutine = FALSE) {
  if (traceThisRoutine) {
    cat(file=stderr(), "Entered 'getVaccDataByGeography'", "\n")
  }
  if (url.exists(Vacc_URL())) {
    if (traceThisRoutine) {
      cat(file=stderr(), "   Vacc_URL() exists", "\n")
    }
    rawData <- try(read_csv(Vacc_URL(),
                            col_types = cols(.default = col_double(),
                                             Province_State = col_character(),
                                             Country_Region = col_character(),
                                             Date = col_date(format = ""),
                                             Vaccine_Type = col_character(),
                                             Combined_Key = col_character())))
    
    if (class(rawData)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file=stderr(), "   read of Vacc_URL() failed, returning error:", "\n")
        cat(file=stderr(), rawData, "\n")
      }
      dataByGeography <- rawData
    } else {
      if (traceThisRoutine) {
        cat(file=stderr(), "   read of Vacc_URL() succeeded", "\n")
      }
      
      justStateData <- rawData %>%
        as_tibble() %>%
        filter(!is.na(FIPS )) %>%
        arrange(FIPS) %>%
        filter(Vaccine_Type == "All") %>%
        select(FIPS, Province_State, Combined_Key, Date,
               Doses_alloc, Doses_shipped, Doses_admin,
               Stage_One_Doses, Stage_Two_Doses)
      
      US_to_prepend <- summarise(justStateData,
                                 FIPS = 0,
                                 Province_State = "US",
                                 Combined_Key = "US",
                                 Date = first(Date),
                                 Doses_alloc = sum(Doses_alloc, na.rm = TRUE),
                                 Doses_shipped = sum(Doses_shipped, na.rm = TRUE),
                                 Doses_admin = sum(Doses_admin, na.rm = TRUE),
                                 Stage_One_Doses = sum(Stage_One_Doses, na.rm = TRUE),
                                 Stage_Two_Doses = sum(Stage_Two_Doses, na.rm = TRUE))
      
      dataByGeography <- bind_rows(US_to_prepend, justStateData)
    }
  }
  if (traceThisRoutine) {
    dDBG = dim(dataByGeography)
    cat(file=stderr(), "   Dims of dataByGeography:", dDBG[1], "x", dDBG[2], "\n")
    cat(file=stderr(), "    dataByGeography$Doses_admin[1]:", dataByGeography$Doses_admin[1], "\n")
    cat(file=stderr(), "    dataByGeography$Province_State[2]:", dataByGeography$Province_State[2], "\n")
    cat(file=stderr(), "    dataByGeography$Doses_admin[2]:", dataByGeography$Doses_admin[2], "\n")
    cat(file=stderr(), "    US_to_prepend$Province_State[1]:", US_to_prepend$Province_State[1], "\n")
    cat(file=stderr(), "    US_to_prepend$Doses_admin[1]:", US_to_prepend$Doses_admin[1], "\n")
    cat(file=stderr(), "Leaving 'getVaccDataByGeography'", "\n")
  }
  dataByGeography
}

gatheredVaccDataByGeography <- function(traceThisRoutine = FALSE) {
  if (traceThisRoutine) {
    cat(file=stderr(), "entered gatheredVaccDataByGeography", "\n")
  }
  theDate <- today("EST")
  columnDate <- paste(month(theDate),
                      day(theDate),
                      (year(theDate) - 2000), sep="/")
  if (traceThisRoutine) {
    cat(file=stderr(), "columnDate =", columnDate, "\n")
  }
  allData <- getVaccDataByGeography(traceThisRoutine) %>%
    gather("Datum", "latest", 5:9) %>%
    mutate(Combined_Key = Combined_Key,
           Datum = Datum,
           Loc_Datum = paste(Province_State, Datum, sep="_"),
           "{columnDate}" := latest,
           .keep = "none")
  
  if (traceThisRoutine) {
    cat(file=stderr(), "leaving gatheredVaccDataByGeography", "\n")
  }
  
  allData 
}

saveTwoVaccinationDataFiles <- function(theData, traceThisRoutine = FALSE) {
  # Local function! Haven't seen those since Pascal!
  tryWrite <- function(aTibble, aPath) {
    w1 <- try(write_csv(aTibble, aPath))
    if (class(w1)[1] == "try-error") {
      print(paste("Write to ", aPath, " failed!", sep=""))
      print(paste("   ", attributes(w1)["condition"]))
    }
  }
  
  if (traceThisRoutine) {
    cat(file=stderr(), "In 'saveTwoVaccinationDataFiles'", "\n")
  }
  US_Data <- theData %>%
    filter(Combined_Key == "US")
  State_Data <- theData %>%
    filter(Combined_Key != "US")

  theDataDir <- "./DATA/"
  US_file_name <- paste(theDataDir, "US_Vaccinations.csv", sep = "")
  US_State_file_name <- paste(theDataDir, "US_State_Vaccinations.csv", sep = "")
  
  if (traceThisRoutine) {
    cat(file=stderr(), "save2VFs:    US file", US_file_name, "\n")
    cat(file=stderr(), "          State file", US_State_file_name, "\n")
  }
  
  tryWrite(US_Data, US_file_name)
  tryWrite(State_Data, US_State_file_name)
}

saveVaccinationTimeSeriesData <- function(theData, traceThisRoutine = FALSE) {
  # Save gathered data as ./DATA/US_Vaccinations.csv
  #                   and ./DATA/US_State_Vaccinations.csv
  if (traceThisRoutine) {
    cat(file=stderr(), "In 'saveVaccinationTimeSeriesData'", "\n")
  }
  saveTwoVaccinationDataFiles(theData, traceThisRoutine)
}

makeInitialVaccDataFiles <- function(traceThisRoutine = FALSE) {
  if (traceThisRoutine) {
    cat(file=stderr(), "In 'makeInitialVaccDataFiles'","\n")
  }
  BuildingData <- gatheredVaccDataByGeography()
  saveVaccinationTimeSeriesData(BuildingData, traceThisRoutine)
}

updateDataForUSVaccTimeSeries <- function(traceThisRoutine = FALSE) {
  options(show.error.messages = traceThisRoutine)
  
  if (traceThisRoutine) {
    cat(file=stderr(), "Entered 'updateDataForUSVaccTimeSeries'", "\n")
  }
  
  US_data_path <- paste("./DATA/", "US_Vaccinations.csv", sep="")
  US_data <- try(read_csv(US_data_path,
                          col_types=cols(.default = col_double(),
                                         Combined_Key = col_character(),
                                         Datum = col_character(),
                                         Loc_Datum = col_character())))
  if (class(US_data)[1] == "try-error") {
    if (traceThisRoutine) {
      cat(file=stderr(), "./DATA/US_Vaccinations.csv not read", "\n")
    }
  } else {
    if (traceThisRoutine) {
      cat(file=stderr(), "./DATA/US_Vaccinations.csv was read", "\n")
    }
    US_State_data_path <- paste("./DATA/", "US_State_Vaccinations.csv", sep="")
    US_State_data <- try(read_csv(US_State_data_path,
                                  col_types=cols(.default = col_double(),
                                                 Combined_Key = col_character(),
                                                 Datum = col_character(),
                                                 Loc_Datum = col_character())))
    if (class(US_data)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file=stderr(), "./DATA/US_State_Vaccinations.csv not read", "\n")
      }
    } else {
      if (traceThisRoutine) {
        cat(file=stderr(), "./DATA/US_State_Vaccinations.csv was read", "\n")
      }
    }
    baseData <- bind_rows(US_data, US_State_data)
  }
  newData <- gatheredVaccDataByGeography(traceThisRoutine = traceThisRoutine) %>%
    select(-Combined_Key) %>%
    select(-Datum)
  allData <- left_join(baseData, newData, by="Loc_Datum")
  if (traceThisRoutine) {
    dAD <- dim(allData)
    cat(file=stderr(), "last date in allData:", names(allData)[dAD[2]], "\n")
  }
  saveVaccinationTimeSeriesData(allData, traceThisRoutine)

  if (traceThisRoutine) {
    cat(file=stderr(), "Leaving 'updateDataForUSVaccTimeSeries'", "\n")
  }
}

updateDataFilesForUSVaccTimeSeriesIfNeeded <- function(traceThisRoutine = FALSE) {
  if (traceThisRoutine) {
    cat(file=stderr(), "In 'updateDataForUSVaccTimeSeriesIfNeeded'", "\n")
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
    print("./DATA/US_Vaccinations.csv was not read")
    cat(file=stderr(), "./DATA/US_Vaccinations.csv was not read", "\n")
    # We couldn't read the file! Creating it gets us all the data we can find.
    makeInitialVaccDataFiles(traceThisRoutine = traceThisRoutine)
  } else {
    if (traceThisRoutine) {
      cat(file=stderr(), "./DATA/US_Vaccinations.csv was read", "\n")
    }
    # The file exists. is it up to date?
    if (names(US_data)[dim(US_data)[2]] == desiredLatestDateSlashes) {
      if (traceThisRoutine) {
        cat(file=stderr(), "US_Vaccinations.csv is up to date", "\n")
      }
    } else {
      if (traceThisRoutine) {
        cat(file=stderr(), "last date in US_Vaccinations.csv:",
            names(US_data)[dim(US_data)[2]], "\n")
        cat(file=stderr(), "desiredLatestDateSlashes:", desiredLatestDateSlashes, "\n")
      }
      # It's not up to date. Is newer data available?
      if (url.exists(Vacc_URL())) {
        # Better create all data files!
        updateDataForUSVaccTimeSeries(traceThisRoutine = traceThisRoutine)
      } else {
        if (traceThisRoutine) {
          cat(file=stderr(), "Vaccination data for",
              desiredLatestDateSlashes, "is not available", "\n")
        }
      }
    }
  }
}

updateTimeSeriesDataFilesAsNecessary <- function(traceThisRoutine = FALSE) {
  for (aType in c("Confirmed", "Deaths")) {
    updateDataFilesForUSTimeSeriesTypeIfNeeded(aType)
  }
  # OUCH Set the argument to TRUE for debugging, normally = traceThisRoutine
  updateDataFilesForUSVaccTimeSeriesIfNeeded(traceThisRoutine = TRUE)
}
