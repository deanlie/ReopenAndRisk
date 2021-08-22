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

VaccTimeline_URL <- function() {
  U_out <- paste("https://raw.githubusercontent.com/govex/COVID-19/",
                 "master/data_tables/vaccine_data/",
                 "us_data/time_series/",
                 "vaccine_data_us_timeline.csv",
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

updateDataForUSTimeSeriesType <- function(aType, traceThisRoutine = FALSE, prepend = "") {
    
  # Local function! Haven't seen those since Pascal!
  tryWrite <- function(aTibble, aPath) {
    w1 <- try(write_csv(aTibble, aPath))
    if (class(w1)[1] == "try-error") {
      print(paste("Write to ", aPath, " failed!", sep=""))
      print(paste("   ", attributes(w1)["condition"]))
    }
  }
  
  myPrepend <- paste(prepend, "  ", sep = "")
  options(show.error.messages = traceThisRoutine)
  
  lcType <- stri_trans_tolower(aType)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateDataForUSTimeSeriesType")
    cat(file=stderr(), myPrepend, "  rawDataURL = ", TS_URL(lcType, "US"),"\n")
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
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateDataForUSTimeSeriesType")
  }
}

updateDataFilesForUSTimeSeriesTypeIfNeeded <- function(aType) {
  desiredLatestDateSlashes <- expectedLatestUpdateDataDateSlashes(UT_UpdateHour = 5)
  
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

cleanyyyymmddVector <- function(aVector) {
  matches <- str_match(as.character(aVector), "^..(..)-(..)-(..)$")
  years <- matches[,2]
  months <- as.character(as.integer(matches[,3]))
  days <- as.character(as.integer(matches[,4]))
  paste(months, days, years, sep="/")
}

getVaccDataByGeography <- function(traceThisRoutine = FALSE, prepend = "") {
  # Local function to use in "summarise" call
  sum_nona <- function(aVector) {
    sum(aVector, na.rm = TRUE)
  }
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Entered 'getVaccDataByGeography'", "\n")
  }
  if (url.exists(VaccTimeline_URL())) {
    if (traceThisRoutine) {
      cat(file=stderr(), myPrepend, "Vacc_URL() exists", "\n")
    }
    rawData <- try(read_csv(VaccTimeline_URL(),
                            col_types = cols(Province_State = col_character(),
                                            Country_Region = col_character(),
                                            Combined_Key = col_character(),
                                            FIPS = col_character(),
                                            Date = col_character(),
                                            Vaccine_Type = col_character(),
                                            .default = col_double())))
    
    if (class(rawData)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file=stderr(), myPrepend, "read of VaccTimeline_URL() failed, returning error:", "\n")
        cat(file=stderr(), myPrepend, rawData, "\n")
      }
      dataByGeography <- rawData
    } else {
      if (traceThisRoutine) {
        cat(file=stderr(), myPrepend, "read of VaccTimeline_URL() succeeded", "\n")
      }
      
      justStateData <- rawData %>%
        filter(Vaccine_Type=="All"&
                 Date > "2021-03-17"&
                 Province_State != "Department of Defense" &
                 Province_State != "Federal Bureau of Prisons" &
                 Province_State != "Indian Health Services"&
                 Province_State != "Long Term Care (LTC) Program" &
                 Province_State != "Veterans Health Administration") %>%
        mutate(Date = cleanyyyymmddVector(Date)) %>%
        select(Province_State, Combined_Key, Date, contains("Doses")) %>%
        pivot_longer(cols=contains("Doses"), names_to="Datum", values_to="number") %>%
        mutate(Loc_Datum = paste(Province_State, Datum, sep="_"),
               .after=Datum, .keep = "all") %>%
        select(Combined_Key, Datum, Loc_Datum, Date, number) %>%
        pivot_wider(names_from = Date, values_from = number) %>%
        arrange(Datum, Combined_Key)

      
      d_Vx2 = dim(justStateData)
      endColRange = d_Vx2[2] - 1

      US_to_prepend <- justStateData %>%
        group_by(Datum) %>%
        summarise(across(3:all_of(endColRange), sum_nona)) %>%
        mutate(Combined_Key = "US", .before = Datum) %>%
        mutate(Loc_Datum = paste(Combined_Key, Datum, sep = "_"), .after = Datum)

      dataByGeography <- bind_rows(US_to_prepend, justStateData)
    }
  }
  if (traceThisRoutine) {
    dDBG = dim(dataByGeography)
    cat(file=stderr(), myPrepend, "Dims of dataByGeography:", dDBG[1], "x", dDBG[2], "\n")
    nn <- names(dataByGeography)
    cat(file=stderr(), myPrepend, "dataByGeography names[1:4] =", nn[1], nn[2], nn[3], nn[4], "\n")
    cat(file=stderr(), prepend, "Leaving 'getVaccDataByGeography'", "\n")
  }
  dataByGeography
}

gatheredVaccDataByGeography <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Entered gatheredVaccDataByGeography", "\n")
  }
  allData <- getVaccDataByGeography(traceThisRoutine, prepend = myPrepend)
  
  if (traceThisRoutine) {
    nn <- names(allData)
    cat(file=stderr(), myPrepend, "allData names[1:4] =", nn[1], nn[2], nn[3], nn[4], "\n")
    cat(file=stderr(), prepend, "Leaving gatheredVaccDataByGeography", "\n")
  }
  
  allData 
}

saveTwoVaccinationDataFiles <- function(theData, traceThisRoutine = FALSE, prepend = "") {
  # Local function! Haven't seen those since Pascal!
  myPrepend <- paste(prepend, "  ", sep = "")
  tryWrite <- function(aTibble, aPath) {
    w1 <- try(write_csv(aTibble, aPath))
    if (class(w1)[1] == "try-error") {
      print(paste("Write to ", aPath, " failed!", sep=""))
      print(paste("   ", attributes(w1)["condition"]))
    }
  }
  
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Entered saveTwoVaccinationDataFiles", "\n")
  }
  US_Data <- theData %>%
    filter(Combined_Key == "US")
  State_Data <- theData %>%
    filter(Combined_Key != "US")

  theDataDir <- "./DATA/"
  US_file_name <- paste(theDataDir, "US_Vaccinations.csv", sep = "")
  US_State_file_name <- paste(theDataDir, "US_State_Vaccinations.csv", sep = "")
  
  if (traceThisRoutine) {
    cat(file=stderr(), myPrepend, "save2VFs:    US file", US_file_name, "\n")
    cat(file=stderr(), myPrepend, "          State file", US_State_file_name, "\n")
  }
  
  tryWrite(US_Data, US_file_name)
  tryWrite(State_Data, US_State_file_name)

  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Leaving saveTwoVaccinationDataFiles", "\n")
  }
}

saveVaccinationTimeSeriesData <- function(theData, traceThisRoutine = FALSE, prepend = "") {
  # Save gathered data as ./DATA/US_Vaccinations.csv
  #                   and ./DATA/US_State_Vaccinations.csv
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Entered saveVaccinationTimeSeriesData", "\n")
  }
  saveTwoVaccinationDataFiles(theData, traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Leaving saveVaccinationTimeSeriesData", "\n")
  }
}

makeInitialVaccDataFiles <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Entered makeInitialVaccDataFiles'","\n")
  }
  BuildingData <- gatheredVaccDataByGeography(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  saveVaccinationTimeSeriesData(BuildingData, traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Leaving makeInitialVaccDataFiles","\n")
  }
}

updateDataForUSVaccTimeSeries <- function(traceThisRoutine = FALSE, prepend = "") {
  options(show.error.messages = traceThisRoutine)
  
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Entered updateDataForUSVaccTimeSeries", "\n")
  }
  
  US_data_path <- paste("./DATA/", "US_Vaccinations.csv", sep="")
  US_data <- try(read_csv(US_data_path,
                          col_types=cols(.default = col_double(),
                                         Combined_Key = col_character(),
                                         Datum = col_character(),
                                         Loc_Datum = col_character())))
  if (class(US_data)[1] == "try-error") {
    if (traceThisRoutine) {
      cat(file=stderr(), myPrepend, "./DATA/US_Vaccinations.csv not read", "\n")
    }
  } else {
    if (traceThisRoutine) {
      cat(file=stderr(), myPrepend, "./DATA/US_Vaccinations.csv was read", "\n")
    }
    US_State_data_path <- paste("./DATA/", "US_State_Vaccinations.csv", sep="")
    US_State_data <- try(read_csv(US_State_data_path,
                                  col_types=cols(.default = col_double(),
                                                 Combined_Key = col_character(),
                                                 Datum = col_character(),
                                                 Loc_Datum = col_character())))
    if (class(US_data)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file=stderr(), myPrepend, "./DATA/US_State_Vaccinations.csv not read", "\n")
      }
    } else {
      if (traceThisRoutine) {
        cat(file=stderr(), myPrepend, "./DATA/US_State_Vaccinations.csv was read", "\n")
      }
    }
    baseData <- bind_rows(US_data, US_State_data)
  }
  newData <- gatheredVaccDataByGeography(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  # OUCH! Get rid of this comment block if data is correct...
  # allData <- left_join(baseData, newData, by="Loc_Datum")
  # if (traceThisRoutine) {
  #   dAD <- dim(allData)
  #   cat(file=stderr(), myPrepend, "last date in allData:", names(allData)[dAD[2]], "\n")
  # }
  
  write_csv(newData, "./DATA/newData.csv") # OUCH for debugging
  saveVaccinationTimeSeriesData(newData, traceThisRoutine = traceThisRoutine, prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Leaving updateDataForUSVaccTimeSeries", "\n")
  }
}

updateDataFilesForUSVaccTimeSeriesIfNeeded <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Entered updateDataForUSVaccTimeSeriesIfNeeded", "\n")
  }
  desiredLatestDateSlashes <- expectedLatestUpdateDataDateSlashes(UT_UpdateHour = 13)
  US_data_path <- paste("./DATA/", "US_Vaccinations.csv", sep="")
  US_data <- try(read_csv(US_data_path,
                          col_types=cols(.default = col_double(),
                                         Combined_Key = col_character(),
                                         Datum = col_character(),
                                         Loc_Datum = col_character())))
  if (class(US_data)[1] == "try-error") {
    print("./DATA/US_Vaccinations.csv was not read")
    cat(file=stderr(), myPrepend, "./DATA/US_Vaccinations.csv was not read", "\n")
    # We couldn't read the file! Creating it gets us all the data we can find.
    makeInitialVaccDataFiles(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  } else {
    if (traceThisRoutine) {
      cat(file=stderr(), myPrepend, "./DATA/US_Vaccinations.csv was read", "\n")
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
      if (url.exists(Vacc_URL())) {
        # Better create all data files!
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
    cat(file=stderr(), prepend, "Leaving updateDataForUSVaccTimeSeriesIfNeeded", "\n")
  }
}

updateTimeSeriesDataFilesAsNecessary <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Entered updateTimeSeriesDataFilesAsNecessary","\n")
  }
  for (aType in c("Confirmed", "Deaths")) {
    updateDataFilesForUSTimeSeriesTypeIfNeeded(aType)
  }
  # OUCH Set the argument to TRUE for debugging, normally = traceThisRoutine
  updateDataFilesForUSVaccTimeSeriesIfNeeded(traceThisRoutine = TRUE, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file=stderr(), prepend, "Leaving updateTimeSeriesDataFilesAsNecessary","\n")
  }
}
