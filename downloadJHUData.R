#*  (checked 2021-08-19; was )
#* 
#************************************************************* 
# https://raw.githubusercontent.com/
# govex/COVID-19/master/
# data_tables/vaccine_data/us_data/time_series/vaccine_data_us_timeline.csv

#************************************************************* 
#* US People vacinated time series data. PVacc_TS_URL()
# https://raw.githubusercontent.com/
# govex/COVID-19/master/
# data_tables/vaccine_data/us_data/time_series/people_vaccinated_us_timeline.csv

library(tidyverse)
library(RCurl)

source("dateFormatRoutines.R")
source("URLFunctions.R")
source("pathnameFunctions.R")
source("columnUtilities.R")

Vacc_TS_Path <- function() {
  P_out <- paste("DATA/VaccUpdate_",
                 jhuFileDateString(Sys.Date()),
                 sep = "")
}

# OUCH default tracing to TRUE for development work
tryToReadURL <- function(aURL, col_types,
                         traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered tryToReadURL\n")
    cat(file = stderr(), myPrepend, "Reading ", aURL, "\n")
  }
  
  if (url.exists(aURL)) {
    rawData <- try(read_csv(aURL,
                            col_types = col_types))
    if (class(rawData)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "try(read_csv()) failed for ", aURL, "\n")
      }
      if (traceThisRoutine) {
        cat(file = stderr(), prepend, "Early return from tryToReadURL\n")
      }
      return(simpleError(paste("Unable to read_csv:", aURL)))
    } 
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), prepend, "Early return from tryToReadURL\n")
    }
    return(simpleError(paste("No such URL:", aURL)))
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving tryToReadURL\n")
  }
  
  return(rawData)
}

# OUCH default tracing to TRUE for development work
getURLOrStop <- function(aURL, col_types, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered getURLOrStop\n")
    cat(file = stderr(), myPrepend, "Trying ", aURL, "\n")
  }
  rawData <- tryToReadURL(aURL, col_types, traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  if ("error" %in% class(rawData)) {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Error returned from tryToReadURL\n")
    }
    stop(conditionMessage(rawData))
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving getURLOrStop\n")
  }
  return(rawData)
}

# OUCH default tracing to TRUE for development work
getURLFromSpecsOrStop <- function(theSpecs, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered getURLFromSpecsOrStop\n")
    cat(file = stderr(), myPrepend, "Trying ", aURL, "\n")
  }
  
  theData <- getURLOrStop(theSpecs$URL, theSpecs$COLS,
                          traceThisRoutine = traceThisRoutine,
                          prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving getURLFromSpecsOrStop\n")
  }
  
  return(theData)
}

getFileFromSpecsOrStop <- function(theSpecs, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered getFileFromSpecsOrStop\n")
  }
  
  if (file.exists(theSpecs$PATH)) {
    rawData <- try(read_csv(theSpecs$PATH,
                            col_types = theSpecs$COLS))
    if (class(rawData)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file = stderr(), "try(read_csv()) failed for ", theSpecs$PATH, "\n")
      }
      stop(paste("FATAL ERROR -- Unable to read:", theSpecs$PATH))
    } 
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), "file.exists returned FALSE for ", theSpecs$PATH, "\n")
    }
    stop(paste("FATAL ERROR -- No such path: ", theSpecs$PATH))
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving getFileFromSpecsOrStop\n")
  }
  
  return(rawData)
}

getDataFromSpecs <- function(theSpecs,
                             traceThisRoutine = FALSE,
                             prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered getDataFromSpecs\n")
  }
  
  if (traceThisRoutine) {
    # cat(file = stderr(), myPrepend, "\n")    
  }
  if (file.exists(theSpecs$PATH)) {
    rawData <- try(read_csv(theSpecs$PATH,
                            col_types = theSpecs$COLS))
    if (class(rawData)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "try(read_csv()) failed for ", theSpecs$PATH, "\n")
      }
      stop(paste("FATAL ERROR -- Unable to read:", theSpecs$PATH))
    } 
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), "will try to download", theSpecs$URL, "\n")
    }
    rawData <- getURLFromSpecsOrStop(theSpecs,
                                     traceThisRoutine = traceThisRoutine,
                                     prepend = myPrepend)
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving getDataFromSpecs\n")
  }
  return(rawData)
}

vaccDailyUpdateDataSpecs <- function(aDate = NULL) {
  if (is.null(aDate)) {
    aDate = Sys.Date()
  }
  list(URL = Vacc_URL(),
       COLS = govexVaccColTypes(),
       PATH = paste("DATA/VaccUpdate_",
                    jhuFileDateString(aDate),
                    ".csv",
                    sep = ""))
}

vaccTimeSeriesDataSpecs <- function(aDate = NULL) {
  if (is.null(aDate)) {
    aDate = Sys.Date()
  }
  list(URL = Vacc_TS_URL(),
       COLS = govexVaccColTypes(),
       PATH = paste("DATA/VaccTS_",
                    jhuFileDateString(aDate),
                    ".csv",
                    sep = ""))
}

#* US People vacinated time series data.
pVaccTimeSeriesDataSpecs <- function(aDate = NULL) {
  if (is.null(aDate)) {
    aDate = Sys.Date()
  }
  list(URL = PVacc_TS_URL(),
       COLS = govexVaccColTypes(),
       PATH = paste("DATA/pVaccUpdate_",
                    jhuFileDateString(aDate),
                    sep = ""))
}

# saveJHUHereForDate <- function(aDate) {
#   paste("F:/DeanDocuments/COVID_DATA/JHU/",
#         jhuFileDateString(aDate), ".csv",
#         sep = "")
# }

downloadStateLevelUpdateData <- function(aDate,
                                         traceThisRoutine = FALSE,
                                         prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered downloadStateLevelUpdateData\n")
  }
  
  updateTibble <- getURLOrStop(updateStateLevelDataForDate_URL(aDate),
                               col_types = dataFileColTypes())

  updateTibble <- updateTibble %>%
    filter(!str_detect(Province_State, "Princess"))

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend,
        "Downloaded", pathnameOfStateLevelUpdateDataForDate(aDate), "\n")
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving downloadAndSaveStateLevelUpdateData\n")
  }
  
  return(updateTibble)
}

# OUCH default tracing to TRUE for development work
processFileForDate <- function(aDate, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered processFileForDate\n")
  }
  # Download the data
   
  desiredURL <- JHUDailyStateDataURLForDate(aDate)

  # Get the data
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Reading ", desiredURL, "\n")
  }

  fullDaysData <- read_csv(desiredURL,
                           col_types = dailyJHUFileColTypes())
  
  # Select and filter to get only desired data
  requiredData <- fullDaysData %>%
    select(Combined_Key, FIPS,
           Admin2, Province_State, Country_Region,
           Confirmed, Deaths, Incident_Rate, Case_Fatality_Ratio) %>%
    filter(!str_detect(Combined_Key, "Princess")) %>%
    filter(!str_detect(Combined_Key, "Unassigned")) %>%
    filter(str_detect(Combined_Key, "US"))
  
  # Save the desired data
  savePath <- saveJHUHereForDate(aDate)
  
  write_csv(requiredData, savePath)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "OK, we downloaded", desiredURL, "\n")
    cat(file = stderr(), myPrepend, "  and wrote", savePath, "\n")
    cat(file = stderr(), prepend, "Leaving processFileForDate\n")
  }
}
 
# firstDate <- as.Date("2021-04-01")
# processFileForDate(firstDate)
