
library(tidyverse)
library(lubridate)
library(stringi)
library(RCurl)

source("./dateFormatRoutines.R")
source("./dataIsCurrent.R")
source('./variableFieldNames.R')

filenameOfSansCountyDataForDate <- function(aDate) {
  paste(jhuFileDateString(aDate), ".csv", sep="")
}

pathnameOfSansCountyUpdateDataForDate <- function(aDate) {
  paste("./DATA/",filenameOfSansCountyDataForDate(aDate), sep = "")
}

updateSansCountyDataForDate_URL <- function(aDate) {
  paste(JHU_repository(),
        "csse_covid_19_daily_reports_us/",
        filenameOfSansCountyDataForDate(aDate),
        sep = "")
}

typesSansCountyData <- function(aDate) {
  vfn0 <- variableFieldNames(aDate - 1)
  vfn1 <- variableFieldNames(aDate)
  
  newTypes <- c("Hospitalization_Rate", "Incident_Rate", "OUCH",
                "People_Hospitalized", "OUCH", "Testing_Rate")
  
  thatDateTypes <- newTypes
  prevDateTypes <- newTypes
  
  prevDateTypes[3] <- vfn0$mortality
  prevDateTypes[5] <- vfn0$numberTested
  thatDateTypes[3] <- vfn1$mortality
  thatDateTypes[5] <- vfn1$numberTested
  
  list(thatDateTypes = thatDateTypes, prevDateTypes = prevDateTypes)
}

sumIgnoreNA <- function(x) {
  sum(x, na.rm = TRUE)
}

makeInitialSansCountyDataFrom0412 <- function(traceMe = FALSE) {
  traceThisRoutine <- traceMe
  if (traceThisRoutine) {
    print("Entered 'makeInitialSansCountyDataFrom0412'")
  }
  # Download 04-12-2020_us.csv from Github if necessary
  firstDate <- as.Date("2020-04-12")

  columnDate <- paste(month(firstDate),
                      day(firstDate),
                      (year(firstDate) - 2000), sep="/")
  options(show.error.messages = traceThisRoutine)
  if (traceThisRoutine) {
    print(paste("before try(read_csv(",
                pathnameOfSansCountyUpdateDataForDate(firstDate),
                "in makeInitialSansCountyDataFrom0412"))
  }
  updateTibble <- try(read_csv(pathnameOfSansCountyUpdateDataForDate(firstDate)))
  if (traceThisRoutine) {
    print(paste("after try(read_csv(",
                pathnameOfSansCountyUpdateDataForDate(firstDate),
                "in makeInitialSansCountyDataFrom0412"))
  }
  options(show.error.messages = TRUE)
  if (class(updateTibble)[1] == "try-error") {
    if (traceThisRoutine) {
      print(paste("Can't read ", pathnameOfSansCountyUpdateDataForDate(firstDate), sep=""))
    }
    # No local file! Let's see if we can download it...
    if (url.exists(updateSansCountyDataForDate_URL(firstDate))) {
      if (traceThisRoutine) {
        print(paste("Remote file ",
                    updateSansCountyDataForDate_URL(firstDate),
                    " exists, will try to download", 
                    sep=""))
      }
      # Get the first file
      updateTibble <- read_csv(updateSansCountyDataForDate_URL(firstDate),
                               col_types = cols(.default = col_double(),
                                                Province_State = col_character(),
                                                Country_Region = col_character(),
                                                Last_Update = col_datetime(format = ""),
                                                People_Hospitalized = col_logical(),
                                                ISO3 = col_character(),
                                                Hospitalization_Rate = col_logical()))
      write_csv(updateTibble, pathnameOfSansCountyUpdateDataForDate(firstDate))
      print(paste("OK, we downloaded and wrote", pathnameOfSansCountyUpdateDataForDate(firstDate)))
    } else {
      print("Maybe JHU removed data??? url.exists says no such file as")
      print(paste('   ', updateSansCountyDataForDate_URL(firstDate)))
    }
  } else {
    if (traceThisRoutine) {
      print(paste("We were able to read",
                  pathnameOfSansCountyUpdateDataForDate(firstDate),
                  "from local disk/SSD",
                  sep = " "))
    }
  }
  aList <- typesSansCountyData(firstDate)
  for (i in 1:length(aList$thatDateTypes)) {
    aType <- aList$thatDateTypes[i]
    newTibble <- updateTibble %>%
      mutate(Combined_Key = paste(Province_State, ", US", sep=""),
             Population = as.integer(100000 * Confirmed / Incident_Rate),
             "{columnDate}" := .data[[aType]],
             .keep = "none")

    # TODO:
    # Test Puerto Rico and DC carefully in all paths of UI interaction
    
    write_csv(newTibble, paste("./DATA/US_State_", aType, ".csv", sep=""))
    
    newUSTibble <- newTibble %>%
      summarise(Province_State = "", Combined_Key = "US",
                across(matches("^Pop|^[1-9]+/"), sumIgnoreNA))

    write_csv(newUSTibble, paste("./DATA/US_", aType, ".csv", sep=""))
  }
}

updateStateLevelSerializedDataFilesAsNecessary <- function(traceThisRoutine = FALSE, prepend = "CALLER??") {
  myPrepend <- paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateStateLevelSerializedDataFilesAsNecessary\n")
  }

  # Check the last state level serialized data file for last date;
  if (!dataIsCurrent("./DATA/US_Testing_Rate.csv")) {
    options(show.error.messages = traceThisRoutine)
    US_Testing_Rate <- try(read_csv("./DATA/US_Testing_Rate.csv",
                                    col_types = justCKColTypes()))
    options(show.error.messages = TRUE)
    # Update is required
    if (class(US_Testing_Rate)[1] == "try-error") {
      # There's no old data of the type we want.
      makeInitialStateLevelData(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
      # Now we should be able to get some data
      options(show.error.messages = TRUE)
      US_Testing_Rate <- try(read_csv("./DATA/US_Testing_Rate.csv",
                                      col_types = justCKColTypes()))
    } 
    # We have US_Testing_Rate for that type -- but it may not be up-to-date
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "We have a data file, but it may not be current.\n")
    }
    # Get date range needed
    colNames <- names(US_Testing_Rate)
    lastName <- colNames[length(colNames)]
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Latest data is from", lastName, "\n")
    }

    lastDate <- mdy(lastName)
    updateDate <- lastDate + 1

    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Update date is ", lastName, "\n")
    }

    while (updateDate < (today("EST"))) {
      columnDate <- paste(month(updateDate),
                          day(updateDate),
                          (year(updateDate) - 2000), sep="/")
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Getting data for update to", columnDate, "\n")
      }
      # Read or download update file for that date
      
      if (!file.exists(pathnameOfStateLevelUpdateDataForDate(updateDate))) {
        # No local file! Let's see if we can download it...
        downloadAndSaveStateLevelUpdateData(updateDate,
                                            traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)
      }
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend,
            paste("before try(read_csv(", pathnameOfStateLevelUpdateDataForDate(updateDate), "))\n", sep = ""))
      }
      updateTibble <- try(read_csv(pathnameOfStateLevelUpdateDataForDate(updateDate),
                                   col_types = dataFileColTypes()))
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, paste("after try(read_csv(",
                                              pathnameOfStateLevelUpdateDataForDate(updateDate),
                                              "))\n", sep = ""))
      }
      if (class(updateTibble)[1] == "try-error") {
        if (traceThisRoutine) {
          cat(file = stderr(), myPrepend,
              "Can't read ", pathnameOfStateLevelUpdateDataForDate(updateDate), "\n")
        }
        cat(file = stderr(), myPrepend, "FATAL ERROR -- UPDATE DATA NOT FOUND\n")
        stop()
      } else {
        if (traceThisRoutine) {
          cat(file = stderr(), myPrepend, "We were able to read",
              pathnameOfStateLevelUpdateDataForDate(updateDate),
              "from local disk/SSD\n")
        }
      }

      if ("tbl" %in% class(updateTibble)) {
        popEstimate <- updatePopulationEstimateData(updateDate, updateTibble,
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
        
        # We need two lists as in createUSNewTypeDataTimeSeries.R
        aList <- typesStateLevelData(updateDate)
        for (i in 1:length(aList$thatDateTypes)) {
          thatType <- aList$thatDateTypes[i]
          prevType <- aList$prevDateTypes[i]
          oldLocalDataPath <- paste("./DATA/US_State_", prevType, ".csv", sep = "")
          newLocalDataPath <- paste("./DATA/US_State_", thatType, ".csv", sep = "")
          newLocalUSDataPath <- paste("./DATA/US_", thatType, ".csv", sep = "")
          if (traceThisRoutine) {
            cat(file = stderr(), myPrepend, "Updating ", oldLocalDataPath, "to", newLocalDataPath,"\n")
            cat(file = stderr(), myPrepend,
                paste("before try(read_csv(", oldLocalDataPath, ")\n", sep = ""))
          }
          options(show.error.messages = traceThisRoutine)
          oldData <- try(read_csv(oldLocalDataPath,
                                  col_types = justCKColTypes()))
          if (traceThisRoutine) {
            cat(file = stderr(), myPrepend,
                paste("after try(read_csv(", oldLocalDataPath, "))\n", sep=""))
          }
          options(show.error.messages = TRUE)
          # Update is required
          if (traceThisRoutine) {
            if (class(oldData)[1] == "try-error") {
              cat(file = stderr(), myPrepend,
                  paste("read_csv(", oldLocalDataPath,") failed\n", sep=""))
              cat(file = stderr(), myPrepend, "newLocalDataPath = ", newLocalDataPath, "\n")
            }
          }
          joinTibble <- updateTibble %>%
            mutate(Combined_Key = paste(Province_State, ", US", sep=""),
                   "{columnDate}" := .data[[thatType]],
                   .keep = "none")
          newData <- left_join(oldData, joinTibble, by="Combined_Key")
          write_csv(newData, newLocalDataPath)
        
          newUSData <- newData %>%
            summarise(Province_State = "", Combined_Key = "US",
                      across(matches("^Pop|^[1-9]+/"), sumIgnoreNA))
          write_csv(newUSData, newLocalUSDataPath)
        }
        updateDate <- updateDate + 1
      } else {
        break;
      }
    }
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateStateLevelSerializedDataFilesAsNecessary\n")
  }
}
