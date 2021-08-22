
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
  stop("Per request!")
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

updateStateLevelSerializedDataFilesAsNecessary <- function(traceThisRoutine = FALSE) {
  if (traceThisRoutine) {
    print("Entering 'updateStateLevelSerializedDataFilesAsNecessary'")
  }

  # Check the last state level serialized data file for last date;
  if (!dataIsCurrent("./DATA/US_Testing_Rate.csv")) {
    options(show.error.messages = traceThisRoutine)
    US_Testing_Rate <- try(read_csv("./DATA/US_Testing_Rate.csv",
                                    col_types = cols(.default = col_double(),
                                                     Province_State = col_logical(),
                                                     Combined_Key = col_character())))
    options(show.error.messages = TRUE)
    # Update is required
    if (class(US_Testing_Rate)[1] == "try-error") {
      # There's no old data of the type we want.
      makeInitialSansCountyDataFrom0412()
      # Now we should be able to get some data
      options(show.error.messages = TRUE)
      US_Testing_Rate <- try(read_csv("./DATA/US_Testing_Rate.csv",
                                      col_types = cols(.default = col_double(),
                                                       Province_State = col_logical(),
                                                       Combined_Key = col_character())))
    } 
    # We have US_Testing_Rate for that type -- but it may not be up-to-date
    if (traceThisRoutine) {
      print("  We have a data file, but it may not be current.")
    }
    # Get date range needed
    colNames <- names(US_Testing_Rate)
    lastName <- colNames[length(colNames)]
    if (traceThisRoutine) {
      print(paste("  Latest data is from", lastName, sep = " "))
    }

    lastDate <- mdy(lastName)
    updateDate <- lastDate + 1
    while (updateDate < (today("EST"))) {
      columnDate <- paste(month(updateDate),
                          day(updateDate),
                          (year(updateDate) - 2000), sep="/")
      if (traceThisRoutine) {
        print(paste("  Getting data for update to", columnDate, sep = " "))
      }
      # Read or download update file for that date
      options(show.error.messages = traceThisRoutine)
      if (traceThisRoutine) {
        print(paste("  before try(read_csv(",
                    pathnameOfSansCountyUpdateDataForDate(updateDate),
                    "))", sep = ""))
      }
      updateTibble <- try(read_csv(pathnameOfSansCountyUpdateDataForDate(updateDate),
                                   col_types = cols(.default = col_double(),
                                                    Province_State = col_character(),
                                                    Country_Region = col_character(),
                                                    Last_Update = col_datetime(format = ""),
                                                    People_Hospitalized = col_logical(),
                                                    ISO3 = col_character(),
                                                    Hospitalization_Rate = col_logical())))
      if (traceThisRoutine) {
        print("  Returned from try(read_csv(...))")
      }
      options(show.error.messages = TRUE)
      if (class(updateTibble)[1] == "try-error") {
        if (traceThisRoutine) {
          print(paste("  Can't read ",
                      pathnameOfSansCountyUpdateDataForDate(updateDate),
                      sep=""))
        }
        # No local file! Let's see if we can download it...
        if (url.exists(updateSansCountyDataForDate_URL(updateDate))) {
          if (traceThisRoutine) {
            print(paste("  Remote file ",
                        updateSansCountyDataForDate_URL(updateDate),
                        " exists, will try to download", 
                        sep=""))
          }
          # Get the first file
          updateTibble <- read_csv(updateSansCountyDataForDate_URL(updateDate),
                                   col_types = cols(.default = col_double(),
                                                    Province_State = col_character(),
                                                    Country_Region = col_character(),
                                                    Last_Update = col_datetime(format = ""),
                                                    People_Hospitalized = col_logical(),
                                                    ISO3 = col_character(),
                                                    Hospitalization_Rate = col_logical()))
          write_csv(updateTibble, pathnameOfSansCountyUpdateDataForDate(updateDate))
          if (traceThisRoutine) {
            print(paste("  OK, we downloaded and wrote", pathnameOfSansCountyUpdateDataForDate(updateDate)))
          }
        } else {
          if (traceThisRoutine) {
            print(paste("Remote file ",
                        updateSansCountyDataForDate_URL(updateDate),
                        " not accessible or does not exist",
                        sep=""))
          }
        }
      }
      if ("tbl" %in% class(updateTibble)) {
        # We need two lists as in createUSNewTypeDataTimeSeries.R
        aList <- typesSansCountyData(updateDate)
        for (i in 1:length(aList$thatDateTypes)) {
          thatType <- aList$thatDateTypes[i]
          prevType <- aList$prevDateTypes[i]
          oldLocalDataPath <- paste("./DATA/US_State_", prevType, ".csv", sep = "")
          newLocalDataPath <- paste("./DATA/US_State_", thatType, ".csv", sep = "")
          newLocalUSDataPath <- paste("./DATA/US_", thatType, ".csv", sep = "")
          if (traceThisRoutine) {
            print(paste("Updating ", oldLocalDataPath, "to", newLocalDataPath, sep=""))
            print(paste("before try(read_csv(", oldLocalDataPath,
                        ".. in updateStateLevelSerializedDataFilesAsNecessary",
                        sep=""))
          }
          options(show.error.messages = traceThisRoutine)
          oldData <- try(read_csv(oldLocalDataPath,
                                  col_types = cols(.default = col_double(),
                                                   # OUCH Province_State = col_logical(),
                                                   Combined_Key = col_character())))
          if (traceThisRoutine) {
            print(paste("after try(read_csv(", localDataPath,
                        ".. in updateStateLevelSerializedDataFilesAsNecessary",
                        sep=""))
          }
          options(show.error.messages = TRUE)
          # Update is required
          if (class(oldData)[1] == "try-error") {
            print(paste("read_csv(", oldLocalDataPath,") failed", sep=""))
            print(paste("  newLocalDataPath = ", newLocalDataPath))
          }
          joinTibble <- updateTibble %>%
            mutate(Combined_Key = paste(Province_State, ", US", sep=""),
                   "{columnDate}" := .data[[thatType]],
                   .keep = "none")
          newData <- left_join(oldData, joinTibble, by="Combined_Key")

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
    print("Exiting 'updateStateLevelSerializedDataFilesAsNecessary'")
  }
}
